#!/usr/bin/env python3
"""
JPAM revision audit: clean comparison DOCX builder
Date: November 5th, 2026
Purpose: Build the clean Word comparison inputs for the R2 submission
(04-jpam-RR-v2, immutable) and the final revision (05-jpam-RR-final) from
their LaTeX sources, using one identical conversion for both versions.

  1. Snapshot each version's .tex/.bib/.png tree (including fragments/) into
     WORK/sources/{old,new}. The submission folders are never written.
  2. Compile submit_preprint.tex in each snapshot with latexmk so that table,
     figure, equation, and section numbers come from that version's AUX file.
     Undefined references or citations stop the run.
  3. Expand \\input recursively with TeX line and comment semantics, build the
     manuscript (title, abstract, keywords, main text, tables, figures) and the
     online appendix, and convert with Pandoc. Citations use citeproc with
     my_paper/csl/apa.csl.
  4. Render every table (with its notes) and every equation from the LaTeX
     source as PNG images. LibreOffice 7.5 drops rows of tracked OMML and
     Word tables, so the review format replaces them with exact renderings.
     Footnotes become numbered notes collected at the end of each document.
  5. Convert comment_audit.md to DOCX for the package.

Any construct the converter does not handle (raw LaTeX left after Pandoc,
unknown \\ref variants, missing \\input targets or images, Pandoc warnings)
stops the run with a non-zero exit code. There is no fallback output.

Usage: python3 01-build-clean-docx.py WORK_DIR   (WORK_DIR must be empty)
Results -> WORK/sources/{old,new}/                 snapshots and preprint builds
        -> WORK/sources/{old,new}/{scope}-comparison.tex
        -> WORK/render/tables/, WORK/render/math/   LaTeX renderings
        -> WORK/clean/{old,new}/{scope}-clean.docx
        -> WORK/audit/comment_audit.docx
        -> WORK/build-manifest.json
Runner: step 1 of 98-run-all.sh.
"""

import calendar
import json
import os
import re
import shutil
import subprocess
import time
from pathlib import Path

import review_lib as RL

try:
    from PIL import Image
    from docx import Document
    from docx.oxml import OxmlElement
    from docx.oxml.ns import qn
    from docx.shared import Inches, Pt, RGBColor
except ImportError as exc:  # pragma: no cover - preflight reports this too
    RL.fail("Missing Python package: %s" % exc, RL.EXIT_PACKAGE)

SCOPES = ("manuscript", "appendix")
VERSIONS = ("old", "new")
RENDER_DPI = 200


class ConversionError(Exception):
    pass


# ---- Process helpers --------------------------------------------------------

def run(cmd, cwd, label, input_text=None, env=None):
    # TeX may emit font-encoded bytes in diagnostics. Escape those bytes without
    # changing input text or hiding a nonzero process status.
    r = subprocess.run(cmd, cwd=str(cwd), input=input_text, env=env,
                       capture_output=True, text=True, encoding="utf-8",
                       errors="backslashreplace")
    if r.returncode != 0:
        RL.fail("%s failed (exit %d)\nSTDOUT tail:\n%s\nSTDERR tail:\n%s"
                % (label, r.returncode, r.stdout[-3000:], r.stderr[-3000:]))
    return r


def pandoc(args, cwd, label, input_text=None, env=None):
    r = run(["pandoc"] + args, cwd, label, input_text, env)
    warnings = [x for x in r.stderr.splitlines() if "[WARNING]" in x]
    RL.require(not warnings, "%s emitted Pandoc warnings:\n%s"
               % (label, "\n".join(warnings)))
    return r


def source_date_env(cfg):
    stamp = calendar.timegm(time.strptime(cfg["revision_date"],
                                          "%Y-%m-%dT%H:%M:%SZ"))
    env = dict(os.environ)
    env["SOURCE_DATE_EPOCH"] = str(stamp)
    return env


# ---- TeX parsing ------------------------------------------------------------

def group(s, pos, left="{", right="}"):
    while pos < len(s) and s[pos].isspace():
        pos += 1
    if pos >= len(s) or s[pos] != left:
        raise ConversionError("Expected '%s' near: %r"
                              % (left, s[max(0, pos - 60):pos + 100]))
    start = pos + 1
    depth = 1
    pos += 1
    while pos < len(s):
        c = s[pos]
        if c == "\\":
            pos += 2
            continue
        if c == left:
            depth += 1
        elif c == right:
            depth -= 1
            if depth == 0:
                return s[start:pos], pos + 1
        pos += 1
    raise ConversionError("Unbalanced group near: %r" % s[start - 1:start + 100])


def commands(s, name, n, replacement):
    """Replace \\name[opt]{a1}..{an} with replacement(a1, .., an)."""
    pat = re.compile(r"\\" + name + r"(?![A-Za-z])")
    out = []
    pos = 0
    while True:
        m = pat.search(s, pos)
        if not m:
            break
        out.append(s[pos:m.start()])
        end = m.end()
        probe = end
        while probe < len(s) and s[probe].isspace():
            probe += 1
        if probe < len(s) and s[probe] == "[":
            _, end = group(s, probe, "[", "]")
        args = []
        for _ in range(n):
            arg, end = group(s, end)
            args.append(arg)
        out.append(replacement(*args))
        pos = end
    return "".join(out) + s[pos:]


def comment_index(line):
    for i, c in enumerate(line):
        if c != "%":
            continue
        k = i - 1
        slashes = 0
        while k >= 0 and line[k] == "\\":
            slashes += 1
            k -= 1
        if slashes % 2 == 0:
            return i
    return None


def uncomment_tex(text):
    """Apply TeX's comment and end-of-line rules.

    A '%' removes the rest of the line and its end-of-line, and TeX skips
    leading blanks on the next line; an empty line is a paragraph break.
    Trap (Nov 2026): the earlier converter kept the newline after a comment,
    which turns '7%' in an \\input fragment into '7 ' and prints '7 %'.
    """
    lines = text.split("\n")
    if text.endswith("\n"):
        lines = lines[:-1]
    out = []
    after_comment = False
    for line in lines:
        if after_comment:
            line = line.lstrip(" \t")
        idx = comment_index(line)
        if idx is not None:
            out.append(line[:idx])
            after_comment = True
            continue
        after_comment = False
        if line.strip() == "":
            out.append("\n\n")
        else:
            out.append(line + "\n")
    return "".join(out)


def expand_inputs(s, root, stack, included):
    """Inline \\input{...} recursively, resolving paths like TeX (cwd)."""
    def include(name):
        name = name.strip()
        cands = [root / name] if name.endswith(".tex") else [
            root / (name + ".tex"), root / name]
        path = next((p for p in cands if p.is_file()), None)
        if path is None:
            raise ConversionError("\\input target not found: %s" % name)
        path = path.resolve()
        try:
            rel = path.relative_to(root)
        except ValueError:
            raise ConversionError("\\input outside the source tree: %s" % name)
        if path in stack:
            raise ConversionError("Recursive \\input: %s" % rel)
        included.add(rel.as_posix())
        body = expand_inputs(uncomment_tex(path.read_text(encoding="utf-8")),
                             root, stack + [path], included)
        # The end-of-line after a file's last line is a space, not a break.
        if body.endswith("\n") and not body.endswith("\n\n"):
            body = body[:-1] + " "
        return body
    return commands(s, "input", 1, include)


def auxmap(path):
    """Label -> printed number, using the last definition (as LaTeX does)."""
    labels = {}
    duplicates = set()
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if not line.startswith("\\newlabel{"):
            continue
        key, end = group(line, len("\\newlabel"))
        value, _ = group(line, end)
        number, _ = group(value, 0)
        if key in labels and labels[key] != number:
            duplicates.add(key)
        labels[key] = number
    return labels, sorted(duplicates)


LABEL_VALUE_RE = re.compile(r"^[A-Za-z0-9.()\-]+$")


def label_value(labels, key):
    if key not in labels:
        raise ConversionError("Unknown reference or label: %s" % key)
    value = labels[key]
    if not LABEL_VALUE_RE.match(value):
        raise ConversionError("Label %s has an unexpanded value %r" % (key, value))
    return value


def label_in(s):
    m = re.search(r"\\label\{([^}]+)\}", s)
    return m.group(1) if m else None


def captions(s):
    found = []

    def saver(star):
        def save(text):
            found.append((star, text))
            return ""
        return save
    s = commands(s, r"caption\*", 1, saver(True))
    s = commands(s, r"caption(?!\*)", 1, saver(False))
    return s, found


def clean_caption(c):
    return commands(c, "label", 1, lambda a: "").strip()


def unwrap(s):
    for cmd in ("specialcell", "makecell"):
        s = commands(s, cmd, 1, lambda a: a.replace("\\\\", " "))
    s = commands(s, "resizebox", 3, lambda a, b, c: c)
    # Size switches only affect PDF layout (e.g. the A.5 note font); dropping
    # them keeps every word and number.
    for cmd in ("tiny", "scriptsize", "footnotesize", "small", "normalsize"):
        s = re.sub(r"\\" + cmd + r"(?![A-Za-z])", "", s)
    s = re.sub(r"\\(?:begin|end)\{(?:threeparttable|ThreePartTable|landscape|center)\}", "", s)
    s = re.sub(r"\\begin\{adjustbox\}\{[^}]*\}|\\end\{adjustbox\}", "", s)
    s = re.sub(r"\\(?:begingroup|endgroup|centering|raggedright|selectfont|"
               r"singlespacing|doublespacing|onehalfspacing|FloatBarrier|hfill|"
               r"newpage|pagebreak|clearpage|insertTableNotes)(?![A-Za-z])", "", s)
    s = re.sub(r"\\(?:vspace|hspace)\*?\{[^}]*\}|\\setstretch\{[^}]*\}|"
               r"\\fontsize\{[^}]*\}\{[^}]*\}", "", s)
    s = commands(s, "setcounter", 2, lambda a, b: "")
    s = commands(s, r"renewcommand\*?", 2, lambda a, b: "")
    s = re.sub(r"\\begin\{tabular\}(?:\[[^]]*\])?\{>\{\}lllll\}",
               r"\\begin{tabular}{lllll}", s)
    return s


def table_notes(s):
    def fn(m):
        t = m.group(2)
        return "\n\n" + re.sub(r"\\item\[([^]]*)\]",
                               lambda k: "\n\n" + k.group(1) + ". ", t) + "\n\n"
    return re.sub(r"\\begin\{(tablenotes|TableNotes)\}(.*?)\\end\{\1\}", fn, s,
                  flags=re.S)


FIG_RE = re.compile(r"\\begin\{figure\}(?:\[[^\]]*\])?(.*?)\\end\{figure\}", re.S)
SUBFIG_RE = re.compile(r"\\begin\{subfigure\}(?:\[[^\]]*\])?\{[^}]*\}(.*?)"
                       r"\\end\{subfigure\}", re.S)
GRAPHICS_RE = re.compile(r"\\includegraphics(?:\[[^\]]*\])?\{([^}]+)\}")
TABLE_RE = re.compile(r"\\begin\{table\}(?:\[[^\]]*\])?(.*?)\\end\{table\}", re.S)
THREEPART_RE = re.compile(r"\\begin\{ThreePartTable\}(.*?)\\end\{ThreePartTable\}", re.S)


def figures(s, labels, vdir):
    def fig(m):
        t = m.group(1)
        # The figure's own label, not a panel label, numbers the exhibit.
        key = label_in(SUBFIG_RE.sub("", t))
        if key is None:
            raise ConversionError("Figure without a main \\label: %r" % t[:200])
        number = label_value(labels, key)
        panel = [0]

        def sub(sm):
            body, caps = captions(sm.group(1))
            c = " ".join(clean_caption(x) for star, x in caps if not star)
            lead = ""
            if c:
                lead = "\\textbf{(" + chr(97 + panel[0]) + ") " + c + "}\n\n"
                panel[0] += 1
            return "\n\n" + lead + body + "\n\n"
        t = SUBFIG_RE.sub(sub, t)
        t, caps = captions(t)
        main = [clean_caption(c) for star, c in caps if not star]
        if len(main) != 1:
            raise ConversionError("Figure %s has %d main captions" % (key, len(main)))
        notes = [clean_caption(c) for star, c in caps if star]
        images = GRAPHICS_RE.findall(t)
        if not images:
            raise ConversionError("Figure %s has no \\includegraphics" % key)
        for name in images:
            if not (vdir / name).is_file():
                raise ConversionError("Figure %s image not found (exact file name "
                                      "required): %s" % (key, name))
        t = GRAPHICS_RE.sub(lambda im: "\n\n\\includegraphics[width=5.8in]{"
                            + im.group(1) + "}\n\n", t)
        return ("\n\n\\section*{Figure " + number + ". " + main[0] + "}\n\n" + t
                + "\n\n" + "\n\n".join(notes) + "\n\n")
    return FIG_RE.sub(fig, s)


def tables(s, labels):
    def table(m):
        t = m.group(1)
        key = label_in(t)
        if key is None:
            raise ConversionError("Table without \\label: %r" % t[:200])
        t, caps = captions(t)
        main = [clean_caption(c) for star, c in caps if not star]
        if len(main) != 1:
            raise ConversionError("Table %s has %d main captions" % (key, len(main)))
        return ("\n\n\\section*{Table " + label_value(labels, key) + ". " + main[0]
                + "}\n\n" + table_notes(unwrap(t)) + "\n\n")
    s = TABLE_RE.sub(table, s)

    def longtable(m):
        t = m.group(1)
        nm = re.search(r"\\begin\{TableNotes\}(.*?)\\end\{TableNotes\}", t, re.S)
        nt = nm.group(0) if nm else ""
        t = t.replace(nt, "")
        key = label_in(t)
        if key is None:
            raise ConversionError("Longtable without \\label: %r" % t[:200])
        t, caps = captions(t)
        main = [clean_caption(c) for star, c in caps if not star]
        if len(main) != 1:
            raise ConversionError("Longtable %s has %d main captions" % (key, len(main)))
        t = re.sub(r"(\\begin\{longtable\}\{[^}]+\})\s*\\\\", r"\1", t)
        endfoot = re.search(r"\\endfoot(.*?)\\endlastfoot", t, re.S)
        footer = endfoot.group(1) if endfoot else ""
        t = re.sub(r"\\endfirsthead.*?\\endlastfoot", "", t, flags=re.S)
        t = t.replace("\\end{longtable}", footer + "\n\\end{longtable}")
        t = t.replace("\\begin{longtable}", "\\begin{tabular}").replace(
            "\\end{longtable}", "\\end{tabular}")
        return ("\n\n\\section*{Table " + label_value(labels, key) + ". " + main[0]
                + "}\n\n" + t + "\n\n" + table_notes(nt) + "\n\n")
    return THREEPART_RE.sub(longtable, s)


def resolve(s, labels):
    s = commands(s, "eqref", 1, lambda k: "(" + label_value(labels, k) + ")")
    s = commands(s, "ref", 1, lambda k: label_value(labels, k))

    # Visible equation numbers are part of the text; keep them explicitly.
    def eq(m):
        key = label_in(m.group(0))
        suffix = (" \\qquad (" + label_value(labels, key) + ")") if key else ""
        return "\\[" + commands(m.group(2), "label", 1, lambda a: "") + suffix + "\\]"
    s = re.sub(r"\\begin\{(equation)\}(.*?)\\end\{\1\}", eq, s, flags=re.S)

    def align(m):
        key = label_in(m.group(0))
        body = commands(m.group(2), "label", 1, lambda a: "")
        body = body.replace("\\nonumber", "").replace("&", "")
        rows = [r.strip() for r in body.split("\\\\") if r.strip()]
        if key:
            rows[-1] += " \\qquad (" + label_value(labels, key) + ")"
        return "\n\n".join("\\[" + r + "\\]" for r in rows)
    s = re.sub(r"\\begin\{(align\*?)\}(.*?)\\end\{\1\}", align, s, flags=re.S)
    # Plain signed numbers typeset in math mode become text with a true minus.
    s = re.sub(r"\$([+\-]?\d[\d.,{}]*)\$",
               lambda m: m.group(1).replace("{", "").replace("}", "").replace("-", "\u2212"),
               s)
    return commands(s, "label", 1, lambda a: "")


UNSUPPORTED = [
    (r"\\input(?![A-Za-z])", "\\input left after expansion"),
    (r"\\(?:include|subfile|import|subimport|InputIfFileExists)(?![A-Za-z])",
     "file inclusion other than \\input"),
    (r"\\begin\{verbatim\}|\\verb(?![A-Za-z])", "verbatim text"),
    (r"\\(?:autoref|cref|Cref|pageref|nameref|vref)(?![A-Za-z])|\\hyperref\[",
     "reference command other than \\ref/\\eqref"),
    (r"\\begin\{(?:table|figure|subfigure|longtable|ThreePartTable|"
     r"sidewaystable|sidewaysfigure|wrapfigure|table\*|figure\*)\}",
     "float left unconverted"),
    (r"\\caption(?![A-Za-z])", "caption outside a converted float"),
    (r"\\source\{", "\\source caption macro"),
]


def check_supported(raw, label):
    for pattern, what in UNSUPPORTED:
        m = re.search(pattern, raw)
        if m:
            raise ConversionError("%s: unsupported construct (%s) near %r"
                                  % (label, what, raw[max(0, m.start() - 80):m.start() + 120]))


def macro(defs, name):
    m = re.search(r"\\newcommand\{\\" + name + r"\}", defs)
    if not m:
        raise ConversionError("Macro \\%s not defined in manuscript_title_etc.tex" % name)
    return group(defs, m.end())[0]


def number_sections(text):
    count = [0]

    def heading(title):
        count[0] += 1
        return "\\section{" + str(count[0]) + ". " + title + "}"
    return commands(text, r"section(?!\*)", 1, heading)


def build_comparison_tex(vdir, scope, labels, included):
    root = vdir.resolve()
    defs = expand_inputs(uncomment_tex(
        (vdir / "manuscript_title_etc.tex").read_text(encoding="utf-8")),
        root, [], included)

    def expanded(name):
        included.add(name)
        return expand_inputs(uncomment_tex((vdir / name).read_text(encoding="utf-8")),
                             root, [(vdir / name).resolve()], included)
    if scope == "manuscript":
        head = ("\\section*{" + macro(defs, "PAPERTITLE") + "}\n"
                + macro(defs, "AUTHORHADAH") + "\n\n\\section*{Abstract}\n"
                + macro(defs, "PAPERABSTRACT") + "\n\n"
                + macro(defs, "PAPERKEYWORDS") + "\n\n")
        raw = (expand_inputs(head, root, [], included)
               + number_sections(expanded("manuscript_main.tex")) + "\n\n"
               + expanded("manuscript_tab_fig.tex"))
    else:
        raw = (expand_inputs("\\section*{Online appendix: " + macro(defs, "PAPERTITLE")
                             + "}\n\n", root, [], included)
               + expanded("manuscript_online_appendix.tex"))
    raw = figures(raw, labels, vdir)
    raw = tables(raw, labels)
    raw = unwrap(resolve(raw, labels))
    check_supported(raw, "%s/%s" % (vdir.name, scope))
    return raw


# ---- Pandoc AST -------------------------------------------------------------

def nodes(x, into_cite=True):
    if isinstance(x, dict):
        yield x
        if into_cite or x.get("t") != "Cite":
            for v in x.values():
                yield from nodes(v, into_cite)
    elif isinstance(x, list):
        for v in x:
            yield from nodes(v, into_cite)


def ast_text(node):
    return " ".join(n["c"] for n in nodes(node) if n.get("t") == "Str")


HEADING_ANY_RE = re.compile(r"\\(?:part|chapter|(?:sub)*section|(?:sub)?paragraph)"
                            r"\*?\s*[\[{]")
EXHIBIT_TITLE_RE = re.compile(r"^(Table|Figure) ((?:[A-Z]\.)?\d+)\. ")


def table_segments(tex):
    """Ordered (number, segment) for each '\\section*{Table N. ...}' block."""
    out = []
    for h in re.finditer(r"\\section\*\{Table ", tex):
        title, end = group(tex, h.start() + len("\\section*"))
        stop = HEADING_ANY_RE.search(tex, end)
        segment = tex[end:stop.start() if stop else len(tex)].strip()
        m = re.match(r"Table ((?:[A-Z]\.)?\d+)\. ", title)
        if not m:
            raise ConversionError("Malformed table heading: %r" % title[:120])
        out.append((m.group(1), segment))
    return out


def image_ast(path, alt):
    with Image.open(path) as im:
        wi = im.width / RENDER_DPI
        hi = im.height / RENDER_DPI
    scale = min(1.0, 7.05 / wi, 8.8 / hi)
    return {"t": "Image", "c": [["", [], [["width", "%.5fin" % (wi * scale)],
                                          ["height", "%.5fin" % (hi * scale)]]],
                                [{"t": "Str", "c": alt}], [str(path), ""]]}


def split_tables(ast, segments, label):
    """Mark Table exhibits; the blocks after each heading are replaced later."""
    blocks = []
    headers = []
    skip = False
    for b in ast["blocks"]:
        if b["t"] == "Header":
            skip = False
            m = EXHIBIT_TITLE_RE.match(ast_text(b))
            if m and m.group(1) == "Table":
                headers.append(m.group(2))
                blocks.append({"table_header": b, "number": m.group(2), "skipped": []})
                skip = True
                continue
        if skip:
            blocks[-1]["skipped"].append(b)
        else:
            blocks.append(b)
    numbers = [n for n, _ in segments]
    if headers != numbers:
        raise ConversionError("%s: table headings %s do not match LaTeX segments %s"
                              % (label, headers, numbers))
    for b in blocks:
        if "table_header" in b and not any(n.get("t") == "Table"
                                           for n in nodes(b["skipped"])):
            raise ConversionError("%s: Table %s produced no table body"
                                  % (label, b["number"]))
    return blocks


def math_in(blocks):
    for b in blocks:
        target = b["table_header"] if "table_header" in b else b
        for n in nodes(target):
            if n.get("t") == "Math":
                yield n["c"][1]


def finish_ast(ast, blocks, table_images, math_images, label):
    notelist = []

    def transform(obj):
        if isinstance(obj, list):
            return [transform(x) for x in obj]
        if not isinstance(obj, dict):
            return obj
        if obj.get("t") == "Math":
            tex = obj["c"][1]
            return image_ast(math_images[tex], "Equation: " + tex)
        if obj.get("t") == "Note":
            notelist.append(None)
            i = len(notelist)
            notelist[i - 1] = transform(obj["c"])
            return {"t": "Superscript", "c": [{"t": "Str", "c": str(i)}]}
        return {k: transform(v) for k, v in obj.items()}

    out = []
    tables_seen = 0
    for b in blocks:
        if "table_header" in b:
            out.append(transform(b["table_header"]))
            # The alt text omits the table number so a renumbered but unchanged
            # table keeps a byte-identical drawing and pairs as the same object.
            out.append({"t": "Para", "c": [image_ast(
                table_images[tables_seen],
                "Complete table and notes rendered from the LaTeX source")]})
            tables_seen += 1
        else:
            out.append(transform(b))
    if notelist:
        out.append({"t": "Header", "c": [1, ["notes", [], []], [{"t": "Str", "c": "Notes"}]]})
        for i, bs in enumerate(notelist, 1):
            if not bs or bs[0]["t"] not in ("Para", "Plain"):
                raise ConversionError("%s: note %d does not start with a paragraph"
                                      % (label, i))
            bs[0]["c"] = [{"t": "Str", "c": str(i) + "."}, {"t": "Space"}] + bs[0]["c"]
            out.extend(bs)
    for n in nodes(out):
        if n.get("t") in ("Table", "Figure", "Math", "Note"):
            raise ConversionError("%s: unconverted %s block" % (label, n["t"]))
    ast["blocks"] = out
    return ast, len(notelist)


# ---- LaTeX renderings -------------------------------------------------------

def render_pages(directory, tex, stem, count):
    directory.mkdir(parents=True, exist_ok=True)
    if count == 0:
        return []
    (directory / (stem + "s.tex")).write_text(tex, encoding="utf-8")
    run(["pdflatex", "-interaction=nonstopmode", "-halt-on-error", stem + "s.tex"],
        directory, "pdflatex %ss.tex" % stem)
    info = run(["pdfinfo", stem + "s.pdf"], directory, "pdfinfo").stdout
    pages = int(re.search(r"Pages:\s+(\d+)", info).group(1))
    RL.require(pages == count, "%ss.pdf has %d pages, expected %d" % (stem, pages, count))
    run(["pdftoppm", "-r", str(RENDER_DPI), "-png", stem + "s.pdf", stem],
        directory, "pdftoppm %ss.pdf" % stem)
    found = {}
    for p in directory.glob(stem + "-*.png"):
        m = re.fullmatch(re.escape(stem) + r"-(\d+)\.png", p.name)
        if m:
            found[int(m.group(1))] = p
    RL.require(sorted(found) == list(range(1, count + 1)),
               "%s renderings incomplete: %s" % (stem, sorted(found)))
    return [found[i] for i in range(1, count + 1)]


TABLE_PREAMBLE = (
    "\\documentclass[10pt]{article}\n"
    "\\usepackage{amsmath,amssymb,mathpazo,eulervm,booktabs,array,multirow,"
    "graphicx,setspace,adjustbox}\n"
    "\\DeclareUnicodeCharacter{2212}{\\ensuremath{-}}\n"
    "\\usepackage[active,tightpage]{preview}\n"
    "\\setlength\\PreviewBorder{2pt}\n\\begin{document}\n")
MATH_PREAMBLE = (
    "\\documentclass[11pt]{article}\n"
    "\\usepackage{amsmath,amssymb,mathpazo,eulervm}\n"
    "\\usepackage[active,tightpage]{preview}\n"
    "\\setlength\\PreviewBorder{1pt}\n\\begin{document}\n")


def render_tables(registry, directory):
    tex = TABLE_PREAMBLE
    for segment in registry:
        boxed = re.sub(r"(\\begin\{tabular\}.*?\\end\{tabular\})",
                       r"\\begin{adjustbox}{max width=\\linewidth}\1\\end{adjustbox}",
                       segment, flags=re.S)
        tex += ("\\begin{preview}\\begin{minipage}{7.1in}\\singlespacing\n"
                + boxed + "\n\\end{minipage}\\end{preview}\n")
    tex += "\\end{document}\n"
    return render_pages(directory, tex, "table", len(registry))


def render_math(registry, directory):
    tex = MATH_PREAMBLE
    for formula in registry:
        tex += "\\begin{preview}\\hbox{$\\displaystyle " + formula + "$}\\end{preview}\n"
    tex += "\\end{document}\n"
    return render_pages(directory, tex, "equation", len(registry))


# ---- Word styling -----------------------------------------------------------

def style_clean_docx(path, author, label):
    d = Document(str(path))
    for sec in d.sections:
        sec.page_width = Inches(8.5)
        sec.page_height = Inches(11)
        sec.top_margin = sec.bottom_margin = Inches(.7)
        sec.left_margin = sec.right_margin = Inches(.65)
        p = sec.footer.paragraphs[0]
        p.alignment = 1
        fld = OxmlElement("w:fldSimple")
        fld.set(qn("w:instr"), "PAGE")
        p._p.append(fld)
    for name in ("Normal", "Body Text", "First Paragraph"):
        st = d.styles[name]
        st.font.name = "Times New Roman"
        st.font.size = Pt(11)
        st.paragraph_format.line_spacing = 1.15
        st.paragraph_format.space_after = Pt(6)
    for name in ("Heading 1", "Heading 2", "Heading 3"):
        st = d.styles[name]
        st.font.name = "Times New Roman"
        st.font.size = Pt(13 if name == "Heading 1" else 11)
        st.font.color.rgb = RGBColor(0, 0, 0)
    for p in d.paragraphs:
        if p.style.name.startswith("Heading") and p.text.startswith(
                ("Table ", "Figure ", "Notes", "References")):
            p.paragraph_format.page_break_before = True
    body = d.element.body
    # Pandoc bookmarks are unused (no internal links); dropping them keeps the
    # tracked file's accept/reject projections identical to these inputs.
    for tag in ("w:bookmarkStart", "w:bookmarkEnd"):
        for el in list(body.iter(qn(tag))):
            el.getparent().remove(el)
    # Author decision Nov 2026: Pandoc's First Paragraph style is Body Text
    # with no other properties; using Body Text throughout avoids style-only
    # revisions wherever an earlier block was inserted or deleted.
    for st in body.iter(qn("w:pStyle")):
        if st.get(qn("w:val")) == "FirstParagraph":
            st.set(qn("w:val"), "BodyText")
    for tag, what in (("w:hyperlink[@w:anchor]", "internal hyperlinks"),
                      ("w:footnoteReference", "footnote references"),
                      ("w:tbl", "Word tables")):
        RL.require(not body.xpath(".//" + tag),
                   "%s: unexpected %s in clean DOCX" % (label, what))
    RL.require(not body.xpath(".//*[local-name()='oMath']"),
               "%s: unexpected OMML math in clean DOCX" % label)
    d.core_properties.author = author
    d.save(str(path))
    RL.rezip_deterministic(path)


def build_audit_docx(md, out, env):
    pandoc([str(md), "-o", str(out)], md.parent, "pandoc comment_audit.md", env=env)
    d = Document(str(out))
    for sec in d.sections:
        sec.page_width = Inches(8.5)
        sec.page_height = Inches(11)
        sec.top_margin = sec.bottom_margin = Inches(.7)
        sec.left_margin = sec.right_margin = Inches(.65)
    for s in ("Normal", "Body Text", "First Paragraph"):
        d.styles[s].font.name = "Calibri"
        d.styles[s].font.size = Pt(10)
        d.styles[s].paragraph_format.space_after = Pt(5)
    for table in d.tables:
        n = len(table.columns)
        widths = {4: [.65, 1.4, 3.5, 1.65], 3: [1.3, 3.5, 2.4],
                  2: [2.8, 4.4]}.get(n, [7.2 / n] * n)
        table.autofit = False
        for col, wid in zip(table.columns, widths):
            col.width = Inches(wid)
        for row in table.rows:
            for cell, wid in zip(row.cells, widths):
                cell.width = Inches(wid)
                for p in cell.paragraphs:
                    p.paragraph_format.space_after = Pt(4)
                    for r in p.runs:
                        r.font.size = Pt(8.5)
            row._tr.get_or_add_trPr().append(OxmlElement("w:cantSplit"))
        table.rows[0]._tr.get_or_add_trPr().append(OxmlElement("w:tblHeader"))
    d.save(str(out))
    RL.rezip_deterministic(out)


# ---- Pipeline ---------------------------------------------------------------

def snapshot(src, dest, cfg):
    manifest = RL.source_manifest(src, cfg)
    RL.require(manifest, "No source files found in %s" % src, RL.EXIT_MISSING_FILE)
    for rel in manifest:
        target = dest / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src / rel, target)
    return manifest


def compile_preprint(vdir, main, label):
    run(["latexmk", "-pdf", "-interaction=nonstopmode", "-halt-on-error", main],
        vdir, "latexmk %s (%s)" % (main, label))
    log_text = (vdir / (Path(main).stem + ".log")).read_text(encoding="utf-8",
                                                             errors="replace")
    problems = re.findall(r"^.*(?:Reference|Citation) `[^']*' on page \S+ undefined.*$"
                          r"|^.*There were undefined references.*$"
                          r"|^Package biblatex Warning: .*(?:undefined|could not be found).*$",
                          log_text, flags=re.M)
    RL.require(not problems, "%s: unresolved references or citations:\n%s"
               % (label, "\n".join(sorted(set(problems)))))
    blg = vdir / (Path(main).stem + ".blg")
    if blg.is_file():
        missing = [x for x in blg.read_text(encoding="utf-8", errors="replace").splitlines()
                   if "didn't find a database entry" in x]
        RL.require(not missing, "%s: biber missing entries:\n%s" % (label, "\n".join(missing)))


def main():
    cfg = RL.load_config()
    root = RL.repo_root()
    work = RL.work_dir_from_argv()
    RL.require(not any(work.iterdir()),
               "Work directory must be empty (fresh build): %s" % work)
    env = source_date_env(cfg)
    csl = root / cfg["csl"]
    main_tex = cfg["latex_main"]
    manifest = {"sources": {}, "included_tex": {}, "duplicate_labels": {},
                "exhibits": {}, "notes": {}, "pandoc": run(["pandoc", "--version"], work,
                                                          "pandoc").stdout.splitlines()[0]}

    asts = {}
    segments = {}
    for v in VERSIONS:
        src = root / cfg["versions"][v]["dir"]
        vdir = work / "sources" / v
        vdir.mkdir(parents=True)
        manifest["sources"][v] = {"dir": cfg["versions"][v]["dir"],
                                  "sha256": snapshot(src, vdir, cfg)}
        RL.log("Snapshot %s: %d files" % (v, len(manifest["sources"][v]["sha256"])))
        compile_preprint(vdir, main_tex, v)
        labels, dups = auxmap(vdir / (Path(main_tex).stem + ".aux"))
        manifest["duplicate_labels"][v] = dups
        if dups:
            RL.log("NOTE %s: multiply defined labels resolve to their last "
                   "definition, as in the compiled PDF: %s" % (v, dups))
        included = set()
        for scope in SCOPES:
            label = "%s/%s" % (v, scope)
            try:
                tex = build_comparison_tex(vdir, scope, labels, included)
            except ConversionError as exc:
                RL.fail("%s: %s" % (label, exc))
            texfile = vdir / (scope + "-comparison.tex")
            texfile.write_text(tex, encoding="utf-8")
            out = pandoc([texfile.name, "-f", "latex", "-t", "json"], vdir,
                         "pandoc read %s" % label)
            ast = json.loads(out.stdout)
            raws = [n["c"] for n in nodes(ast, into_cite=False)
                    if n.get("t") in ("RawBlock", "RawInline")]
            RL.require(not raws, "%s: LaTeX Pandoc could not convert: %s"
                       % (label, raws[:10]))
            try:
                segments[v, scope] = table_segments(tex)
                asts[v, scope] = (ast, split_tables(ast, segments[v, scope], label))
            except ConversionError as exc:
                RL.fail(str(exc))
        manifest["included_tex"][v] = sorted(included)

    # One shared rendering per distinct table segment and formula, so an
    # unchanged exhibit or equation yields byte-identical images in both versions.
    table_registry = []
    math_registry = []
    for v in VERSIONS:
        for scope in SCOPES:
            for _, segment in segments[v, scope]:
                if segment not in table_registry:
                    table_registry.append(segment)
            for formula in math_in(asts[v, scope][1]):
                if formula not in math_registry:
                    math_registry.append(formula)
    table_pngs = render_tables(table_registry, work / "render" / "tables")
    math_pngs = render_math(math_registry, work / "render" / "math")
    math_images = dict(zip(math_registry, math_pngs))
    RL.log("Rendered %d distinct tables and %d distinct formulas"
           % (len(table_registry), len(math_registry)))

    for v in VERSIONS:
        vdir = work / "sources" / v
        outdir = work / "clean" / v
        outdir.mkdir(parents=True)
        for scope in SCOPES:
            label = "%s/%s" % (v, scope)
            ast, blocks = asts[v, scope]
            images = [table_pngs[table_registry.index(seg)] for _, seg in segments[v, scope]]
            try:
                ast, note_count = finish_ast(ast, blocks, images, math_images, label)
            except ConversionError as exc:
                RL.fail(str(exc))
            out = outdir / (scope + "-clean.docx")
            pandoc(["-f", "json", "-t", "docx", "--citeproc",
                    "--bibliography", cfg["bibliography"], "--csl", str(csl),
                    "-M", "reference-section-title=References", "-o", str(out)],
                   vdir, "pandoc write %s" % label, json.dumps(ast), env)
            style_clean_docx(out, cfg["revision_author"], label)
            heads = [EXHIBIT_TITLE_RE.match(ast_text(b)) for b in ast["blocks"]
                     if b["t"] == "Header"]
            heads = [m.group(1) + " " + m.group(2) for m in heads if m]
            manifest["exhibits"]["%s_%s" % (v, scope)] = heads
            manifest["notes"]["%s_%s" % (v, scope)] = note_count
            RL.log("Clean %s: %d tables, %d figures, %d notes"
                   % (label, sum(h.startswith("Table") for h in heads),
                      sum(h.startswith("Figure") for h in heads), note_count))

    md = root / cfg["audit_dir"] / cfg["audit_markdown"]
    RL.require(md.is_file(), "Missing %s" % md, RL.EXIT_MISSING_FILE)
    (work / "audit").mkdir()
    build_audit_docx(md, work / "audit" / "comment_audit.docx", env)
    manifest["audit_markdown_sha256"] = RL.sha256_file(md)
    manifest["csl_sha256"] = RL.sha256_file(csl)
    RL.write_json(work / "build-manifest.json", manifest)
    RL.log("Step 1 complete: clean comparison inputs in %s" % (work / "clean"))


if __name__ == "__main__":
    main()
