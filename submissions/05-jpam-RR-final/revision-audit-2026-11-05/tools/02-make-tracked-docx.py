#!/usr/bin/env python3
"""
JPAM revision audit: native Word tracked-change builder
Date: November 5th, 2026
Purpose: Write native OOXML revisions (w:ins, w:del, w:pPrChange) that turn
each clean R2 comparison input into the matching final-revision input.

  - Prose paragraphs are compared word by word. Two changed paragraphs are
    paired only when both are headings or both are body text and their text
    is similar; otherwise the old paragraph is deleted and the new one is
    inserted whole. Whole-paragraph replacement also protects the boundary
    when trailing inserted text is followed by an inserted paragraph:
    LibreOffice otherwise merges the surviving paragraphs on Reject All.
  - A table or figure (its heading, panels, images, and notes) is one exhibit.
    Exhibits are paired only when they are the same object: same kind and
    byte-identical rendered images in the same order. Any other old exhibit is
    deleted whole and the new one inserted whole. Captions of different
    objects are never compared word by word.
    Trap (Nov 2026): pairing by caption text merged old Figure 5 into new
    Figure 3 ("Figure 53. AsianMarginal ..."); identity now comes from images.
  - Old images and hyperlinks are imported under word/old/ with "old_"
    relationship ids. No other relationship type is imported.
  - Revision ids and drawing ids are unique.

The script then projects Accept All and Reject All directly on the XML and
requires each projection to equal the corresponding clean input exactly:
paragraph properties, run formatting, text, hyperlinks, and image bytes.
Unsupported content (Word tables, OMML, footnote references, bookmarks,
section breaks inside paragraphs, unknown run content) stops the run.

Usage: python3 02-make-tracked-docx.py WORK_DIR
Results -> WORK/tracked/{scope}-tracked.docx
        -> WORK/tracked/{scope}-accept-proof.docx, {scope}-reject-proof.docx
        -> WORK/tracked/{scope}-report.json
Runner: step 2 of 98-run-all.sh.
"""

import difflib
import re
from copy import deepcopy

from lxml import etree as E

import review_lib as RL
from review_lib import A, CT, NS, PIC, R, REL_HYPERLINK, REL_IMAGE, WP, w

SCOPES = ("manuscript", "appendix")
XML_SPACE = "{http://www.w3.org/XML/1998/namespace}space"
P_CHILDREN = {w("pPr"), w("r"), w("hyperlink")}
RUN_CONTENT = {w("t"), w("tab"), w("br"), w("cr"), w("drawing"),
               w("noBreakHyphen"), w("softHyphen")}
EXHIBIT_RE = re.compile(r"^(Table|Figure) ((?:[A-Z]\.)?\d+)\. ")
TOKEN_RE = re.compile(r"\w+(?:[-\u2010-\u2014]\w+)*|\s+|[^\w\s]")
PAIR_THRESHOLD = 0.36
# Parts that both clean inputs must share byte for byte; the tracked file
# keeps the revised copy, so a difference would change the rejected view.
SHARED_EXCLUDE = re.compile(r"^(?:word/document\.xml|word/_rels/.*|word/media/.*|"
                            r"docProps/.*|\[Content_Types\]\.xml|_rels/\.rels)$")
SETTINGS_BEFORE_REVISION_VIEW = {
    "writeProtection", "view", "zoom", "removePersonalInformation",
    "removeDateAndTime", "doNotDisplayPageBoundaries", "displayBackgroundShape",
    "printPostScriptOverText", "printFractionalCharacterWidth", "printFormsData",
    "embedTrueTypeFonts", "embedSystemFonts", "saveSubsetFonts", "saveFormsData",
    "mirrorMargins", "alignBordersAndEdges", "bordersDoNotSurroundHeader",
    "bordersDoNotSurroundFooter", "gutterAtTop", "hideSpellingErrors",
    "hideGrammaticalErrors", "activeWritingStyle", "proofState", "formsDesign",
    "attachedTemplate", "linkStyles", "stylePaneFormatFilter",
    "stylePaneSortMethod", "documentType", "mailMerge"}


def c14n(el):
    return E.tostring(el, method="c14n", exclusive=True).decode("utf-8")


def local(el):
    return E.QName(el).localname


# ---- Validation -------------------------------------------------------------

def validate(parts, label):
    doc = RL.xml(parts["word/document.xml"])
    body = doc.find(w("body"))
    kids = list(body)
    RL.require(kids and kids[-1].tag == w("sectPr"),
               "%s: body must end with one sectPr" % label)
    for el in kids[:-1]:
        RL.require(el.tag == w("p"), "%s: unsupported body element w:%s"
                   % (label, local(el)))
        for c in el:
            RL.require(c.tag in P_CHILDREN, "%s: unsupported paragraph child %s"
                       % (label, c.tag))
            runs = [c] if c.tag == w("r") else (list(c) if c.tag == w("hyperlink") else [])
            for r in runs:
                RL.require(r.tag == w("r"), "%s: unsupported hyperlink child %s"
                           % (label, r.tag))
                for x in r:
                    RL.require(x.tag == w("rPr") or x.tag in RUN_CONTENT,
                               "%s: unsupported run content %s" % (label, x.tag))
        ppr = el.find(w("pPr"))
        if ppr is not None:
            RL.require(ppr.find(w("sectPr")) is None,
                       "%s: section break inside a paragraph" % label)
    for bad in ("ins", "del", "pPrChange", "rPrChange", "bookmarkStart",
                "footnoteReference", "endnoteReference", "tbl"):
        RL.require(not doc.findall(".//" + w(bad)),
                   "%s: unsupported w:%s in clean input" % (label, bad))
    RL.require(not doc.xpath(".//*[local-name()='oMath']"),
               "%s: OMML math in clean input" % label)
    return doc


class Resolver:
    """Relationship-aware normal forms for one package."""

    def __init__(self, parts):
        self.parts = parts
        self.rels = RL.relationships(parts, "word/document.xml")

    def media_digest(self, rid):
        RL.require(rid in self.rels, "Unknown relationship %s" % rid)
        typ, target, external = self.rels[rid]
        RL.require(typ == REL_IMAGE and not external,
                   "Relationship %s is not an embedded image" % rid)
        return RL.sha256_bytes(self.parts[target])

    def link_key(self, h):
        if h is None:
            return ""
        attrs = sorted((k, v) for k, v in h.attrib.items() if k != "{%s}id" % R)
        rid = h.get("{%s}id" % R)
        target = ""
        if rid is not None:
            RL.require(rid in self.rels and self.rels[rid][0] == REL_HYPERLINK,
                       "Hyperlink relationship %s missing" % rid)
            target = self.rels[rid][1]
        return "LINK:%s:%r" % (target, attrs)

    def drawing_key(self, drawing):
        d = deepcopy(drawing)
        for x in d.iter("{%s}docPr" % WP, "{%s}cNvPr" % PIC):
            x.attrib.pop("id", None)
            x.attrib.pop("name", None)
        for x in d.iter():
            for k, v in list(x.attrib.items()):
                if k.startswith("{%s}" % R):
                    x.set(k, "sha256:" + self.media_digest(v))
        return c14n(d)


def is_heading(p):
    s = p.find("w:pPr/w:pStyle", NS)
    return s is not None and s.get(w("val"), "").startswith("Heading")


def is_equation(drawing):
    pr = drawing.find(".//{%s}docPr" % WP)
    return pr is not None and pr.get("descr", "").startswith("Equation: ")


def figure_drawings(p):
    return [d for d in p.iter(w("drawing")) if not is_equation(d)]


def plain_text(p):
    return "".join(p.xpath(".//w:t/text()", namespaces=NS))


def ppr_key(p):
    ppr = p.find(w("pPr"))
    if ppr is None:
        return ""
    c = deepcopy(ppr)
    for x in c.findall(w("rPr")) + c.findall(w("pPrChange")):
        c.remove(x)
    return c14n(c) if (len(c) or c.attrib) else ""


def mark_key(p):
    rpr = p.find("w:pPr/w:rPr", NS)
    if rpr is None:
        return ""
    c = deepcopy(rpr)
    for x in c.findall(w("ins")) + c.findall(w("del")):
        c.remove(x)
    return c14n(c) if (len(c) or c.attrib) else ""


# ---- Runs and tokens --------------------------------------------------------

def atomic_runs(run):
    rpr = run.find(w("rPr"))
    out = []
    for c in run:
        if c.tag == w("rPr"):
            continue
        r = E.Element(w("r"), dict(run.attrib))
        if rpr is not None:
            r.append(deepcopy(rpr))
        r.append(deepcopy(c))
        out.append(r)
    return out


def flatten(p):
    """[(run, hyperlink or None)] with one content element per run."""
    out = []
    for c in p:
        if c.tag == w("r"):
            out += [(r, None) for r in atomic_runs(c)]
        elif c.tag == w("hyperlink"):
            for r in c:
                out += [(x, c) for x in atomic_runs(r)]
    return out


def run_item(res, run, link):
    rpr = run.find(w("rPr"))
    content = [x for x in run if x.tag != w("rPr")][0]
    lk = res.link_key(link)
    rk = c14n(rpr) if rpr is not None else ""
    if content.tag == w("t"):
        return (lk, rk, "T", content.text or "")
    if content.tag == w("drawing"):
        return (lk, rk, "DRAWING", res.drawing_key(content))
    if content.tag == w("br"):
        return (lk, rk, "BR", c14n(content))
    return (lk, rk, local(content), "")


def paragraph_key(res, p):
    items = []
    for run, link in flatten(p):
        it = run_item(res, run, link)
        if items and it[2] == "T" and items[-1][2] == "T" and items[-1][:2] == it[:2]:
            items[-1] = it[:3] + (items[-1][3] + it[3],)
        else:
            items.append(it)
    items = [x for x in items if not (x[2] == "T" and x[3] == "")]
    return (ppr_key(p), mark_key(p), tuple(items))


def body_paragraphs(doc):
    return [x for x in doc.find(w("body")) if x.tag == w("p")]


def normalized_body(res, doc):
    return [paragraph_key(res, p) for p in body_paragraphs(doc)]


def sect_key(doc):
    s = deepcopy(doc.find(w("body")).find(w("sectPr")))
    for x in s.iter():
        for k in list(x.attrib):
            if k.startswith("{%s}" % R):
                del x.attrib[k]
    return c14n(s)


class Unit:
    def __init__(self, kind, paras, sig, exhibit=None, number=None, ident=None):
        self.kind = kind
        self.paras = paras
        self.sig = sig
        self.exhibit = exhibit
        self.number = number
        self.ident = ident


# ---- Comparison -------------------------------------------------------------

class Tracker:
    def __init__(self, old_parts, new_parts, cfg, label):
        self.label = label
        self.author = cfg["revision_author"]
        self.date = cfg["revision_date"]
        self.counter = 0
        self.pairing = []
        self._text = {}
        self.old_res_clean = Resolver(old_parts)
        self.new_res_clean = Resolver(new_parts)
        self.parts = dict(new_parts)
        self.old_doc = validate(old_parts, label + " (R2)")
        self.new_doc = validate(new_parts, label + " (final)")
        self.check_shared(old_parts, new_parts)
        self.import_old(old_parts)
        self.res = Resolver(self.parts)

    def check_shared(self, old_parts, new_parts):
        names = sorted(n for n in set(old_parts) | set(new_parts)
                       if not SHARED_EXCLUDE.match(n))
        diff = [n for n in names if old_parts.get(n) != new_parts.get(n)]
        RL.require(not diff, "%s: clean inputs differ in shared parts %s"
                   % (self.label, diff))
        RL.require(sect_key(self.old_doc) == sect_key(self.new_doc),
                   "%s: page setup differs between versions" % self.label)

    def import_old(self, old_parts):
        rels_name = "word/_rels/document.xml.rels"
        new_rels = RL.xml(self.parts[rels_name])
        existing = {r.get("Id") for r in new_rels}
        mapping = {}
        for r in RL.xml(old_parts[rels_name]):
            if r.get("Type") not in (REL_IMAGE, REL_HYPERLINK):
                continue
            nid = "old_" + r.get("Id")
            RL.require(nid not in existing, "%s: relationship id clash %s"
                       % (self.label, nid))
            rr = deepcopy(r)
            rr.set("Id", nid)
            if r.get("TargetMode") != "External":
                src = RL.resolve_target(rels_name, r.get("Target"))
                RL.require(src.startswith("word/") and src in old_parts,
                           "%s: missing old part %s" % (self.label, src))
                dst = "word/old/" + src[len("word/"):]
                RL.require(dst not in self.parts, "%s: part clash %s" % (self.label, dst))
                self.parts[dst] = old_parts[src]
                rr.set("Target", dst[len("word/"):])
            new_rels.append(rr)
            mapping[r.get("Id")] = nid
        # Only body paragraphs are imported. The final document keeps its own
        # section/footer references; check_shared already verifies those parts.
        for paragraph in body_paragraphs(self.old_doc):
            for el in paragraph.iter():
                for k, v in list(el.attrib.items()):
                    if k.startswith("{%s}" % R):
                        RL.require(v in mapping, "%s: R2 document uses relationship %s "
                                   "of a type outside the image/hyperlink allowlist"
                                   % (self.label, v))
                        el.set(k, mapping[v])
        self.parts[rels_name] = RL.serial(new_rels)
        ct = RL.xml(self.parts["[Content_Types].xml"])
        have = {x.get("Extension", "").lower() for x in ct if x.tag == "{%s}Default" % CT}
        for x in RL.xml(old_parts["[Content_Types].xml"]):
            if x.tag == "{%s}Default" % CT and x.get("Extension", "").lower() not in have:
                ct.append(deepcopy(x))
        self.parts["[Content_Types].xml"] = RL.serial(ct)

    # -- revision marks --
    def rev(self, kind):
        self.counter += 1
        return E.Element(w(kind), {w("id"): str(self.counter),
                                   w("author"): self.author, w("date"): self.date})

    def emit(self, p, segments):
        """segments: [(None|'ins'|'del', run, hyperlink or None)]."""
        link_src = None
        link_el = None
        rev_kind = None
        rev_el = None
        for kind, run, link in segments:
            if link is not link_src:
                link_src = link
                link_el = None
                rev_kind = rev_el = None
                if link is not None:
                    link_el = E.SubElement(p, w("hyperlink"), dict(link.attrib))
            container = p if link_el is None else link_el
            run = deepcopy(run)
            if kind is None:
                rev_kind = rev_el = None
                container.append(run)
                continue
            if kind == "del":
                for t in run.iter(w("t")):
                    t.tag = w("delText")
            if kind != rev_kind or rev_el is None or rev_el.getparent() is not container:
                rev_kind = kind
                rev_el = self.rev(kind)
                container.append(rev_el)
            rev_el.append(run)

    def tokens(self, res, p):
        out = []
        for run, link in flatten(p):
            content = [x for x in run if x.tag != w("rPr")][0]
            if content.tag == w("t"):
                for piece in TOKEN_RE.findall(content.text or ""):
                    r = deepcopy(run)
                    t = r.find(w("t"))
                    t.text = piece
                    t.set(XML_SPACE, "preserve")
                    out.append((run_item(res, r, link), r, link))
            else:
                out.append((run_item(res, run, link), run, link))
        return out

    def new_paragraph_shell(self, src):
        p = E.Element(w("p"), dict(src.attrib))
        ppr = src.find(w("pPr"))
        if ppr is not None:
            p.append(deepcopy(ppr))
        return p

    def paragraph(self, a, b):
        p = self.new_paragraph_shell(b)
        if ppr_key(a) != ppr_key(b):
            ppr = p.find(w("pPr"))
            if ppr is None:
                ppr = E.Element(w("pPr"))
                p.insert(0, ppr)
            old = E.Element(w("pPr"))
            src = a.find(w("pPr"))
            if src is not None:
                for x in src:
                    if x.tag not in (w("rPr"), w("pPrChange")):
                        old.append(deepcopy(x))
            change = self.rev("pPrChange")
            change.append(old)
            ppr.append(change)
        aa = self.tokens(self.res, a)
        bb = self.tokens(self.res, b)
        sm = difflib.SequenceMatcher(None, [x[0] for x in aa], [x[0] for x in bb],
                                     autojunk=False)
        segments = []
        for tag, i, j, k, l in sm.get_opcodes():
            if tag == "equal":
                segments += [(None, x[1], x[2]) for x in bb[k:l]]
            else:
                segments += [("del", x[1], x[2]) for x in aa[i:j]]
                segments += [("ins", x[1], x[2]) for x in bb[k:l]]
        self.emit(p, segments)
        return p

    def whole(self, src, kind):
        p = self.new_paragraph_shell(src)
        ppr = p.find(w("pPr"))
        if ppr is None:
            ppr = E.Element(w("pPr"))
            p.insert(0, ppr)
        rpr = ppr.find(w("rPr"))
        if rpr is None:
            rpr = E.Element(w("rPr"))
            ppr.append(rpr)
        rpr.insert(0, self.rev(kind))
        self.emit(p, [(kind, run, link) for run, link in flatten(src)])
        return p

    # -- units --
    def units(self, res, paras, version):
        out = []
        i = 0
        while i < len(paras):
            p = paras[i]
            m = EXHIBIT_RE.match(plain_text(p)) if is_heading(p) else None
            if not m:
                out.append(Unit("para", [p], paragraph_key(res, p)))
                i += 1
                continue
            j = i + 1
            while j < len(paras) and not is_heading(paras[j]):
                j += 1
            members = paras[i:j]
            drawings = [d for q in members for d in figure_drawings(q)]
            kind, number = m.group(1), m.group(2)
            RL.require(drawings and (kind == "Figure" or len(drawings) == 1),
                       "%s %s: %s %s has %d rendered images"
                       % (self.label, version, kind, number, len(drawings)))
            ident = tuple(res.drawing_key(d) for d in drawings)
            sig = ("EXHIBIT",) + tuple(paragraph_key(res, q) for q in members)
            out.append(Unit("exhibit", members, sig, kind, number, ident))
            i = j
        return out

    def para_similarity(self, a, b):
        if is_heading(a) != is_heading(b) or mark_key(a) != mark_key(b):
            return 0
        if figure_drawings(a) or figure_drawings(b):
            return 0

        sm = difflib.SequenceMatcher(None, self.similarity_text(a),
                                     self.similarity_text(b), autojunk=False)
        if sm.real_quick_ratio() < PAIR_THRESHOLD or sm.quick_ratio() < PAIR_THRESHOLD:
            return 0
        ratio = sm.ratio()
        return ratio if ratio >= PAIR_THRESHOLD else 0

    def similarity(self, u, v):
        if u.kind != v.kind:
            return 0
        if u.kind == "exhibit":
            return 2.0 if (u.exhibit == v.exhibit and u.ident == v.ident) else 0
        return self.para_similarity(u.paras[0], v.paras[0])

    def similarity_text(self, p):
        if p not in self._text:
            self._text[p] = "".join(x[3] if x[2] == "T" else "\ufffc"
                                    for x in paragraph_key(self.res, p)[2])
        return self._text[p]

    def log_exhibit(self, action, u=None, v=None):
        self.pairing.append({
            "action": action,
            "old": "%s %s" % (u.exhibit, u.number) if u else None,
            "new": "%s %s" % (v.exhibit, v.number) if v else None})

    def pair(self, u, v):
        if u.kind == "para":
            return [self.paragraph(u.paras[0], v.paras[0])]
        self.log_exhibit("same object, revised in place", u, v)
        return self.sequence(
            [Unit("para", [p], paragraph_key(self.res, p)) for p in u.paras],
            [Unit("para", [p], paragraph_key(self.res, p)) for p in v.paras])

    def drop(self, u, kind):
        if u.kind == "exhibit":
            self.log_exhibit("deleted whole" if kind == "del" else "inserted whole",
                             u if kind == "del" else None, u if kind == "ins" else None)
        return [self.whole(p, kind) for p in u.paras]

    def replacement(self, a, b):
        na, nb = len(a), len(b)
        score = [[0.0] * (nb + 1) for _ in range(na + 1)]
        way = {}
        for i in range(na - 1, -1, -1):
            for j in range(nb - 1, -1, -1):
                options = [(score[i + 1][j], "del"), (score[i][j + 1], "ins")]
                sim = self.similarity(a[i], b[j])
                if sim:
                    options.append((score[i + 1][j + 1] + sim, "pair"))
                value, choice = max(options, key=lambda o: (o[0], o[1] == "pair"))
                score[i][j] = value
                way[i, j] = choice
        out = []
        i = j = 0
        while i < na or j < nb:
            choice = "ins" if i == na else ("del" if j == nb else way[i, j])
            if choice == "pair":
                paired = self.pair(a[i], b[j])
                next_choice = ("ins" if i + 1 == na and j + 1 < nb
                               else way.get((i + 1, j + 1)))
                if (a[i].kind == "para" and next_choice == "ins"
                        and paired[0][-1].tag == w("ins")):
                    # Adjacent trailing and whole-paragraph insertions can
                    # swallow an unchanged paragraph break in LibreOffice.
                    # Keep both versions' paragraph marks with their text.
                    out += self.drop(a[i], "del") + self.drop(b[j], "ins")
                else:
                    out += paired
                i += 1
                j += 1
            elif choice == "del":
                out += self.drop(a[i], "del")
                i += 1
            else:
                out += self.drop(b[j], "ins")
                j += 1
        return out

    def sequence(self, a, b):
        sm = difflib.SequenceMatcher(None, [u.sig for u in a], [u.sig for u in b],
                                     autojunk=False)
        out = []
        for tag, i, j, k, l in sm.get_opcodes():
            if tag == "equal":
                for u, v in zip(a[i:j], b[k:l]):
                    if v.kind == "exhibit":
                        self.log_exhibit("unchanged", u, v)
                    out += [deepcopy(p) for p in v.paras]
            else:
                out += self.replacement(a[i:j], b[k:l])
        return out

    def build(self):
        old_units = self.units(self.res, body_paragraphs(self.old_doc), "R2")
        new_units = self.units(self.res, body_paragraphs(self.new_doc), "final")
        self.counts = {
            "old_tables": sum(u.exhibit == "Table" for u in old_units),
            "old_figures": sum(u.exhibit == "Figure" for u in old_units),
            "new_tables": sum(u.exhibit == "Table" for u in new_units),
            "new_figures": sum(u.exhibit == "Figure" for u in new_units)}
        nodes = self.sequence(old_units, new_units)
        body = self.new_doc.find(w("body"))
        sect = body.find(w("sectPr"))
        for x in list(body):
            body.remove(x)
        body.extend(nodes)
        body.append(sect)
        for n, pr in enumerate(self.new_doc.iter("{%s}docPr" % WP), 1):
            pr.set("id", str(n))
        self.parts["word/document.xml"] = RL.serial(self.new_doc)
        settings = RL.xml(self.parts["word/settings.xml"])
        for tag in ("trackRevisions", "revisionView"):
            for x in settings.findall(w(tag)):
                settings.remove(x)
        pos = 0
        for idx, x in enumerate(settings):
            if local(x) in SETTINGS_BEFORE_REVISION_VIEW:
                pos = idx + 1
        settings.insert(pos, E.Element(w("trackRevisions")))
        settings.insert(pos, E.Element(w("revisionView"), {
            w("markup"): "true", w("insDel"): "true", w("formatting"): "true"}))
        self.parts["word/settings.xml"] = RL.serial(settings)


# ---- Projection -------------------------------------------------------------

def project(doc_bytes, accept):
    root = RL.xml(doc_bytes)
    remove, keep = ("del", "ins") if accept else ("ins", "del")
    body = root.find(w("body"))
    for p in list(body.findall(w("p"))):
        rpr = p.find("w:pPr/w:rPr", NS)
        if rpr is not None:
            if rpr.find(w(remove)) is not None:
                body.remove(p)
                continue
            for m in rpr.findall(w(keep)):
                rpr.remove(m)
            if not len(rpr) and not rpr.attrib:
                rpr.getparent().remove(rpr)
        ppr = p.find(w("pPr"))
        if ppr is not None:
            change = ppr.find(w("pPrChange"))
            if change is not None:
                ppr.remove(change)
                if not accept:
                    for x in list(ppr):
                        if x.tag != w("rPr"):
                            ppr.remove(x)
                    for idx, x in enumerate(change.find(w("pPr"))):
                        ppr.insert(idx, deepcopy(x))
            if not len(ppr) and not ppr.attrib:
                p.remove(ppr)
        for e in list(p.iter(w(remove))):
            e.getparent().remove(e)
        for e in list(p.iter(w(keep))):
            parent = e.getparent()
            idx = parent.index(e)
            for ch in list(e):
                parent.insert(idx, ch)
                idx += 1
            parent.remove(e)
        for t in p.iter(w("delText")):
            t.tag = w("t")
        for h in list(p.iter(w("hyperlink"))):
            if not len(h):
                h.getparent().remove(h)
    for bad in ("ins", "del", "pPrChange", "delText"):
        RL.require(not root.findall(".//" + w(bad)),
                   "Projection left w:%s behind" % bad)
    return root


def first_mismatch(actual, expected):
    for i, (x, y) in enumerate(zip(actual, expected)):
        if x != y:
            return "paragraph %d:\n  projected %r\n  expected  %r" % (
                i + 1, str(x)[:600], str(y)[:600])
    return "paragraph counts %d vs %d" % (len(actual), len(expected))


def main():
    cfg = RL.load_config()
    work = RL.work_dir_from_argv()
    outdir = work / "tracked"
    outdir.mkdir(exist_ok=True)
    for scope in SCOPES:
        label = scope
        old_path = work / "clean" / "old" / (scope + "-clean.docx")
        new_path = work / "clean" / "new" / (scope + "-clean.docx")
        old_parts = RL.read_package(old_path)
        new_parts = RL.read_package(new_path)
        tracker = Tracker(old_parts, new_parts, cfg, label)
        tracker.build()
        out = outdir / (scope + "-tracked.docx")
        RL.write_package(out, tracker.parts)
        coverage = RL.package_integrity(tracker.parts, label + " tracked")
        projections = {}
        for accept, src_parts, src_res in ((True, new_parts, tracker.new_res_clean),
                                           (False, old_parts, tracker.old_res_clean)):
            name = "accept" if accept else "reject"
            root = project(tracker.parts["word/document.xml"], accept)
            actual = normalized_body(tracker.res, root)
            expected = normalized_body(src_res, RL.xml(src_parts["word/document.xml"]))
            RL.require(actual == expected, "%s %s projection differs from the clean "
                       "input at %s" % (label, name, first_mismatch(actual, expected)))
            proof = dict(tracker.parts)
            proof["word/document.xml"] = RL.serial(root)
            RL.write_package(outdir / ("%s-%s-proof.docx" % (scope, name)), proof)
            projections[name] = "exact"
            RL.log("%s %s: XML projection equals the clean input (%d paragraphs)"
                   % (label, name, len(actual)))
        report = {"coverage": coverage, "xml_projections": projections,
                  "exhibit_counts": tracker.counts, "exhibit_pairing": tracker.pairing,
                  "revision_marks": tracker.counter}
        RL.write_json(outdir / (scope + "-report.json"), report)
        RL.log("%s: %d insertions, %d deletions, %d paragraph-property changes"
               % (label, coverage["insertions"], coverage["deletions"],
                  coverage["paragraph_property_changes"]))
    RL.log("Step 2 complete: tracked documents in %s" % outdir)


if __name__ == "__main__":
    main()
