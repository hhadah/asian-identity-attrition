"""
JPAM revision audit: shared helpers for the review-package tools
Date: November 5th, 2026
Purpose: Repository and work-directory resolution, configuration, deterministic
DOCX packaging, OOXML integrity checks, and application-level DOCX semantics.
Imported by 01-build-clean-docx.py, 02-make-tracked-docx.py, and
04-verify-and-publish.py. 03-office-verify.py runs under LibreOffice's bundled
Python (no lxml or Pillow) and therefore does not import this module.
Every check fails the run; nothing warns and continues.
Requires Python >= 3.8 with lxml, Pillow, and python-docx.
"""

import hashlib
import io
import json
import os
import posixpath
import re
import sys
import unicodedata
import zipfile
from pathlib import Path

from lxml import etree as E

TOOLS_DIR = Path(__file__).resolve().parent
CONFIG_PATH = TOOLS_DIR / "review-config.json"

# Exit codes follow the repository convention.
EXIT_MISSING_ENV = 198
EXIT_CONTRACT = 498
EXIT_PACKAGE = 499
EXIT_MISSING_FILE = 601

W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
M = "http://schemas.openxmlformats.org/officeDocument/2006/math"
A = "http://schemas.openxmlformats.org/drawingml/2006/main"
WP = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
PIC = "http://schemas.openxmlformats.org/drawingml/2006/picture"
REL = "http://schemas.openxmlformats.org/package/2006/relationships"
CT = "http://schemas.openxmlformats.org/package/2006/content-types"
NS = {"w": W, "r": R, "m": M, "a": A, "wp": WP, "pic": PIC}
REL_IMAGE = R + "/image"
REL_HYPERLINK = R + "/hyperlink"

FIXED_ZIP_TIME = (1980, 1, 1, 0, 0, 0)


def w(tag):
    return "{%s}%s" % (W, tag)


def fail(message, code=EXIT_CONTRACT):
    print("ERROR: " + message, file=sys.stderr, flush=True)
    sys.exit(code)


def require(condition, message, code=EXIT_CONTRACT):
    if not condition:
        fail(message, code)


def log(message):
    print(message, flush=True)


# ---- Paths and configuration -------------------------------------------------

def repo_root():
    # JPAM_REPO_ROOT overrides; otherwise the tools directory sits at
    # <repo>/submissions/05-jpam-RR-final/revision-audit-2026-11-05/tools.
    env = os.environ.get("JPAM_REPO_ROOT", "")
    root = Path(env).resolve() if env else TOOLS_DIR.parents[3]
    require(root.is_dir(), "Repository root not found: %s" % root,
            EXIT_MISSING_FILE)
    return root


def load_config():
    require(CONFIG_PATH.is_file(), "Missing %s" % CONFIG_PATH, EXIT_MISSING_FILE)
    cfg = json.loads(CONFIG_PATH.read_text(encoding="utf-8"))
    root = repo_root()
    for key in ("old", "new"):
        d = root / cfg["versions"][key]["dir"]
        require(d.is_dir(), "Missing source directory %s" % d, EXIT_MISSING_FILE)
    require((root / cfg["csl"]).is_file(), "Missing CSL %s" % cfg["csl"],
            EXIT_MISSING_FILE)
    return cfg


def work_dir_from_argv(must_exist=True):
    require(len(sys.argv) >= 2, "Usage: %s WORK_DIR" % Path(sys.argv[0]).name,
            EXIT_MISSING_ENV)
    work = Path(sys.argv[1]).resolve()
    if must_exist:
        require(work.is_dir(), "Work directory not found: %s" % work,
                EXIT_MISSING_FILE)
    return work


def sha256_bytes(data):
    return hashlib.sha256(data).hexdigest()


def sha256_file(path):
    return sha256_bytes(Path(path).read_bytes())


def read_json(path):
    require(Path(path).is_file(), "Missing %s" % path, EXIT_MISSING_FILE)
    return json.loads(Path(path).read_text(encoding="utf-8"))


def write_json(path, data):
    Path(path).write_text(json.dumps(data, indent=2, sort_keys=False) + "\n",
                          encoding="utf-8")


def source_manifest(src, cfg):
    """Relative path -> sha256 for every snapshotted source file."""
    suffixes = set(cfg["snapshot_suffixes"])
    excluded_dirs = set(cfg["snapshot_exclude_dirs"])
    excluded_files = set(cfg["snapshot_exclude_files"])
    out = {}
    for dirpath, dirnames, filenames in os.walk(src):
        dirnames[:] = sorted(d for d in dirnames
                             if d not in excluded_dirs and not d.startswith("."))
        for name in sorted(filenames):
            p = Path(dirpath) / name
            rel = p.relative_to(src).as_posix()
            if p.suffix not in suffixes or name in excluded_files:
                continue
            if name.startswith("."):
                continue
            out[rel] = sha256_file(p)
    return dict(sorted(out.items()))


# ---- DOCX packages ----------------------------------------------------------

def read_package(path):
    require(Path(path).is_file(), "Missing package %s" % path, EXIT_MISSING_FILE)
    with zipfile.ZipFile(path) as z:
        return {name: z.read(name) for name in z.namelist()}


def write_package(path, parts):
    """Deterministic zip: fixed timestamps, [Content_Types].xml first."""
    names = list(parts)
    if "[Content_Types].xml" in names:
        names.remove("[Content_Types].xml")
        names.insert(0, "[Content_Types].xml")
    with zipfile.ZipFile(path, "w") as z:
        for name in names:
            info = zipfile.ZipInfo(name, FIXED_ZIP_TIME)
            info.compress_type = zipfile.ZIP_DEFLATED
            info.external_attr = 0o644 << 16
            z.writestr(info, parts[name])


def rezip_deterministic(path):
    write_package(path, read_package(path))


def xml(data):
    return E.fromstring(data)


def serial(el):
    return E.tostring(el, xml_declaration=True, encoding="UTF-8",
                      standalone=True)


def rels_part_for(part):
    d, name = posixpath.split(part)
    return posixpath.join(d, "_rels", name + ".rels")


def rel_base(rels_part):
    if rels_part == "_rels/.rels":
        return ""
    return posixpath.dirname(posixpath.dirname(rels_part))


def resolve_target(rels_part, target):
    if target.startswith("/"):
        return target.lstrip("/")
    return posixpath.normpath(posixpath.join(rel_base(rels_part), target))


def relationships(parts, part):
    """Id -> (type, target, external) for a part's relationships."""
    rp = rels_part_for(part)
    if rp not in parts:
        return {}
    out = {}
    for r in xml(parts[rp]):
        external = r.get("TargetMode") == "External"
        target = r.get("Target") if external else resolve_target(rp, r.get("Target"))
        out[r.get("Id")] = (r.get("Type"), target, external)
    return out


def package_integrity(parts, label):
    """Fail on any structural defect; return revision coverage counts."""
    errors = []
    names = set(parts)
    parsed = {}
    for name, data in parts.items():
        if name.endswith(".xml") or name.endswith(".rels"):
            try:
                parsed[name] = xml(data)
            except E.XMLSyntaxError as exc:
                errors.append("XML parse error in %s: %s" % (name, exc))
    require(not errors, "%s: %s" % (label, errors))
    # Every internal relationship target exists.
    for name in sorted(names):
        if not name.endswith(".rels"):
            continue
        for r in parsed[name]:
            if r.get("TargetMode") == "External":
                continue
            target = resolve_target(name, r.get("Target"))
            if target not in names:
                errors.append("%s -> missing %s" % (name, target))
    # Every part has a content type.
    ct = parsed["[Content_Types].xml"]
    defaults = {x.get("Extension").lower() for x in ct
                if x.tag == "{%s}Default" % CT}
    overrides = {x.get("PartName").lstrip("/") for x in ct
                 if x.tag == "{%s}Override" % CT}
    for name in names:
        if name == "[Content_Types].xml" or name.endswith("/"):
            continue
        ext = name.rsplit(".", 1)[-1].lower() if "." in name else ""
        if name not in overrides and ext not in defaults:
            errors.append("No content type for %s" % name)
    # Every relationship reference in XML parts resolves.
    for name, root in parsed.items():
        if name.endswith(".rels") or name == "[Content_Types].xml":
            continue
        rels = relationships(parts, name)
        for el in root.iter():
            for k, v in el.attrib.items():
                if k.startswith("{%s}" % R) and v not in rels:
                    errors.append("%s: unresolved r:%s=%s" % (name, k.split("}")[1], v))
    doc = parsed["word/document.xml"]
    # Revision marks: unique ids, author and date present.
    revs = doc.xpath("//w:ins | //w:del | //w:pPrChange | //w:rPrChange",
                     namespaces=NS)
    ids = [x.get(w("id")) for x in revs]
    if len(ids) != len(set(ids)):
        errors.append("Duplicate revision ids")
    for x in revs:
        if not x.get(w("author")) or not x.get(w("date")) or x.get(w("id")) is None:
            errors.append("Revision mark without id/author/date")
            break
    # Drawing object ids must be unique across the document and its stories.
    docpr = []
    for name, root in parsed.items():
        if name.startswith("word/") and not name.endswith(".rels"):
            docpr += root.xpath("//wp:docPr/@id", namespaces=NS)
    if len(docpr) != len(set(docpr)):
        errors.append("Duplicate wp:docPr ids (%d ids, %d unique)"
                      % (len(docpr), len(set(docpr))))
    # Internal hyperlinks must point at existing bookmarks.
    bookmarks = set(doc.xpath("//w:bookmarkStart/@w:name", namespaces=NS))
    for anchor in doc.xpath("//w:hyperlink/@w:anchor", namespaces=NS):
        if anchor not in bookmarks:
            errors.append("Hyperlink anchor without bookmark: %s" % anchor)
    require(not errors, "%s package integrity: %s" % (label, errors[:20]))
    ins = doc.xpath("//w:ins", namespaces=NS)
    dels = doc.xpath("//w:del", namespaces=NS)
    ppr = doc.xpath("//w:pPrChange", namespaces=NS)
    return {
        "insertions": len(ins),
        "deletions": len(dels),
        "paragraph_property_changes": len(ppr),
        "authors": sorted({x.get(w("author")) for x in revs}),
        "unique_revision_ids": True,
        "unique_drawing_ids": True,
        "valid_xml_relationships_content_types": True,
    }


# ---- Application-level DOCX semantics ---------------------------------------

def docx_semantic(path):
    """Text, math, notes, ordered image pixels, and counts of a saved DOCX."""
    from PIL import Image  # imported here so 02 runs without Pillow loaded

    parts = read_package(path)
    doc = xml(parts["word/document.xml"])
    rels = relationships(parts, "word/document.xml")
    images = []
    for blip in doc.iter("{%s}blip" % A):
        rid = blip.get("{%s}embed" % R)
        require(rid is not None and rid in rels,
                "%s: image without embedded relationship" % path)
        _, target, external = rels[rid]
        require(not external, "%s: externally linked image" % path)
        with Image.open(io.BytesIO(parts[target])) as im:
            rgba = im.convert("RGBA")
            images.append([list(rgba.size), sha256_bytes(rgba.tobytes())])

    def norm(texts):
        joined = unicodedata.normalize("NFKC", "".join(texts))
        return re.sub(r"\s+", " ", joined).strip()

    notes = []
    for part in ("word/footnotes.xml", "word/endnotes.xml"):
        if part in parts:
            notes += xml(parts[part]).xpath(".//w:t/text()", namespaces=NS)
    return {
        "text": norm(doc.xpath(".//w:t/text()", namespaces=NS)),
        "math": norm(doc.xpath(".//m:t/text()", namespaces=NS)),
        "notes": norm(notes),
        "images": images,
        "table_count": len(doc.findall(".//" + w("tbl"))),
        "drawing_count": len(doc.findall(".//" + w("drawing"))),
    }
