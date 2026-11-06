#!/usr/bin/env python3
"""
JPAM revision audit: final checks and package publication
Date: November 5th, 2026
Purpose: Check the LibreOffice outputs from step 3 and, only if every check
passes, copy the review package into the audit directory and write
verification.json.

Checks (each failure stops the run before anything is published):
  - The application report from step 3 has no failures.
  - The DOCX saved after Accept All equals the clean final input passed
    through the same application, and Reject All equals the clean R2 input:
    text, math text, notes, ordered image pixels, table and drawing counts.
  - The tracked DOCX passes the package integrity checks again.
  - Every PDF opens with pdfinfo and has at least one page.
  - The live submission sources and comment_audit.md still match the
    snapshot taken in step 1 (no edits during the build).

Usage: python3 04-verify-and-publish.py WORK_DIR
Results -> <audit_dir>/{manuscript,appendix}_tracked.docx
        -> <audit_dir>/{manuscript,appendix}_tracked_preview.pdf
        -> <audit_dir>/comparison-inputs/{scope}_{R2_original,final_revision}.docx
        -> <audit_dir>/comment_audit.docx, comment_audit.pdf
        -> <audit_dir>/verification.json
Runner: step 4 of 98-run-all.sh.
"""

import difflib
import re
import shutil
import subprocess
import time

import review_lib as RL

SCOPES = ("manuscript", "appendix")
ROUNDTRIPS = (("accept", "new"), ("reject", "old"))


def pdf_pages(path):
    RL.require(path.is_file(), "Missing PDF %s" % path, RL.EXIT_MISSING_FILE)
    r = subprocess.run(["pdfinfo", str(path)], capture_output=True, text=True)
    RL.require(r.returncode == 0, "pdfinfo failed on %s: %s" % (path, r.stderr))
    pages = int(re.search(r"Pages:\s+(\d+)", r.stdout).group(1))
    RL.require(pages > 0, "%s has no pages" % path)
    return pages


def describe(a, b, key):
    if key in ("text", "math", "notes"):
        sm = difflib.SequenceMatcher(None, b[key], a[key], autojunk=False)
        diffs = [(tag, b[key][i1:i2][:120], a[key][j1:j2][:120])
                 for tag, i1, i2, j1, j2 in sm.get_opcodes() if tag != "equal"]
        return "%d differences, first: %r" % (len(diffs), diffs[:3])
    if key == "images":
        for i, (x, y) in enumerate(zip(a[key], b[key])):
            if x != y:
                return "image %d differs" % (i + 1)
        return "image counts %d vs %d" % (len(a[key]), len(b[key]))
    return "%r vs %r" % (a[key], b[key])


def main():
    cfg = RL.load_config()
    root = RL.repo_root()
    work = RL.work_dir_from_argv()
    manifest = RL.read_json(work / "build-manifest.json")
    office = RL.read_json(work / "office" / "office-report.json")
    RL.require(not office["failures"], "Step 3 recorded failures: %s" % office["failures"])

    roundtrips = {}
    native_xml = {}
    pairing = {}
    xml_projections = {}
    counts = {}
    for scope in SCOPES:
        tracked = work / "tracked" / (scope + "-tracked.docx")
        report = RL.read_json(work / "tracked" / (scope + "-report.json"))
        native_xml[scope] = RL.package_integrity(RL.read_package(tracked), scope + " tracked")
        RL.require(native_xml[scope] == report["coverage"],
                   "%s tracked DOCX changed after step 2" % scope)
        pairing[scope] = report["exhibit_pairing"]
        xml_projections[scope] = report["xml_projections"]
        counts[scope] = report["exhibit_counts"]
        for action, version in ROUNDTRIPS:
            a = RL.docx_semantic(work / "office" / ("%s-native-%s.docx" % (scope, action)))
            b = RL.docx_semantic(work / "office" / ("%s-control-%s.docx" % (scope, version)))
            checks = {k: a[k] == b[k] for k in a}
            bad = ["%s (%s)" % (k, describe(a, b, k)) for k, ok in checks.items() if not ok]
            RL.require(not bad, "%s %s does not reproduce the clean %s input: %s"
                       % (scope, action, version, "; ".join(bad)))
            app = office["scopes"][scope][action]
            RL.require(all(app.values()), "%s %s application checks failed: %s"
                       % (scope, action, app))
            checks.update({"app_" + k: v for k, v in app.items()})
            checks["image_count"] = len(a["images"])
            roundtrips["%s_%s" % (scope, action)] = checks
            RL.log("%s %s: application output matches the clean %s input"
                   % (scope, action, version))

    pdfs = {}
    for scope in SCOPES:
        pdfs[scope + "-markup.pdf"] = pdf_pages(work / "office" / (scope + "-markup.pdf"))
        pdfs[scope + "-clean-new-preview.pdf"] = pdf_pages(
            work / "office" / (scope + "-clean-new-preview.pdf"))
    pdfs["comment_audit.pdf"] = pdf_pages(work / "office" / "comment_audit.pdf")

    # Sources must not have changed between the snapshot and publication.
    for v in ("old", "new"):
        live = RL.source_manifest(root / cfg["versions"][v]["dir"], cfg)
        snap = manifest["sources"][v]["sha256"]
        changed = sorted(k for k in set(live) | set(snap) if live.get(k) != snap.get(k))
        RL.require(not changed, "%s sources changed during the build; rerun "
                   "98-run-all.sh: %s" % (cfg["versions"][v]["dir"], changed))
    audit_dir = root / cfg["audit_dir"]
    md = audit_dir / cfg["audit_markdown"]
    RL.require(RL.sha256_file(md) == manifest["audit_markdown_sha256"],
               "%s changed during the build; rerun 98-run-all.sh" % md.name)

    # Publish.
    inputs = audit_dir / "comparison-inputs"
    inputs.mkdir(exist_ok=True)
    copies = []
    for scope in SCOPES:
        copies.append((work / "tracked" / (scope + "-tracked.docx"),
                       audit_dir / (scope + "_tracked.docx")))
        copies.append((work / "office" / (scope + "-markup.pdf"),
                       audit_dir / (scope + "_tracked_preview.pdf")))
        for v in ("old", "new"):
            copies.append((work / "clean" / v / (scope + "-clean.docx"),
                           inputs / ("%s_%s.docx" % (scope, cfg["versions"][v]["tag"]))))
    copies.append((work / "audit" / "comment_audit.docx", audit_dir / "comment_audit.docx"))
    copies.append((work / "office" / "comment_audit.pdf", audit_dir / "comment_audit.pdf"))
    for src, dst in copies:
        RL.require(src.is_file(), "Missing build output %s" % src, RL.EXIT_MISSING_FILE)
        shutil.copyfile(src, dst)

    def short(scope, v):
        return "%s_%s" % (cfg["scopes"][scope]["short"], cfg["versions"][v]["short"])
    tools = {p.name: RL.sha256_file(p) for p in sorted(RL.TOOLS_DIR.iterdir())
             if p.is_file() and not p.name.startswith(".")}
    record = {
        "audit_date": cfg["audit_date"],
        "built_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "command": (cfg["audit_dir"] + "/tools/98-run-all.sh"),
        "tools_sha256": tools,
        "baseline": cfg["versions"]["old"]["dir"],
        "revised": cfg["versions"]["new"]["dir"],
        "submission_sources_modified_by_tools": False,
        "sources_unchanged_during_build": True,
        "source_sha256": {cfg["versions"][v]["dir"].rsplit("/", 1)[-1]:
                          manifest["sources"][v]["sha256"] for v in ("old", "new")},
        "included_tex": manifest["included_tex"],
        "duplicate_labels_resolved_as_latex": manifest["duplicate_labels"],
        "csl": cfg["csl"],
        "csl_sha256": manifest["csl_sha256"],
        "comparison_method": (
            "Identical LaTeX-source conversion for both versions (AUX-resolved "
            "numbers, citeproc references); tables with notes and all equations "
            "rendered from LaTeX as images; notes collected at the end; native "
            "OOXML word-level insertions and deletions for similar prose, with "
            "whole-paragraph replacements where needed to preserve boundaries; "
            "an exhibit is revised in place only when its rendered images are identical, "
            "otherwise the old exhibit is deleted whole and the new one inserted "
            "whole."),
        "pandoc": manifest["pandoc"],
        "application": office["application"] + ", private headless profile; "
                       "Microsoft Word not automated",
        "xml_projection_roundtrips": xml_projections,
        "native_application_roundtrips": roundtrips,
        "native_redlines_loaded": {s: office["scopes"][s]["redlines_loaded"] for s in SCOPES},
        "native_xml": native_xml,
        "exhibit_pairing": pairing,
        "figure_counts": {short(s, v): counts[s][v + "_figures"]
                          for s in SCOPES for v in ("old", "new")},
        "logical_table_counts": {short(s, v): counts[s][v + "_tables"]
                                 for s in SCOPES for v in ("old", "new")},
        "note_counts": {short(s, v): manifest["notes"]["%s_%s" % (v, s)]
                        for s in SCOPES for v in ("old", "new")},
        "pdf_pages": pdfs,
        "artifacts": {dst.relative_to(audit_dir).as_posix(): RL.sha256_file(dst)
                      for _, dst in sorted(copies, key=lambda x: str(x[1]))},
    }
    record["artifacts"][md.name] = manifest["audit_markdown_sha256"]
    RL.write_json(audit_dir / "verification.json", record)
    RL.log("Step 4 complete: package published to %s" % audit_dir.relative_to(root))


if __name__ == "__main__":
    main()
