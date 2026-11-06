"""
JPAM revision audit: native-application revision check in LibreOffice
Date: November 5th, 2026
Purpose: Open the tracked DOCX files in a private, headless LibreOffice
instance and exercise the revisions the way a reviewer would.

  - Starts soffice with its own throwaway profile under WORK/office/ and a
    private pipe; it never connects to a running LibreOffice or to Microsoft
    Word, and it never opens files outside WORK.
  - For each scope: exports the all-markup PDF preview, runs Accept All and
    Reject All on separate loads, and saves each result as DOCX.
  - Opens the clean final and clean R2 inputs in the same instance, saves
    them as controls, and exports a clean preview PDF of each final input.
  - Compares accept vs clean final and reject vs clean R2 inside the
    application: paragraph texts in order, footnote and endnote texts,
    graphic count, and the ordered list of graphics exported as PNG
    (pixel size and SHA-256). Any difference fails the run.
  - Exports comment_audit.docx to PDF.

Run with LibreOffice's bundled Python (it provides the uno module):
  "$JPAM_LO_PYTHON" 03-office-verify.py WORK_DIR SOFFICE_BINARY
Results -> WORK/office/{scope}-markup.pdf
        -> WORK/office/{scope}-native-accept.docx, {scope}-native-reject.docx
        -> WORK/office/{scope}-control-new.docx, {scope}-control-old.docx
        -> WORK/office/{scope}-clean-new-preview.pdf
        -> WORK/office/comment_audit.pdf
        -> WORK/office/office-report.json
Runner: step 3 of 98-run-all.sh. Exit 498 on any failed check.
"""

import hashlib
import json
import os
import subprocess
import sys
import time
from pathlib import Path

import uno
from com.sun.star.beans import PropertyValue
from com.sun.star.connection import NoConnectException

SCOPES = ("manuscript", "appendix")
CONNECT_TIMEOUT = 120
EXIT_CONTRACT = 498


def fail(message):
    print("ERROR: " + message, file=sys.stderr, flush=True)
    sys.exit(EXIT_CONTRACT)


def prop(name, value):
    p = PropertyValue()
    p.Name = name
    p.Value = value
    return p


def url(path):
    return uno.systemPathToFileUrl(str(path))


class Office:
    def __init__(self, soffice, office_dir):
        self.office_dir = office_dir
        profile = office_dir / "lo-profile"
        pipe = "jpam_audit_%d" % os.getpid()
        self.proc = subprocess.Popen(
            [soffice, "-env:UserInstallation=" + url(profile), "--headless",
             "--invisible", "--norestore", "--nodefault", "--nologo",
             "--nofirststartwizard",
             "--accept=pipe,name=%s;urp;StarOffice.ComponentContext" % pipe],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        local = uno.getComponentContext()
        resolver = local.ServiceManager.createInstanceWithContext(
            "com.sun.star.bridge.UnoUrlResolver", local)
        deadline = time.time() + CONNECT_TIMEOUT
        self.ctx = None
        while self.ctx is None:
            if self.proc.poll() is not None:
                fail("soffice exited during start-up (code %s)" % self.proc.returncode)
            try:
                self.ctx = resolver.resolve(
                    "uno:pipe,name=%s;urp;StarOffice.ComponentContext" % pipe)
            except NoConnectException:
                if time.time() > deadline:
                    self.proc.kill()
                    fail("Could not connect to the private soffice instance")
                time.sleep(1)
        sm = self.ctx.ServiceManager
        self.desktop = sm.createInstanceWithContext("com.sun.star.frame.Desktop", self.ctx)
        self.dispatcher = sm.createInstanceWithContext(
            "com.sun.star.frame.DispatchHelper", self.ctx)
        self.graphics = sm.createInstanceWithContext(
            "com.sun.star.graphic.GraphicProvider", self.ctx)
        provider = sm.createInstanceWithContext(
            "com.sun.star.configuration.ConfigurationProvider", self.ctx)
        product = provider.createInstanceWithArguments(
            "com.sun.star.configuration.ConfigurationAccess",
            (prop("nodepath", "/org.openoffice.Setup/Product"),))
        self.version = "%s %s" % (product.getByName("ooName"),
                                  product.getByName("ooSetupVersionAboutBox"))

    def load(self, path):
        if not Path(path).is_file():
            fail("Missing input %s" % path)
        doc = self.desktop.loadComponentFromURL(
            url(path), "_blank", 0,
            (prop("Hidden", True), prop("UpdateDocMode", 0), prop("ReadOnly", False)))
        if doc is None:
            fail("LibreOffice could not open %s" % path)
        return doc

    def save_docx(self, doc, path):
        doc.storeToURL(url(path), (prop("FilterName", "Office Open XML Text"),
                                   prop("Overwrite", True)))

    def save_pdf(self, doc, path):
        doc.storeToURL(url(path), (prop("FilterName", "writer_pdf_Export"),
                                   prop("Overwrite", True)))

    def dispatch(self, doc, command):
        self.dispatcher.executeDispatch(doc.getCurrentController().getFrame(),
                                        command, "", 0, ())

    def graphic_digest(self, obj, index):
        tmp = self.office_dir / ("graphic-export-%d.png" % index)
        self.graphics.storeGraphic(obj.Graphic, (prop("URL", url(tmp)),
                                                 prop("MimeType", "image/png")))
        data = tmp.read_bytes()
        tmp.unlink()
        size = obj.Graphic.SizePixel
        return [size.Width, size.Height, hashlib.sha256(data).hexdigest()]

    def snapshot(self, doc, label):
        """Ordered paragraph texts, notes, and graphics as the app sees them."""
        if doc.TextTables.Count:
            fail("%s: Word tables are outside the supported review format" % label)
        paragraphs = []
        graphics = []
        enum = doc.Text.createEnumeration()
        while enum.hasMoreElements():
            par = enum.nextElement()
            if not par.supportsService("com.sun.star.text.Paragraph"):
                fail("%s: unsupported body element %s" % (label, par.getImplementationName()))
            paragraphs.append(par.getString())
            portions = par.createEnumeration()
            while portions.hasMoreElements():
                portion = portions.nextElement()
                if portion.TextPortionType != "Frame":
                    continue
                contents = portion.createContentEnumeration("com.sun.star.text.TextContent")
                while contents.hasMoreElements():
                    obj = contents.nextElement()
                    if not obj.supportsService("com.sun.star.text.TextGraphicObject"):
                        fail("%s: unsupported anchored object %s"
                             % (label, obj.getImplementationName()))
                    graphics.append(self.graphic_digest(obj, len(graphics)))
        if len(graphics) != doc.GraphicObjects.Count:
            fail("%s: %d graphics in text order but %d in the document"
                 % (label, len(graphics), doc.GraphicObjects.Count))
        notes = {}
        for kind in ("Footnotes", "Endnotes"):
            coll = getattr(doc, kind)
            notes[kind.lower()] = [coll.getByIndex(i).getString()
                                   for i in range(coll.getCount())]
        return {"paragraphs": paragraphs, "footnotes": notes["footnotes"],
                "endnotes": notes["endnotes"], "graphics": graphics}

    def close(self, doc):
        doc.close(True)

    def shutdown(self):
        try:
            self.desktop.terminate()
        except Exception:  # the bridge closes as soffice exits
            pass
        try:
            self.proc.wait(timeout=60)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            self.proc.wait()


def compare(actual, expected):
    checks = {}
    for key in ("paragraphs", "footnotes", "endnotes", "graphics"):
        checks[key] = actual[key] == expected[key]
    checks["graphic_count"] = len(actual["graphics"]) == len(expected["graphics"])
    return checks


def first_difference(a, b):
    for i, (x, y) in enumerate(zip(a, b)):
        if x != y:
            return "item %d: %r vs %r" % (i + 1, str(x)[:300], str(y)[:300])
    return "lengths %d vs %d" % (len(a), len(b))


def main():
    if len(sys.argv) != 3:
        fail("Usage: 03-office-verify.py WORK_DIR SOFFICE_BINARY")
    work = Path(sys.argv[1]).resolve()
    soffice = sys.argv[2]
    office_dir = work / "office"
    office_dir.mkdir(exist_ok=True)
    office = Office(soffice, office_dir)
    report = {"application": office.version, "scopes": {}}
    failures = []
    try:
        for scope in SCOPES:
            tracked = work / "tracked" / (scope + "-tracked.docx")
            entry = {}
            doc = office.load(tracked)
            entry["redlines_loaded"] = doc.Redlines.Count
            tracker = json.loads((work / "tracked" / (scope + "-report.json"))
                                 .read_text(encoding="utf-8"))["coverage"]
            expected = (tracker["insertions"] + tracker["deletions"]
                        + tracker["paragraph_property_changes"])
            if (doc.Redlines.Count > 0) != (expected > 0):
                failures.append("%s: LibreOffice loaded %d revisions; the XML has %d marks"
                                % (scope, doc.Redlines.Count, expected))
            doc.ShowChanges = True
            office.save_pdf(doc, office_dir / (scope + "-markup.pdf"))
            office.close(doc)
            for action, version in (("Accept", "new"), ("Reject", "old")):
                doc = office.load(tracked)
                office.dispatch(doc, ".uno:%sAllTrackedChanges" % action)
                remaining = doc.Redlines.Count
                entry[action.lower() + "_remaining_redlines"] = remaining
                if remaining:
                    failures.append("%s %s: %d revisions remain" % (scope, action, remaining))
                native = office.snapshot(doc, "%s %s" % (scope, action))
                office.save_docx(doc, office_dir / ("%s-native-%s.docx" % (scope, action.lower())))
                office.close(doc)
                clean = work / "clean" / version / (scope + "-clean.docx")
                doc = office.load(clean)
                if doc.Redlines.Count:
                    failures.append("%s clean %s input carries revisions" % (scope, version))
                control = office.snapshot(doc, "%s clean %s" % (scope, version))
                office.save_docx(doc, office_dir / ("%s-control-%s.docx" % (scope, version)))
                if version == "new":
                    office.save_pdf(doc, office_dir / (scope + "-clean-new-preview.pdf"))
                office.close(doc)
                checks = compare(native, control)
                entry[action.lower()] = checks
                for key, ok in checks.items():
                    if not ok:
                        failures.append("%s %s %s differ: %s" % (
                            scope, action, key,
                            first_difference(native.get(key, []), control.get(key, []))
                            if key != "graphic_count" else
                            "%d vs %d" % (len(native["graphics"]), len(control["graphics"]))))
                entry[action.lower() + "_counts"] = {
                    "paragraphs": len(native["paragraphs"]),
                    "graphics": len(native["graphics"]),
                    "footnotes": len(native["footnotes"]),
                    "endnotes": len(native["endnotes"])}
            report["scopes"][scope] = entry
            print("%s: %d revisions loaded; accept/reject checked" % (
                scope, entry["redlines_loaded"]), flush=True)
        doc = office.load(work / "audit" / "comment_audit.docx")
        office.save_pdf(doc, office_dir / "comment_audit.pdf")
        office.close(doc)
    finally:
        office.shutdown()
    report["failures"] = failures
    (office_dir / "office-report.json").write_text(json.dumps(report, indent=2) + "\n",
                                                   encoding="utf-8")
    if failures:
        fail("Native application checks failed:\n" + "\n".join(failures))
    print("Step 3 complete: %s" % report["application"], flush=True)


if __name__ == "__main__":
    main()
