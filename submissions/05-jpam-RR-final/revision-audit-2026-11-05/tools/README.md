# Rebuilding the review package

These scripts rebuild the tracked-changes package in this audit directory from the LaTeX sources of `submissions/04-jpam-RR-v2` (R2, read only) and `submissions/05-jpam-RR-final` (final revision). They never edit either source tree. Every build runs in a separate work directory, and the package files are replaced only after all checks pass.

## Run

From the repository root:

```sh
# Optional source step, owned separately: regenerate table notes in 05.
Rscript submissions/05-jpam-RR-final/revision-audit-2026-11-05/tools/refresh-table-notes.R

# Build, check, and publish the review package.
JPAM_PYTHON=/opt/anaconda3/bin/python3 \
  submissions/05-jpam-RR-final/revision-audit-2026-11-05/tools/98-run-all.sh
```

`JPAM_PYTHON` must point to a Python 3.8+ with lxml, Pillow, and python-docx. The other settings are optional: `JPAM_SOFFICE` and `JPAM_LO_PYTHON` (LibreOffice binary and its bundled Python; defaults are the standard macOS app paths), `JPAM_REPO_ROOT`, and `JPAM_AUDIT_WORK` (an empty work directory; default is a new `mktemp` directory). The runner prints the work directory and log path and keeps both.

Requirements: pandoc 3.3, latexmk, pdflatex, biber, poppler (`pdftoppm`, `pdfinfo`), LibreOffice 7.5.

## Steps

1. `01-build-clean-docx.py` copies each version's `.tex`, `.bib`, and `.png` files (including `fragments/`) into the work directory, compiles `submit_preprint.tex` with latexmk, and reads exhibit, equation, and section numbers from that version's AUX file. It expands `\input` with TeX comment and line-end rules and converts the title, abstract, main text, tables, figures, and appendix with Pandoc and citeproc (`my_paper/csl/apa.csl`). Tables with their notes and all equations are rendered from LaTeX as images, because LibreOffice 7.5 drops rows of tracked Word tables and OMML. Footnotes become numbered notes at the end. It also converts `comment_audit.md` to DOCX.
2. `02-make-tracked-docx.py` writes native Word revisions. Similar prose paragraphs are compared word by word; whole-paragraph replacement is used when needed to preserve a boundary beside inserted paragraphs. A table or figure is revised in place only when its rendered images are identical in both versions. Otherwise the old exhibit is deleted whole and the new one inserted whole, so captions of different objects are never merged. It then applies Accept All and Reject All to the XML and requires an exact match with the clean final and R2 inputs.
3. `03-office-verify.py` runs under LibreOffice's Python with a private profile and pipe. It never touches Microsoft Word or a running LibreOffice. It exports the all-markup PDF, runs Accept All and Reject All, and compares the results with the clean inputs opened in the same application: paragraph text in order, notes, and every graphic's pixel size and PNG hash.
4. `04-verify-and-publish.py` repeats the comparison on the saved DOCX files (text, math, notes, ordered image pixels, counts) and rechecks package integrity: XML, relationships, content types, unique revision and drawing ids. It confirms that the sources and `comment_audit.md` did not change during the build. It then copies the package here and writes `verification.json`.

Any unsupported LaTeX construct, unresolved reference or citation, missing image or `\input` file, Pandoc warning, or failed check stops the run with a non-zero exit code. There is no fallback output.

## Files

- `review-config.json`: source directories, CSL, output names, and the fixed revision author and date.
- `review_lib.py`: shared helpers for steps 1, 2, and 4.
- `refresh-table-notes.R`: table-note regeneration for the 05 sources. It is not part of `98-run-all.sh`.
