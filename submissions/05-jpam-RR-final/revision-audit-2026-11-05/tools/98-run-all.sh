#!/usr/bin/env bash
# JPAM revision audit: rebuild the tracked-changes review package
# Date: November 5th, 2026
# Purpose: Run the four review-package steps in order and stop at the first
# failure. Reads submissions/04-jpam-RR-v2 and submissions/05-jpam-RR-final;
# never writes to either source tree except the audit directory's published
# package files. Builds happen in a separate work directory, which is kept.
#
#   1  01-build-clean-docx.py     clean R2 and final DOCX from LaTeX
#   2  02-make-tracked-docx.py    native revisions + exact XML projections
#   3  03-office-verify.py        private LibreOffice: markup PDF,
#                                 Accept All / Reject All, app-level checks
#   4  04-verify-and-publish.py   final checks, publish, verification.json
#
# refresh-table-notes.R (table-note regeneration in 05 sources) is a source
# step owned separately and is not run here; run it first when needed.
#
# Environment (all optional):
#   JPAM_REPO_ROOT   repository root (default: derived from this file)
#   JPAM_PYTHON      Python with lxml, Pillow, python-docx (default: python3)
#   JPAM_SOFFICE     LibreOffice soffice binary
#   JPAM_LO_PYTHON   LibreOffice bundled Python (provides uno)
#   JPAM_AUDIT_WORK  work directory, must be empty (default: new mktemp dir)
# Exit codes: 499 missing tool or package, 498 failed check, 601 missing file.
# Results -> <audit_dir>/{manuscript,appendix}_tracked.docx and _preview.pdf
#         -> <audit_dir>/comparison-inputs/*.docx
#         -> <audit_dir>/comment_audit.docx, comment_audit.pdf
#         -> <audit_dir>/verification.json
#         -> $WORK/run.log and all intermediate files

set -euo pipefail

TOOLS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export JPAM_REPO_ROOT="${JPAM_REPO_ROOT:-$(cd "$TOOLS_DIR/../../../.." && pwd)}"
PY="${JPAM_PYTHON:-python3}"
SOFFICE="${JPAM_SOFFICE:-/Applications/LibreOffice.app/Contents/MacOS/soffice}"
LO_PYTHON="${JPAM_LO_PYTHON:-/Applications/LibreOffice.app/Contents/Resources/python}"

die() {
  echo "ERROR: $2" >&2
  exit "$1"
}

# ---- Read-only preflight; installs nothing ----
for cmd in pandoc latexmk pdflatex biber pdftoppm pdfinfo; do
  command -v "$cmd" >/dev/null 2>&1 || die 499 "$cmd not found on PATH"
done
"$PY" -c 'import lxml, PIL, docx' 2>/dev/null \
  || die 499 "$PY lacks lxml, Pillow, or python-docx; set JPAM_PYTHON"
[ -x "$SOFFICE" ] || die 499 "soffice not found at $SOFFICE; set JPAM_SOFFICE"
[ -x "$LO_PYTHON" ] || die 499 "LibreOffice Python not found at $LO_PYTHON; set JPAM_LO_PYTHON"
"$LO_PYTHON" -c 'import uno' 2>/dev/null \
  || die 499 "$LO_PYTHON cannot import uno; set JPAM_LO_PYTHON"
[ -d "$JPAM_REPO_ROOT/submissions/04-jpam-RR-v2" ] \
  || die 601 "Repository root not found: $JPAM_REPO_ROOT"

if [ -n "${JPAM_AUDIT_WORK:-}" ]; then
  mkdir -p "$JPAM_AUDIT_WORK"
  [ -z "$(ls -A "$JPAM_AUDIT_WORK")" ] || die 498 "JPAM_AUDIT_WORK is not empty"
  WORK="$(cd "$JPAM_AUDIT_WORK" && pwd)"
else
  WORK="$(mktemp -d -t jpam-audit.XXXXXX)"
fi

# The work log sits outside the build tree checked for emptiness by step 1.
LOG="$WORK.log"
exec > >(tee -a "$LOG") 2>&1

echo "Repository: $JPAM_REPO_ROOT"
echo "Work:       $WORK"
echo "Log:        $LOG"
echo "Python:     $("$PY" --version 2>&1)"
echo "Pandoc:     $(pandoc --version | head -n 1)"
echo

step() {
  local label="$1"
  shift
  if "$@"; then
    printf '%-44s OK\n' "$label"
  else
    local code=$?
    printf '%-44s FAILED (exit %d)\n' "$label" "$code"
    echo "Work directory kept for inspection: $WORK"
    exit "$code"
  fi
}

step "1 clean comparison inputs" "$PY" "$TOOLS_DIR/01-build-clean-docx.py" "$WORK"
step "2 native tracked revisions" "$PY" "$TOOLS_DIR/02-make-tracked-docx.py" "$WORK"
step "3 LibreOffice accept/reject checks" "$LO_PYTHON" "$TOOLS_DIR/03-office-verify.py" \
  "$WORK" "$SOFFICE"
step "4 verify and publish" "$PY" "$TOOLS_DIR/04-verify-and-publish.py" "$WORK"

echo
echo "Package rebuilt. Intermediates kept in $WORK"
