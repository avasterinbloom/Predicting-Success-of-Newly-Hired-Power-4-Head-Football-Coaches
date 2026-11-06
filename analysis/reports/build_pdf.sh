#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
command -v xelatex >/dev/null 2>&1 || { echo "ERROR: xelatex is required." >&2; exit 127; }
xelatex -interaction=nonstopmode -halt-on-error final_report.tex
xelatex -interaction=nonstopmode -halt-on-error final_report.tex
pdfinfo final_report.pdf | grep -E '^(Pages|Page size|File size):'
