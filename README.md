# RT — Coaching Hires Regression Project

This is the group-folder root for the course submission. The current folder label `RT` is a placeholder because the official group name was not supplied with the project archive. **Before submission, rename this folder to the exact group name required by the professor** (for example, `Group A-CC`). Do not change the archive filenames unless required by the submission portal.

## Start here

1. Read `analysis/README.md` for the project map, dependencies, data provenance, and reproduction instructions.
2. Read `analysis/reports/final_report.pdf` for the final report (11 pages including title, contents, appendix material and references; within the 20-page limit).
3. Use `analysis/reports/final_report.tex` to edit and rebuild the report.
4. The downloadable source dataset is `analysis/data/raw/data.csv`; its SHA-256 is recorded alongside it.
5. The R source, processed data, figures, numerical result tables, and weekly memos are included under `analysis/`.

The existing `final_report.Rmd` and `final_report.html` were copied unchanged from the supplied archive. The final submission report is the TeX/PDF pair. The team-compiled dataset's coach-profile URLs are preserved in the CSV; the report explicitly discloses that a complete historical SP+ source ledger is not present.

## Statistical interpretation

The report does not claim that a validated hiring predictor has been found. No H1-H4 hypothesis survives Holm adjustment; the 16-parameter confirmatory model has negative 10-fold CV R-squared; the BIC model's post hoc temporal holdout does not beat the simple mean baseline. Two small archived numerical inconsistencies and the corrected mixed-effects inference are disclosed in the report.


## Submission package contents

This ZIP contains the files needed to read, reproduce, and share the project: R scripts and helper functions; raw and processed data; figures and result tables; the final TeX/PDF report; the unchanged R Markdown/HTML outputs; and supporting memos/provenance notes. Internal duplicate-audit files, the legacy 60-page report package, and presentation slides are omitted because they are not needed to evaluate or rerun the submitted analysis.
