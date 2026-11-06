#!/usr/bin/env bash
# Reproduce the full pipeline from data/raw/data.csv
set -e
cd "$(dirname "$0")"
mkdir -p reports tables figures data/processed

run() {
  local f="$1"
  if [ -f "$f" ]; then
    echo "  >> Rscript $f"
    Rscript "$f" > "reports/$(basename "$f" .R)_run.log" 2>&1 || {
      echo "  FAILED: $f  (see reports/$(basename "$f" .R)_run.log)"
      exit 1
    }
  else
    echo "  SKIP: $f (not present)"
  fi
}

echo "[1/4] pipeline + models"
for f in lib/01_pipeline_w1_w5.R lib/01b_models.R; do run "$f"; done

echo "[2/4] W1-W5 analysis"
for f in R/01_explore.R R/02_missing.R R/03_baseline_lm.R \
         R/04_diagnostics.R R/05_remedies.R R/06_multicollinearity.R \
         R/07_model_building.R R/08_model_selection.R R/09_mixed_effects.R \
         R/10_simultaneous_inference.R R/11_logistic.R; do run "$f"; done

echo "[3/4] W6-W7 analysis"
for f in R/12_resampling.R R/13_survival.R R/14_holdout.R \
         lib/15b_decision_rule.R lib/17_h1_sensitivity.R \
         lib/18_data_corrections.R lib/19_lrt_fix.R; do run "$f"; done

echo "[4/4] render report"
Rscript -e 'rmarkdown::render("reports/final_report.Rmd",
  output_file="final_report.html", knit_root_dir=getwd())'

echo "pipeline complete"
