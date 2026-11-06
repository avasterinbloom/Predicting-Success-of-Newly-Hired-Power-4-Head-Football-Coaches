#!/usr/bin/env bash
# Reproduce the saved analysis pipeline without re-rendering final_report.html.
set -euo pipefail
cd "$(dirname "$0")"
command -v Rscript >/dev/null 2>&1 || { echo "ERROR: Rscript is required." >&2; exit 127; }
mkdir -p reports tables figures data/processed
run() {
  local f="$1"
  if [[ -f "$f" ]]; then
    echo ">> Rscript $f"
    Rscript "$f" > "reports/$(basename "$f" .R)_run.log" 2>&1 || {
      echo "FAILED: $f (see reports/$(basename "$f" .R)_run.log)" >&2
      exit 1
    }
  else
    echo "SKIP: $f (not present)"
  fi
}
run lib/01_pipeline_w1_w5.R
run lib/01b_models.R
for f in \
  R/01_explore.R R/02_missing.R R/03_baseline_lm.R R/04_diagnostics.R \
  R/05_remedies.R R/06_multicollinearity.R R/07_model_building.R \
  R/08_model_selection.R R/09_mixed_effects.R R/10_simultaneous_inference.R \
  R/11_logistic.R R/12_resampling.R R/13_survival.R R/14_holdout.R; do run "$f"; done
for f in lib/15b_decision_rule.R lib/17_h1_sensitivity.R lib/18_data_corrections.R lib/19_lrt_fix.R; do run "$f"; done
echo "Analysis run complete. The existing final_report.Rmd and final_report.html were not rendered or overwritten."
