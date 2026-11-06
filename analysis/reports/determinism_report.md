# Determinism report

All scripts set `set.seed(2026)`. Twin runs of the resampling scripts
produce byte-identical logs.

| Script | Twin-run |
|--------|----------|
| 08_model_selection | identical |
| 12_resampling | identical |
| 14_holdout | identical |

Verify with:
Rscript R/<script>.R > reports/<script>_a.log 2>&1
Rscript R/<script>.R > reports/<script>_b.log 2>&1
diff reports/<script>_a.log reports/<script>_b.log && echo identical
