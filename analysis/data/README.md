# Data dictionary — `data/raw/data.csv`

132 rows × 60 columns. Never edit in place; all transformations go to `data/processed/`.

## Key columns

| Column | Type | Meaning |
|---|---|---|
| name | character | Coach name |
| school | character | Hiring school |
| first_season | integer | First season at school |
| last_season | integer | Last observed season (NA for 2026 speculative hires) |
| Conf.Hired | character | Conference of hiring school |
| Conf.Leaving | character | Prior conference |
| PrevHCAnyLevel | 0/1 | Prior head-coaching experience |
| Fired | 0/1 | Fired for on-field performance |
| OffFieldIssuesFiring | 0/1 | Fired for off-field reasons (censored in survival) |
| StillEmployedAtSchool | 0/1 | Still at school as of latest observation |
| AllPriorWinPct | numeric | Career win% as prior HC (NA if first-time HC) |
| PreviousWinPct | numeric | Win% at most recent HC job (0.5 if no prior HC) |
| PreviousOC | 0/1 | Previously offensive coordinator |
| PreviousDC | 0/1 | Previously defensive coordinator |
| PlayedNFL | 0/1 | Played in the NFL |
| ConnectionToHiring.School | 0/1 | Prior connection to hiring school |
| team_sp_recent | numeric | Program SP+ prior 3 years |
| sp_prev_rel | numeric | Coach SP+ relative to FBS mean in prior role |
| sp_rel_team | numeric | **Outcome**: team SP+ relative to FBS mean (CPI) |
| winpct_rel_team | numeric | **Sensitivity outcome**: team win% relative to mean |
| AgeAtHiringMinus50 | integer | Coach age minus 50 at hire |
| WonCCasHCany | 0/1 | Won conference championship as HC |
| Conf.LeavingP4 | 0/1 | Left a Power-4 conference |

## Missing-value conventions

- `AllPriorWinPct`: structurally NA for first-time HCs (`PrevHCAnyLevel == 0`); imputed to 0.50 in `R/02_missing.R`.
- Two prior-HC rows (Chad Morris, Dana Holgorsen) had `AllPriorWinPct` missing despite valid `PreviousWinPct`; imputed from `PreviousWinPct`.
- Sam Pittman (Arkansas 2020) excluded: `PrevHCAnyLevel = 1` but `Wins.at.Prev = 0, Losses.at.Prev = 0` (miscoded).
- 12 speculative 2026 hires have `last_season = NA`, `StillEmployedAtSchool = NA`; excluded from all analyses.

## Data integrity

- Raw CSV hash in `data/raw/data.csv.sha256`.
- Flagged rows documented in `tables/data_corrections.csv` (5 rows: Hugh Freeze, Luke Fickell, Brian Kelly, Mike Gundy, James Franklin).
- Exclusion and inclusion lists in `tables/exclusion_list.csv` and `tables/inclusion_list.csv`.
