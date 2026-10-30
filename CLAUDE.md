# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Instructions

- DO NOT TAKE INITIATIVES.
- Only perform the exact actions explicitly requested by the user.
- Do not make changes, edits, or updates unless directly asked.
- __YOU ARE NOT ALLOWED TO READ ANY FILES__ in `analysis/generated_data/` (raw API caches, Stage 1/2 outputs, combined CSVs), `shiny/data/`, or the `analysis/*.zip` archives. Package datasets in `data/*.rda` are read through R (`OutbreakExtractR::WPP2024`), never opened directly.
- Ask before any HPC (Yggdrasil) submission: show the plan or dry run and wait for a yes.
- API credentials only ever come from the env vars `CHOLERA_API_USERNAME` / `CHOLERA_API_KEY`; never write them into configs, scripts, logs or commits.
- New decisions and dated change history go to the knowledge vault (a `decision` note or a `record`), never into this file.

## Project Overview

**OutbreakExtractR** (`HopkinsIDD/OutbreakExtractR`) is an R package that extracts cholera outbreaks from (sub-)weekly suspected-case surveillance data pulled from the **Cholera Taxonomy** database (HopkinsIDD), using a configurable operational outbreak definition, and fires early-warning alerts on the same series. It implements the extraction of Zheng et al. 2022.

On top of the package, the `analysis/` layer parallelises the pipeline over **countries × 4-month time windows** on Yggdrasil (University of Geneva HPC) and produces the outbreak dataset consumed by the VIMC modelling repo `cholera_climate_outbreaks_africa` (ingested by its `analysis/00_ingest_outbreakextractr.R`; vintage `outbreak_data_Aug_2026`).

**Deployed outbreak definition** (`analysis/config_defaults.yml`): threshold = location's mean weekly incidence rate over the zero-filled series; start = `consecutive`, 2 weeks above threshold; end = the last week before a closing run of `tail_period` = 6 consecutive below-threshold weeks, the run itself excluded (`exclude_closing_tail`; vintages up to `outbreak_data_Aug_2026` included it); outbreaks with < 50 total cases dropped, counted without the closing run (`filter_outbreaks_by_size`, `cumulative_min_cases: 50`); non-outbreak locations kept (`keep_nonoutbreak_locations`).

## Package pipeline (`R/`)

Five sequential layers. Location periods (LPs) are the spatial unit; time periods are always `TL` (start) / `TR` (end) date pairs.

1. **Ingestion & cleaning** — `clean_psql_data()` (raw export → types, spatial/temporal scale, primary flag), `clean_location_names()` (`::` hierarchy), `observation_filter()` (dates, WHO region, scale, minimum cases).
2. **Normalisation** — `observation_aggregator()` (daily → weekly), `fill_missing_lps()`, `average_duplicate_observations()`, `set_uniform_wday_start()`, `fill_phantom_zeroes()` (±8 weeks of zero padding).
3. **Population** — `add_population()` (WorldPop constrained 100 m rasters, cached in `analysis/worldpop/`; national totals from `WPP2024`), `get_country_boundary()`, `build_composite_locations()` + `resolve_composite_children()` (composite `|`-joined locations have no API geometry; rebuilt from atomic children, else the parent polygon), `detect_duplicate_geometries()`, `validate_population()` (gates; `pop` is NA, never 0).
4. **Outbreak detection** — `identify_outbreaks()` calls `get_outbreak_threshold()` → `identify_epidemic_start()` (`consecutive` | `dual_window`) → `identify_epidemic_tail()`, then numbers each outbreak itself from its start week to the last week before its closing run (`exclude_closing_tail = TRUE`) or through the end of that run (`FALSE`, the package default), then applies the internal `filter_small_outbreaks()`. `identify_consecutive_outbreak_data()`, `identify_epidemic_period()` and `identify_washout_period()` are exported but have no caller in `R/` or `analysis/`. Post-hoc check: `verify_outbreak_definitions()`.
5. **Alerts & post-processing** — `trigger_alert()` (17 alert types: trend, case-count, cumulative families), `format_alerts()`, `label_*()`, `create_alert_groups*()`, `extract_*_outcomes*()`. `_obs` suffix = operates on observed data; no suffix = modelled/threshold data.

## HPC analysis layer (`analysis/`)

Config-driven, same pattern as `mpox-uvira-sprint`. Taxdat location string: `"CT-World::{WHO_REGION}::{ISO3}"`.

| Stage | Script | SLURM driver | Unit | Output (`analysis/generated_data/`) |
|---|---|---|---|---|
| 0 | `00_make_configs.R` | — | — | `configs/pull_set/` (3,060), `configs/detection_set/` (68), `test_pull/`, `test_detection/` |
| 1 | `01_pull_data.R` | `bash/submit_01_pull_data.sh` (`--array=0-3059%25`) | country × 4-month window | `raw_api_cache_*.rds`, `stage1_geo_*`, `stage1_flat_*` |
| 2 | `02_run_outbreak_detection.R` | `bash/submit_02_detection.sh` (`--array=0-67%10`) | country (full series) | `stage2_{region}_{iso3}.{rds,parquet}` |
| 3 | `03_aggregate_results.R` | — (local) | all countries | `combined_outbreaks_cholera.csv` |

- `utils.R` — `get_default_config()`, `write_configs()`, `make_options_from_config()`, `make_taxdat_location()`, `make_run_id()`, filename builders (`make_stage1_*_filename()`, `make_stage2_filename()`), I/O (`write_tabular()` / `read_tabular()` / `write_spatial()`).
- `01_pull_data.R` patches `taxdat` at load time with `assignInNamespace` (`flatten_json_result`, `read_taxonomy_data_api`).
- `parse_pull_logs.R` / `parse_detect_logs.R` — SLURM log triage → `*_log_summary*.csv`.
- `scratch_*.R`, `scratch_*.Rmd` — exploration only; nothing numbered calls them.
- `data_issue_*.md`, `pull_set_errors_*.md` — legacy dated issue notes (new ones go to the vault).
- `shiny/` — Cholera Outbreak Explorer (`app.R`, `data_prep.R`; deployed to shinyapps.io). `notebooks/01_descriptive_analysis.Rmd` — descriptive report.

## Data sources and key dates

- **Cases**: Cholera Taxonomy API (`api_website` in `config_defaults.yml`) via `taxdat::pull_taxonomy_data(source = "api")`; suspected cases, `sCh` (weekly) / `cCh`. Window 2010-01-01 – 2024-12-31; TZA has no data after 2019. 68 detection units (compound ISO3 such as `TZA::Mainland`, `TZA::Zanzibar` count separately); ~44 countries carry data, across AFR, AMR, EMR, SEAR.
- **Population**: WorldPop constrained 100 m rasters (R2025A, 2015–2030; downloaded on first use), `WPP2022` / `WPP2024` national totals, `endemic_locs_gte50nz_3ydata` (bundled in `data/`).
- **Boundaries**: LP geometry from the API; country boundary from `rgeoboundaries`, falling back to the union of LP geometries.

## Conventions

- Location strings use `::` as hierarchy delimiter (`country::admin1::admin2::admin3::admin4`); `custom_paste()` concatenates safely. Composite locations join members with `|`.
- Spatial scales: `country`, `admin1`–`admin4` (`admin4+`). Temporal scales: `daily`, `multiday`, `weekly`, `multiweek`, `monthly`, `multimonth`, `yearly`, `multiyear`. WHO regions: `AFR`, `EMR`, `AMR`, `SEAR`, `EUR`, `WPR`.
- Threshold types are the literal strings `"fixed threshold"`, `"mean weekly incidence rate"`, `"outbreak_dependent threshold"`, `"time_restricted threshold"` (note: `R/get_outbreak_threshold.R:81` tests `"outbreak-dependent threshold"`, with a hyphen).
- R 4.1+ lambda syntax (`\(x)`), magrittr `%>%`; rolling windows via `slider`, RLE grouping via `zoo`, per-location processing via `purrr::map`.
- Tests: testthat edition 3, `tests/testthat/`, CSV fixtures `outbreak_testing_data.csv` / `clean_outbreak_testing_data.csv`.

## Entry-point commands

```bash
Rscript -e 'devtools::document()'                                   # regenerate man/ + NAMESPACE
Rscript -e 'install.packages(".", repos = NULL, type = "source")'   # reinstall (analysis/ calls OutbreakExtractR::)
Rscript -e 'devtools::test()'                                       # all tests
Rscript -e 'testthat::test_file("tests/testthat/test-identify_outbreaks.R")'
Rscript -e 'devtools::check()'
Rscript analysis/00_make_configs.R                                  # regenerate configs; prints --array bounds
Rscript analysis/01_pull_data.R -c analysis/configs/test_pull/test_pull_1.yml             # Stage 1 dry run
Rscript analysis/02_run_outbreak_detection.R -c analysis/configs/test_detection/test_detection_1.yml
SLURM_ARRAY_TASK_ID=0 bash analysis/bash/submit_01_pull_data.sh     # SLURM script dry run, submits nothing
BATCH1=$(sbatch --parsable analysis/bash/submit_01_pull_data.sh)    # ASK FIRST
sbatch --dependency=afterok:$BATCH1 analysis/bash/submit_02_detection.sh   # ASK FIRST
Rscript analysis/03_aggregate_results.R
shiny::runApp("shiny")                                              # from R, project root
```

## Rules

1. `NAMESPACE` and `man/` are generated by roxygen2; never edit them by hand.
2. `analysis/` calls the package as `OutbreakExtractR::`, not `load_all()`: after any change in `R/`, reinstall locally **and on Yggdrasil** before running a stage.
3. `NAMESPACE` imports only magrittr: every other call in `R/` is namespace-prefixed (`dplyr::`, `sf::`, …).
4. Population is NA, never 0 (`pop == 0` gives `sCh/pop = Inf` and flips the threshold class to "high").
5. `make_options_from_config()` does not merge `config_defaults.yml` at runtime: every config carries every key. A new key goes into `config_defaults.yml`, is regenerated into all configs by `00_make_configs.R`, and is read with an `isTRUE()` / `%||%` guard for older configs.
6. Output format defaults to GeoJSON + RDS; GeoParquet / Parquet only with `use_geoparquet: true`.
7. A SLURM array index is the 0-based position in `ls <config dir> | sort -V`, not the number in the file name. Set the `#SBATCH --array=` lines from `00_make_configs.R`'s printout.
8. Stage 1 reuses `raw_api_cache_*.rds` regardless of `--redo`: delete the cache to refetch. Population and composite changes need only Stage 2.
9. Compound ISO3 codes (`TZA::Mainland`) are reduced to the 3-letter code before WorldPop, WPP or boundary lookups.
10. Composite locations get geometry only by reconstruction (children, else parent); the API never returns it.
11. `shiny/data/`, `shiny/manifest.json`, `shiny/rsconnect/` are never committed (purged from history).
12. `analysis/bash/set_taxonomy_api_key.sh` is untracked; recreate it by hand on a fresh checkout.
13. Ask before any HPC submission; never read `analysis/generated_data/` or `shiny/data/`.

The change behind each rule is a vault `record` tagged `rule`.

## Branches and state

`main` is the package; `parallel_refactor` carries the `analysis/` layer, the population/composite fixes and the Shiny app, and is **not merged to `main`**. Current state lives in the vault project index [[outbreakextractr]], not here.

## Decisions (knowledge vault)

- Project index: [[outbreakextractr]]
- Data: [[cholera-composite-location-periods]] · [[cholera-outbreakextractr-ingest-pipeline]] · [[cholera-population-pipeline]]
- Downstream consumer: [[cholera-climate-outbreaks-africa]]

Dated change history: `records/` in the vault (one note per change; Dataview table in the project index [[outbreakextractr]]).

## Open methodological questions

- Outbreak duration includes the six-week closing washout, so the minimum outbreak is eight weeks; is that the duration the downstream duration model should see?
- Composite locations whose children are never observed atomically (most of BDI) only get a parent-area population approximation.
- Case-definition heterogeneity across countries and LPs (suspected cases only).

## Key R packages

Package: `dplyr`, `purrr`, `tidyr`, `stringr`, `tibble`, `lubridate`, `zoo`, `slider`, `sf`, `raster`, `exactextractr`, `DBI`, `RPostgres`, `glue`, `curl`, `magrittr`. Analysis: `taxdat` (HopkinsIDD), `rgeoboundaries`, `yaml`, `optparse`, `here`, `furrr`, `future`, `arrow`, `sfarrow`, `digest`, `withr`; app: `shiny`.

## Context Navigation (Graphify)

No graphify graph is built for this repo yet. If one is built (`/graphify`), follow [[project-level-graphify-instructions]] and scope it to `R/` and `analysis/` via `.graphifyignore`.

## Session Management (Knowledge Vault)

Session save/resume commands and cross-project workflow conventions live in the **knowledge vault** at `../claude_code_vault` (absolute path `/home/perez/projects/claude_code_vault`). See `claude_code_vault/CLAUDE.md`, notably `/save` and `/resume`, the note-type taxonomy ([[workflow/note-type-taxonomy]]), and the chat-import pipeline ([[chat-import-pipeline]]).
