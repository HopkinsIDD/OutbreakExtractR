# Composite Location Geometry — Investigation State (2026-07-17)

Fleeting note. Originally paused after Step 0 of plan `resilient-nibbling-noodle`
(fix composite population via WorldPop lookup + keep non-outbreak locations).

> **Status update (2026-08-06).** Step A and the `identify_outbreaks()` half of
> Step B have since landed; the findings below are still accurate but the
> "Remaining plan steps" section at the bottom was stale and has been corrected.
> See that section for what is actually left.

## Problem recap

Colleague's May-2025 pre-outbreak extraction vs June-2026 refactor: missing rows, all
**composite locations** (`|`-joined names). Countries MRT/MDG/SEN vanish entirely.

Root cause (from prior session): composite `pop = 0` → NaN threshold → risk "low" →
no epidemic start → `identify_outbreaks()` returns empty frame → Stage 2 drops it.

## Step 0 finding — composites have NO API geometry (triple-confirmed)

Confirmed via (a) cached `raw_api_cache_AFR_BDI_*.rds`, (b) low-level live POST,
(c) the actual patched `taxdat::read_taxonomy_data_api` production path.

Live BDI pull, window 2014-01-02 → 2014-05-01:
- 2192 obs = 656 composite + 1536 atomic.
- **All 656 composites: `location_period_id = NULL`, empty POINT geometry.**
- Atomic: 1328 POLYGON + 32 MULTIPOLYGON + 176 empty POINT.

Why (raw JSON, pre-processing):
- Composite obs carry a real `attributes.location_id` (e.g. 29697) but
  `attributes.location_period_id = NULL`. Geometry (shape) hangs off a *location_period*,
  not a location_id → no shape returned.
- **The client code does NOT drop geometry** — the source response already lacks the link;
  `read_taxonomy_data_api`'s left-join on the missing id faithfully yields empty geometry.

Raw `location_periods` block has **no hidden geometry**: 43 LPs, 43 shapes,
all referenced by atomic obs, **0 orphan LPs, 0 orphan shapes**.

No API route resolves a shape from a `location_id` (probed
`locations/{id}`, `locations/{id}/location_periods`, `location_periods?location_id=`,
`shapes?location_id=`, `location_periods/by_location` → all 404/500).

Only remaining way to know if the DB itself stores a composite shape: **direct-SQL path**
(`taxdat::build_geoms_query` / `get_unique_db_shps` / `read_taxonomy_locationperiods_sql`).
Not yet attempted (needs DB creds).

## Why reconstruction is hard — vocabulary mismatch

BDI pull = 91 unique locations (50 atomic + 41 composite). Atomic and composite units live in
**different admin vocabularies**:
- Atomic = health-system **"Sanitary Districts"** (admin3) under **province-name** admin2,
  e.g. `AFR::BDI::Ruyigi::Butezi Sanitary District`.
- Composite children = **communes**; many composites use **ISO province codes** at admin2,
  e.g. `AFR::BDI::BI-CA::(Cankuzo|Cendajuru|Kigamba)`.

So `build_composite_locations()` exact-match child lookup mostly fails
(36/47 composites → no child matched; only 5 all-children, 6 partial). Root names DO overlap
(Butezi, Bubanza, Cankuzo…) but exact equality breaks on:
1. `" Sanitary District"` suffix, and
2. `BI-XX` code vs province name at admin2.
Even normalized, communes ≠ sanitary districts (different partitions of a province) — not a
full 1:1.

## Current pipeline behaviour (existing `stage1_geo_AFR_BDI_composite.geojson`)

Only 6 composites currently get any geometry, all via **parent-polygon fallback** (areas come
in identical pairs = shared parent province). The rest get summed-child / parent-pop / 0.

## Open decision (RESOLVED)

WorldPop-on-geometry gives a **true sub-area denominator only for ~1/4 of BDI composites**;
the rest fall back to the parent area. Options that were on the table:
- (a) Direct-SQL DB check on composite `location_id`s for a server-side shape.
- (b) Proceed with child-union / parent-fallback reconstruction as planned.
- (c) Improve child-name matching first (strip "Sanitary District", map `BI-XX` →
  province name) to recover more real child-unions.

**Chosen: (c) then (b).** `match_children_to_lps()` now strips the
" Sanitary District" suffix and resolves each child to a single LP, and
`build_composite_locations()` reconstructs geometry from the recovered children
with a parent-polygon fallback.

The premise of the "pop > 0 → fixes the drop bug" argument has since been
**retracted**. Substituting the parent-area population to avoid `pop = 0` trades
one defect for another: the parent is a strictly larger area, so the denominator
is wrong in a known direction. The population fix (2026-08-06) makes NA — not a
parent approximation and not 0 — the honest value, gated behind
`allow_parent_pop_fallback` (default FALSE). Note that `pop = 0` was never merely
"missing": `get_outbreak_threshold()` sends `is.na(pop)` to the "low" surveillance
class, but `pop == 0` gives `sCh/pop == Inf`, which classifies as "high" — a zero
denominator *flips* the threshold.

## Remaining plan steps

Done:
- ~~Step A~~ — `raster_dir` param, `estimate_pop_for_geometries()`, geometry-derived
  pop as the primary source, header comment corrected. Wired in
  `analysis/02_run_outbreak_detection.R:250`.
- ~~`keep_nonoutbreak_locations` param + non-empty else branch~~ —
  `R/identify_outbreaks.R:50,153`.
- ~~Docs~~ — `roxygen2::roxygenise()` run 2026-08-06.

Still open:
- Wire `keep_nonoutbreak_locations = TRUE` into
  `analysis/02_run_outbreak_detection.R` (the parameter exists and defaults to
  FALSE for backward compatibility, but the analysis layer does not yet pass it,
  so Stage 2 still drops locations with no detected outbreak).
- Add a testthat case for `keep_nonoutbreak_locations` in
  `tests/testthat/test-identify_outbreaks.R` — there is none.
- Stage 2 dry-run on BDI.

Test scripts: `/tmp/api_composite_test.R`, `/tmp/api_taxdat_test.R`,
`/tmp/raw_composite_dump.R`, `/tmp/probe_raw_shapes.R`, `/tmp/list_bdi_locations.R`
(raw response cached at `/tmp/bdi_live_raw.rds`).
