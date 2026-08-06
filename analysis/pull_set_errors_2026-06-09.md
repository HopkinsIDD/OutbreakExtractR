# Pull Set Error Analysis — 2026-06-09

Job array: `44806696` (315 tasks, `analysis/configs/pull_set/`)

## Summary

| Outcome | Count | % |
|---|---|---|
| Success | 149 | 47.3% |
| Failed | 166 | 52.7% |

---

## Error Groups

### 1. Missing column in `dplyr::rename()` — 103 jobs (62% of failures)

`analysis/01_pull_data.R` lines 165–174: the `rename()` call is unconditional — `any_of()` guards the preceding `select()` but not the rename. Columns absent from the API response cause the job to fail.

**Fix**: apply the cCh guard pattern (rename if present, else `mutate(col = NA)`) to all optional columns.

| Missing column | Count |
|---|---|
| `relationships.observation_collection.data.id` | 90 |
| `attributes.fields.deaths` | 7 |
| `attributes.location_period_id` | 5 |
| `attributes.fields.suspected_cases` | 1 |

Affected job IDs: 1, 4, 5, 6, 8, 9, 11, 12, 15, 16, 18, 19, 22, 25, 30, 32, 33, 40, 43, 44, 46, 47, 50, 51, 54, 57, 58, 60, 61, 64, 65, 67, 68, 71, 72, 74, 75, 78, 79, 82, 83, 85, 86, 88, 89, 90, 92, 93, 95, 96, 97, 99, 100, 102, 103, 104, 106, 109, 110, 114, 116, 117, 121, 123, 124, 131, 137, 180, 187, 194, 205, 207, 213, 214, 215, 220, 221, 222, 228, 229, 232, 233, 235, 236, 242, 243, 249, 250, 257, 263, 264, 266, 271, 273, 278, 280, 282, 284, 285, 286, 289, 292, 312

---

### 2. Inconsistent `start_weekday` — 27 jobs (16% of failures)

`Error: All observations should have the same start_weekday. Please run set_uniform_wday_start on this dataset.`

`set_uniform_wday_start()` is called in `01_pull_data.R` but the error fires downstream, suggesting some code path bypasses or re-introduces mixed weekdays after normalization.

Affected: 2, 13, 20, 81, 128, 135, 142, 144, 149, 151, 252, 287, 291, 293, 294, 298, 299, 300, 301, 303, 305, 306, 307, 308, 310, 313, 314

---

### 3. `purrr::map()` — missing value where TRUE/FALSE needed — 22 jobs (13% of failures)

Occurs in `add_population()` during `exactextractr::exact_extract()` on invalid or degenerate geometries. Likely a NULL/empty geometry row slipping through before the spatial join.

Affected: 0, 7, 14, 21, 35, 42, 49, 56, 63, 70, 77, 84, 91, 132, 139, 146, 153, 165, 168, 175, 189, 203

---

### 4. SSL/network timeout — 9 jobs (5% of failures)

`Error in curl::curl_fetch_memory(): Timeout was reached: [cholera-taxonomy.middle-distance.com] SSL connection timeout`

Transient. Safe to requeue.

Affected: 26, 27, 28, 29, 31, 36, 37, 38, 39

---

### 5. `dplyr::left_join()` — missing `location_period_id` — 3 jobs (2% of failures)

`"Join columns in y must be present in the data. Problem with location_period_id."`

Downstream join fails when `location_period_id` is NA throughout (from Error 1 unfixed, or when the column is absent). Investigate after Error 1 fix.

Affected: 41, 277, 296

---

### 6. `[readValues] cannot read values` — 2 jobs (1% of failures)

Two distinct root causes, both handled in `R/get_pop.R` / `R/add_population.R`.

**Job 24 (NGA):** Corrupted WorldPop download. Both R2024B and R2025A use identical LZW+PREDICTOR=2 compression (standard, readable by all GDAL versions). The "Using code not yet in table" / `TIFFReadEncodedTile` errors are produced by a truncated LZW stream — i.e., a partial download of the 150 MB NGA raster. Fix: `download_worldpop_constrained()` now reads one tile after download to verify the file is intact; if corrupt, it deletes the cached file and retries with the next release (R2024B). On a clean re-run the R2025A download will succeed and no fallback is needed.

**Job 53 (ZMB):** Single-observation LP had a POINT geometry (centroid coordinates only). `exactextractr::exact_extract()` only supports polygon geometries (`st_dimension == 2`). Fix: non-polygon geometries are now filtered out alongside empty geometries in `add_population()`, with pop set to NA.

Affected: 24, 53
