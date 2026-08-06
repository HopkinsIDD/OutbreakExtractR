#' @export
#' @title validate_population
#' @name validate_population
#' @description Run the population quality gates over one country's
#'   location-period populations and return a tidy per-gate report.
#'
#'   The gates encode failure modes that were verified against the extraction
#'   corpus rather than inferred from reading code, so their thresholds are
#'   calibrated to observed behaviour. Gate 3's band, for instance, is loose
#'   because the observed adjustment factors span only 1.005-1.034 — anything
#'   far outside that indicates the raster total was extracted on the wrong
#'   polygon, not a genuine disagreement with WPP.
#'
#'   Default \code{on_fail = "warn"}. Gate 4 currently fails corpus-wide (many
#'   location periods legitimately share geometry with an alias record), so
#'   aborting by default would make a full re-extraction impossible. Use
#'   \code{on_fail = "abort"} only for gates you have already driven to zero.
#'
#' @param lp_pop data.frame: one row per location period, with at least
#'   \code{location_period_id} and \code{pop}. Optional columns
#'   \code{location}, \code{pop_source}, \code{pop_geom_dup_n},
#'   \code{pop_geom_dup_class} and \code{adj_factor} enable additional gates.
#' @param iso3 character: ISO3 country code, recorded in the report.
#' @param on_fail character: "warn" (default) or "abort".
#' @param wpp_total numeric or NULL: the WPP2024 national total to compare
#'   against. When NULL it is looked up from the bundled WPP2024 table using
#'   the median \code{pop_year_raster} in \code{lp_pop}.
#' @return a tibble with one row per gate: \code{iso3}, \code{gate},
#'   \code{description}, \code{n_violations}, \code{n_checked}, \code{passed},
#'   and \code{detail} (a compact listing of offending location periods).
validate_population <- function(lp_pop, iso3, on_fail = c("warn", "abort"),
                                wpp_total = NULL) {

  on_fail <- match.arg(on_fail)
  iso3    <- toupper(iso3)

  if (!"location_period_id" %in% names(lp_pop) || !"pop" %in% names(lp_pop)) {
    stop("validate_population(): lp_pop must have location_period_id and pop columns.")
  }

  # One row per location period. Population is a static per-LP attribute, so a
  # weekly frame would otherwise inflate every violation count by the number of
  # weeks observed.
  lp <- lp_pop %>%
    dplyr::filter(!is.na(location_period_id)) %>%
    dplyr::group_by(location_period_id) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()

  has <- function(col) col %in% names(lp)
  report <- list()

  add_gate <- function(gate, description, violations, n_checked,
                       record_only = FALSE) {
    ids <- as.character(violations)
    report[[length(report) + 1L]] <<- dplyr::tibble(
      iso3         = iso3,
      # Character throughout: gates 5a/5b/7/8 are not integers, and a mixed
      # integer/character column cannot be bound into one report.
      gate         = as.character(gate),
      description  = description,
      n_violations = length(ids),
      n_checked    = n_checked,
      passed       = record_only | length(ids) == 0L,
      record_only  = record_only,
      detail       = if (length(ids) == 0L) NA_character_ else
        paste(utils::head(ids, 20L), collapse = ", ")
    )
  }

  # -- Gate 1: pop is finite and non-NA --------------------------------------
  # NA is the correct representation of "unknown"; this gate simply counts how
  # much of the country lacks a denominator.
  g1 <- lp$location_period_id[is.na(lp$pop) | !is.finite(lp$pop)]
  add_gate(1L, "pop is present and finite", g1, nrow(lp))

  # -- Gate 2: pop is never zero ---------------------------------------------
  # A zero denominator is worse than a missing one: get_outbreak_threshold()
  # sends is.na(pop) to the "low" surveillance class, but pop == 0 yields
  # sCh/pop == Inf, which classifies as "high". Zero silently flips the
  # detection threshold, so it must never be emitted.
  g2 <- lp$location_period_id[!is.na(lp$pop) & lp$pop <= 0]
  add_gate(2L, "pop is never zero or negative (zero flips the detection threshold)",
           g2, nrow(lp))

  # -- Gate 3: adjustment factor within a plausible band ---------------------
  if (has("adj_factor")) {
    g3 <- lp$location_period_id[!is.na(lp$adj_factor) &
                                  (lp$adj_factor < 0.8 | lp$adj_factor > 1.5)]
    add_gate(3L, "adj_factor within [0.8, 1.5] (observed corpus range 1.005-1.034)",
             g3, sum(!is.na(lp$adj_factor)))
  }

  # -- Gate 4: no two LPs share an identical geometry ------------------------
  # Broad net. Includes benign alias records, so this is expected to be
  # non-zero; gates 5a/5b isolate the harmful subsets.
  if (has("pop_geom_dup_n")) {
    g4 <- lp$location_period_id[!is.na(lp$pop_geom_dup_n) & lp$pop_geom_dup_n > 1L]
    add_gate(4L, "no two location periods share an identical geometry",
             g4, nrow(lp))
  }

  # -- Gates 5a / 5b: the harmful duplicate classes --------------------------
  # Splitting these matters. A single "duplicate pop spanning >1 admin depth"
  # gate catches only parent inheritance and misses same-depth cross-unit
  # collisions entirely — and in the audited corpus the cross-unit class was
  # the larger of the two.
  if (has("pop_geom_dup_class")) {
    g5a <- lp$location_period_id[lp$pop_geom_dup_class %in% "parent_inherited"]
    add_gate("5a", "no child location period carries its parent's geometry",
             g5a, nrow(lp))

    g5b <- lp$location_period_id[lp$pop_geom_dup_class %in% "cross_unit"]
    add_gate("5b", "no two distinct units at the same depth share one geometry",
             g5b, nrow(lp))
  }

  # -- Gate 6: no LP exceeds the national total ------------------------------
  natl <- wpp_total
  if (is.null(natl) && has("pop_natl_ref")) {
    natl <- suppressWarnings(stats::median(lp$pop_natl_ref, na.rm = TRUE))
  }
  if (!is.null(natl) && is.finite(natl) && natl > 0) {
    g6 <- lp$location_period_id[!is.na(lp$pop) & lp$pop > natl]
    add_gate(6L, sprintf("no location period exceeds the national total (%s)",
                         format(round(natl), big.mark = ",")),
             g6, nrow(lp))
  }

  # -- Gate 7: country total within 30% of WPP (record only) -----------------
  # Record-only: the frame covers surveilled areas, not the whole country, so a
  # shortfall is expected and is not by itself evidence of a defect.
  if (!is.null(natl) && is.finite(natl) && natl > 0) {
    observed <- sum(lp$pop, na.rm = TRUE)
    ratio    <- observed / natl
    report[[length(report) + 1L]] <- dplyr::tibble(
      iso3 = iso3, gate = "7",
      description = "country sum vs WPP2024 national total",
      n_violations = as.integer(!is.na(ratio) && (ratio < 0.7 || ratio > 1.3)),
      n_checked = nrow(lp), passed = TRUE, record_only = TRUE,
      detail = sprintf("sum=%s national=%s ratio=%.3f",
                       format(round(observed), big.mark = ","),
                       format(round(natl), big.mark = ","), ratio)
    )
  }

  # -- Gate 8: pop_source composition (record only) --------------------------
  if (has("pop_source")) {
    tab <- table(lp$pop_source, useNA = "ifany")
    report[[length(report) + 1L]] <- dplyr::tibble(
      iso3 = iso3, gate = "8", description = "population source composition",
      n_violations = sum(lp$pop_source %in% "parent_fallback"),
      n_checked = nrow(lp), passed = TRUE, record_only = TRUE,
      detail = paste(names(tab), as.integer(tab), sep = "=", collapse = ", ")
    )
  }

  out <- dplyr::bind_rows(report)

  failed <- out[!out$passed & !out$record_only, , drop = FALSE]
  if (nrow(failed) > 0L) {
    msg <- paste0(
      "validate_population(): ", nrow(failed), " gate(s) failed for ", iso3, ":\n",
      paste0("  gate ", failed$gate, ": ", failed$description,
             " — ", failed$n_violations, "/", failed$n_checked,
             " violation(s) [", failed$detail, "]", collapse = "\n")
    )
    if (identical(on_fail, "abort")) stop(msg) else warning(msg, call. = FALSE)
  }

  out
}
