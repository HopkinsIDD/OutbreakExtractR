gate_row <- function(qc, g) qc[as.character(qc$gate) == as.character(g), , drop = FALSE]

clean_lp <- function() {
  data.frame(
    location_period_id = as.character(1:4),
    location           = c("AFR::ZZZ::A", "AFR::ZZZ::B", "AFR::ZZZ::C", "AFR::ZZZ::D"),
    pop                = c(1000, 2000, 3000, 4000),
    pop_source         = rep("worldpop_constrained", 4L),
    pop_geom_dup_n     = rep(1L, 4L),
    pop_geom_dup_class = rep("unique", 4L),
    adj_factor         = rep(1.02, 4L),
    pop_natl_ref       = rep(10000, 4L),
    stringsAsFactors   = FALSE
  )
}

testthat::test_that("validate_population passes every enforced gate on clean input", {
  testthat::expect_silent(qc <- validate_population(clean_lp(), "ZZZ"))

  enforced <- qc[!qc$record_only, , drop = FALSE]
  testthat::expect_true(all(enforced$passed))
  testthat::expect_true(all(is.na(enforced$detail)))
  # Gates 1, 2, 3, 4, 5a, 5b, 6 are enforced; 7 and 8 are record-only.
  testthat::expect_setequal(as.character(enforced$gate),
                            c("1", "2", "3", "4", "5a", "5b", "6"))
})

testthat::test_that("validate_population requires location_period_id and pop", {
  testthat::expect_error(
    validate_population(data.frame(location_period_id = "1"), "ZZZ"),
    "location_period_id and pop"
  )
})

testthat::test_that("gate 1 fires on missing or non-finite pop", {
  lp <- clean_lp()
  lp$pop[2] <- NA_real_
  lp$pop[3] <- Inf
  testthat::expect_warning(qc <- validate_population(lp, "ZZZ"), "gate 1")

  g <- gate_row(qc, 1L)
  testthat::expect_false(g$passed)
  testthat::expect_equal(g$n_violations, 2L)
  testthat::expect_equal(g$n_checked, 4L)
})

testthat::test_that("gate 2 fires on a zero denominator", {
  lp <- clean_lp()
  lp$pop[1] <- 0
  testthat::expect_warning(qc <- validate_population(lp, "ZZZ"), "gate 2")

  testthat::expect_equal(gate_row(qc, 2L)$n_violations, 1L)
  testthat::expect_equal(gate_row(qc, 2L)$detail, "1")
  # A zero is present, not missing, so gate 1 is unaffected.
  testthat::expect_true(gate_row(qc, 1L)$passed)
})

testthat::test_that("gate 3 fires outside [0.8, 1.5] and is skipped without adj_factor", {
  lp <- clean_lp()
  lp$adj_factor[4] <- 3.2
  testthat::expect_warning(qc <- validate_population(lp, "ZZZ"), "gate 3")
  testthat::expect_equal(gate_row(qc, 3L)$n_violations, 1L)

  lp2 <- clean_lp()
  lp2$adj_factor <- NULL
  qc2 <- validate_population(lp2, "ZZZ")
  testthat::expect_equal(nrow(gate_row(qc2, 3L)), 0L)
})

testthat::test_that("gates 4, 5a and 5b split the duplicate-geometry classes", {
  lp <- clean_lp()
  lp$pop_geom_dup_n     <- c(2L, 2L, 3L, 1L)
  lp$pop_geom_dup_class <- c("alias", "alias", "parent_inherited", "unique")
  testthat::expect_warning(qc <- validate_population(lp, "ZZZ"))

  testthat::expect_equal(gate_row(qc, 4L)$n_violations, 3L)   # broad net
  testthat::expect_equal(gate_row(qc, "5a")$n_violations, 1L) # parent inheritance
  testthat::expect_equal(gate_row(qc, "5b")$n_violations, 0L) # no cross-unit here
  testthat::expect_true(gate_row(qc, "5b")$passed)
})

testthat::test_that("gate 5b catches same-depth cross-unit collisions that 5a misses", {
  lp <- clean_lp()
  lp$pop_geom_dup_n     <- c(2L, 2L, 1L, 1L)
  lp$pop_geom_dup_class <- c("cross_unit", "cross_unit", "unique", "unique")
  testthat::expect_warning(qc <- validate_population(lp, "ZZZ"), "gate 5b")

  testthat::expect_equal(gate_row(qc, "5a")$n_violations, 0L)
  testthat::expect_equal(gate_row(qc, "5b")$n_violations, 2L)
})

testthat::test_that("gate 6 fires when an LP exceeds the national total", {
  lp <- clean_lp()
  lp$pop[4] <- 50000
  testthat::expect_warning(qc <- validate_population(lp, "ZZZ", wpp_total = 10000),
                                  "gate 6")
  testthat::expect_equal(gate_row(qc, 6L)$n_violations, 1L)
  testthat::expect_match(gate_row(qc, 6L)$description, "10,000")
})

testthat::test_that("gate 6 falls back to the median pop_natl_ref when wpp_total is NULL", {
  lp <- clean_lp()
  lp$pop[4] <- 50000
  testthat::expect_warning(qc <- validate_population(lp, "ZZZ"), "gate 6")
  testthat::expect_equal(gate_row(qc, 6L)$n_violations, 1L)
})

testthat::test_that("gate 6 compares each LP to its own pop_natl_ref, not a corpus-wide median", {
  # Regression test for the BDI smoke-test finding: a country's national
  # total grows over a multi-year extraction window, so an LP assigned a
  # later year (higher pop_natl_ref) must not be flagged against an earlier
  # year's median just because other LPs in the same country were assigned
  # earlier years. Three LPs simulate three assignment years with growing
  # national totals; the fourth LP's pop is deliberately just below its OWN
  # year's reference but above the median of the other three -- the old
  # median-based gate 6 would have false-positived on it.
  lp <- clean_lp()
  lp$pop_natl_ref <- c(10000, 11000, 12000, 12600)
  lp$pop          <- c(1000, 2000, 3000, 12400)   # LP 4: below its own ref (12600)...
  # ...but above median(pop_natl_ref) = 11500, which the old implementation
  # used as a single scalar for every row.
  testthat::expect_silent(qc <- validate_population(lp, "ZZZ"))

  g6 <- gate_row(qc, 6L)
  testthat::expect_true(g6$passed)
  testthat::expect_equal(g6$n_violations, 0L)
  testthat::expect_match(g6$description, "own year-specific")
})

testthat::test_that("gate 6 still fires when an LP exceeds its OWN pop_natl_ref", {
  lp <- clean_lp()
  lp$pop_natl_ref <- c(10000, 11000, 12000, 12600)
  lp$pop          <- c(1000, 2000, 3000, 13000)   # LP 4: above its own ref (12600)
  testthat::expect_warning(qc <- validate_population(lp, "ZZZ"), "gate 6")

  g6 <- gate_row(qc, 6L)
  testthat::expect_false(g6$passed)
  testthat::expect_equal(g6$n_violations, 1L)
  testthat::expect_equal(g6$detail, "4")
})

testthat::test_that("gates 7 and 8 are record-only and never fail the run", {
  lp <- clean_lp()          # sum = 10000 vs national 10000 -> ratio 1.0
  lp$pop <- c(10, 10, 10, 10)
  lp$pop_source <- c("worldpop_constrained", "none", "parent_fallback", "child_sum")

  qc <- validate_population(lp, "ZZZ", wpp_total = 1e6)

  g7 <- gate_row(qc, "7")
  testthat::expect_true(g7$record_only)
  testthat::expect_true(g7$passed)
  testthat::expect_equal(g7$n_violations, 1L)   # ratio far below 0.7, recorded only
  testthat::expect_match(g7$detail, "ratio=")

  g8 <- gate_row(qc, "8")
  testthat::expect_true(g8$record_only)
  testthat::expect_equal(g8$n_violations, 1L)   # one parent_fallback
  testthat::expect_match(g8$detail, "worldpop_constrained=1")
})

testthat::test_that("validate_population counts each LP once even on a weekly frame", {
  lp <- clean_lp()
  lp$pop[1] <- 0
  weekly <- lp[rep(seq_len(nrow(lp)), each = 52L), , drop = FALSE]

  testthat::expect_warning(qc <- validate_population(weekly, "ZZZ"), "gate 2")
  testthat::expect_equal(gate_row(qc, 2L)$n_checked, 4L)
  testthat::expect_equal(gate_row(qc, 2L)$n_violations, 1L)
})

testthat::test_that("validate_population aborts instead of warning when asked", {
  lp <- clean_lp()
  lp$pop[1] <- 0
  testthat::expect_error(validate_population(lp, "ZZZ", on_fail = "abort"), "gate 2")
})

testthat::test_that("validate_population records the uppercased iso3", {
  qc <- validate_population(clean_lp(), "zzz")
  testthat::expect_true(all(qc$iso3 == "ZZZ"))
})
