# Offline WorldPop stand-in: a 0.01-degree raster over lon [0,3] x lat [0,1].
# Each cell carries `value`, so a 1x1-degree square extracts to 10,000 * value.
# download_worldpop_constrained() is mocked to hand this file back, which keeps
# the real raster::raster() and exactextractr::exact_extract() code paths under
# test without any network access.
local_worldpop_stub <- function(value = 1, env = parent.frame()) {
  r <- raster::raster(xmn = 0, xmx = 3, ymn = 0, ymx = 1, res = 0.01,
                      crs = "+proj=longlat +datum=WGS84")
  raster::values(r) <- value
  path <- withr::local_tempfile(fileext = ".tif", .local_envir = env)
  raster::writeRaster(r, path, overwrite = TRUE)
  testthat::local_mocked_bindings(
    download_worldpop_constrained = function(...) path,
    .env = env
  )
  path
}

sq <- function(x0) {
  sf::st_polygon(list(rbind(c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1),
                            c(x0, 1), c(x0, 0))))
}

two_lp_fixture <- function(locations = c("AFR::ZZZ::A", "AFR::ZZZ::B"),
                           geoms = sf::st_sfc(sq(0), sq(1), crs = 4326),
                           year = 2018L) {
  raw_sf <- sf::st_sf(location_period_id = c("100", "200"), geometry = geoms)
  normalized <- data.frame(
    location           = rep(locations, each = 3L),
    location_period_id = rep(c("100", "200"), each = 3L),
    TL                 = as.Date(paste0(year, c("-01-01", "-02-01", "-03-01"))),
    stringsAsFactors   = FALSE
  )
  list(raw_sf = raw_sf, normalized = normalized)
}

testthat::test_that("add_population extracts pop and attaches the full provenance schema", {
  testthat::skip_if_not_installed("sf")
  local_worldpop_stub(value = 1)

  fx  <- two_lp_fixture()
  res <- suppressMessages(
    add_population(fx$normalized, fx$raw_sf, "ZZZ",
                   boundary_cache_dir = withr::local_tempdir()))

  testthat::expect_true(all(c("pop", "pop_source", "pop_geom_dup_n",
                              "pop_geom_dup_class", "pop_year_obs",
                              "pop_year_raster", "pop_natl_ref", "adj_factor",
                              "adj_factor_flag") %in% names(res)))
  testthat::expect_equal(nrow(res), nrow(fx$normalized))
  testthat::expect_equal(unique(res$pop), 10000, tolerance = 1e-6)
  testthat::expect_equal(unique(res$pop_source), "worldpop_constrained")
  testthat::expect_equal(unique(res$pop_geom_dup_n), 1L)
  testthat::expect_equal(unique(res$pop_geom_dup_class), "unique")
})

testthat::test_that("a missing rgeoboundaries leaves pop unadjusted rather than union-scaled", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if(requireNamespace("rgeoboundaries", quietly = TRUE),
                    "rgeoboundaries is installed; the no-boundary path is untestable here")
  local_worldpop_stub(value = 1)

  # COD is present in WPP2024, so tot_UN resolves and the *only* thing standing
  # between the code and an adjustment factor is the national boundary.
  fx  <- two_lp_fixture()
  res <- suppressMessages(
    add_population(fx$normalized, fx$raw_sf, "COD",
                   boundary_cache_dir = withr::local_tempdir()))

  # No boundary -> adj_factor 1.0 and flagged "unadjusted". The old LP-union
  # fallback would have understated country_raw and scaled every pop upward.
  testthat::expect_true(all(res$pop_natl_ref > 0))
  testthat::expect_equal(unique(res$adj_factor), 1.0)
  testthat::expect_equal(unique(res$adj_factor_flag), "unadjusted")
  testthat::expect_equal(unique(res$pop), 10000, tolerance = 1e-6)
})

testthat::test_that("add_population records the observed year and the clamped raster year", {
  testthat::skip_if_not_installed("sf")
  local_worldpop_stub(value = 1)

  fx  <- two_lp_fixture(year = 2010L)   # before the WorldPop constrained range
  res <- suppressMessages(
    add_population(fx$normalized, fx$raw_sf, "ZZZ",
                   boundary_cache_dir = withr::local_tempdir()))

  testthat::expect_equal(unique(res$pop_year_obs), 2010L)
  testthat::expect_equal(unique(res$pop_year_raster), 2015L)
  testthat::expect_true(all(res$pop_year_obs != res$pop_year_raster))
})

testthat::test_that("add_population flags LPs that share an identical geometry", {
  testthat::skip_if_not_installed("sf")
  local_worldpop_stub(value = 1)

  # Two LPs at different hierarchy depths carrying the same polygon: the child
  # is assigned the whole parent population.
  fx <- two_lp_fixture(
    locations = c("AFR::ZZZ::Conakry", "AFR::ZZZ::Conakry::Dixinn"),
    geoms     = sf::st_sfc(sq(0), sq(0), crs = 4326)
  )
  res <- suppressMessages(
    add_population(fx$normalized, fx$raw_sf, "ZZZ",
                   boundary_cache_dir = withr::local_tempdir()))

  testthat::expect_equal(unique(res$pop_geom_dup_n), 2L)
  testthat::expect_equal(unique(res$pop_geom_dup_class), "parent_inherited")
})

testthat::test_that("add_population emits NA, never 0, when WorldPop has no cells", {
  testthat::skip_if_not_installed("sf")
  local_worldpop_stub(value = 0)

  fx  <- two_lp_fixture()
  res <- suppressMessages(
    add_population(fx$normalized, fx$raw_sf, "ZZZ",
                   boundary_cache_dir = withr::local_tempdir()))

  # pop == 0 gives sCh/pop == Inf, which get_outbreak_threshold() classifies as
  # "high" — a zero denominator flips the threshold instead of being missing.
  testthat::expect_true(all(is.na(res$pop)))
  testthat::expect_equal(unique(res$pop_source), "none")
})

testthat::test_that("add_population sets pop = NA for LPs with no geometry", {
  testthat::skip_if_not_installed("sf")
  local_worldpop_stub(value = 1)

  fx <- two_lp_fixture()
  fx$normalized$location_period_id[fx$normalized$location_period_id == "200"] <- "999"

  res <- suppressMessages(
    add_population(fx$normalized, fx$raw_sf, "ZZZ",
                   boundary_cache_dir = withr::local_tempdir()))

  testthat::expect_true(all(is.na(res$pop[res$location_period_id == "999"])))
  testthat::expect_equal(unique(res$pop_source[res$location_period_id == "999"]),
                         "none")
  testthat::expect_false(any(is.na(res$pop[res$location_period_id == "100"])))
})

testthat::test_that("a failed raster download yields NA pop with the schema intact", {
  testthat::skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    download_worldpop_constrained = function(...) stop("no network")
  )

  fx  <- two_lp_fixture()
  res <- suppressMessages(
    add_population(fx$normalized, fx$raw_sf, "ZZZ",
                   boundary_cache_dir = withr::local_tempdir()))

  testthat::expect_true(all(is.na(res$pop)))
  testthat::expect_equal(unique(res$pop_source), "none")
  testthat::expect_equal(unique(res$pop_year_obs), 2018L)
  testthat::expect_equal(unique(res$pop_geom_dup_class), "unique")
})

testthat::test_that("add_population returns the provenance schema when no LP id is usable", {
  testthat::skip_if_not_installed("sf")

  fx <- two_lp_fixture()
  fx$normalized$location_period_id <- NA_character_

  testthat::expect_message(
    res <- add_population(fx$normalized, fx$raw_sf, "ZZZ",
                          boundary_cache_dir = withr::local_tempdir()),
    "no valid location_period_ids")

  testthat::expect_equal(nrow(res), nrow(fx$normalized))
  testthat::expect_true(all(is.na(res$pop)))
  testthat::expect_equal(unique(res$pop_source), "none")
  testthat::expect_equal(unique(res$pop_geom_dup_n), 1L)
  testthat::expect_true(all(is.na(res$pop_year_obs)))
})

testthat::test_that("add_population accepts the camelCase and lctn_pr geometry keys", {
  testthat::skip_if_not_installed("sf")
  local_worldpop_stub(value = 1)

  fx <- two_lp_fixture()
  for (key in c("locationPeriod_id", "lctn_pr")) {
    raw <- fx$raw_sf
    names(raw)[names(raw) == "location_period_id"] <- key
    res <- suppressMessages(
      add_population(fx$normalized, raw, "ZZZ",
                     boundary_cache_dir = withr::local_tempdir()))
    testthat::expect_equal(unique(res$pop), 10000, tolerance = 1e-6)
  }

  raw <- fx$raw_sf
  names(raw)[names(raw) == "location_period_id"] <- "nonsense"
  testthat::expect_error(add_population(fx$normalized, raw, "ZZZ"),
                         "location_period_id")
})

testthat::test_that("add_population overwrites stale provenance columns instead of suffixing them", {
  testthat::skip_if_not_installed("sf")
  local_worldpop_stub(value = 1)

  fx <- two_lp_fixture()
  fx$normalized$pop        <- 42
  fx$normalized$pop_source <- "stale"
  fx$normalized$adj_factor <- 99

  res <- suppressMessages(
    add_population(fx$normalized, fx$raw_sf, "ZZZ",
                   boundary_cache_dir = withr::local_tempdir()))

  testthat::expect_false(any(grepl("\\.x$|\\.y$", names(res))))
  testthat::expect_equal(unique(res$pop), 10000, tolerance = 1e-6)
  testthat::expect_equal(unique(res$pop_source), "worldpop_constrained")
})
