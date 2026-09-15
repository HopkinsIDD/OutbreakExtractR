testthat::test_that("get_country_boundary returns NULL rather than a union when nothing resolves", {
  testthat::skip_if_not_installed("sf")

  # A union of LP geometries is NOT a country boundary: it covers only the
  # surveilled sub-areas, so country_raw is understated and adj_factor inflated.
  # The contract is that an unresolvable boundary yields NULL ("skip the
  # adjustment"), never an approximation.
  empty_cache <- withr::local_tempdir()

  testthat::expect_message(res <- get_country_boundary("ZZZ", cache_dir = empty_cache))
  testthat::expect_null(res)
})

testthat::test_that("get_country_boundary returns NULL for an unparseable code", {
  testthat::expect_message(res <- get_country_boundary("12", cache_dir = NULL),
                           "could not extract an ISO3 code")
  testthat::expect_null(res)
})

testthat::test_that("get_country_boundary reads the on-disk cache without fetching", {
  testthat::skip_if_not_installed("sf")

  cache <- withr::local_tempdir()
  poly  <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      crs = 4326
    )
  )
  sf::st_write(poly, file.path(cache, "ZZZ_adm0.geojson"), quiet = TRUE)

  res <- get_country_boundary("ZZZ", cache_dir = cache)

  testthat::expect_s3_class(res, "sf")
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_equal(sf::st_crs(res)$epsg, 4326L)
})

testthat::test_that("get_country_boundary tolerates a sub-national suffix", {
  testthat::skip_if_not_installed("sf")

  cache <- withr::local_tempdir()
  poly  <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      crs = 4326
    )
  )
  sf::st_write(poly, file.path(cache, "TZA_adm0.geojson"), quiet = TRUE)

  res <- get_country_boundary("TZA::Mainland", cache_dir = cache)
  testthat::expect_s3_class(res, "sf")
})

testthat::test_that("get_country_boundary refetches when the cache file is corrupt", {
  testthat::skip_if_not_installed("sf")

  cache <- withr::local_tempdir()
  writeLines("not geojson", file.path(cache, "ZZZ_adm0.geojson"))

  testthat::expect_message(res <- get_country_boundary("ZZZ", cache_dir = cache))
  testthat::expect_null(res)   # no rgeoboundaries fallback available offline
})

testthat::test_that("band_adj_factor accepts the plausible band untouched", {
  testthat::expect_equal(band_adj_factor(1.02), list(value = 1.02, flag = "ok"))
  testthat::expect_equal(band_adj_factor(0.67)$flag, "ok")
  testthat::expect_equal(band_adj_factor(1.5)$flag, "ok")
})

testthat::test_that("band_adj_factor accepts-and-flags the wide band", {
  testthat::expect_message(wide <- band_adj_factor(1.8), "accepted but flagged")
  testthat::expect_equal(wide$value, 1.8)
  testthat::expect_equal(wide$flag, "wide")

  testthat::expect_message(low <- band_adj_factor(0.55))
  testthat::expect_equal(low$flag, "wide")
})

testthat::test_that("band_adj_factor clamps implausible factors to 1.0", {
  # An extreme factor means the raster total was extracted on the wrong polygon,
  # not that the country disagrees with WPP. Leave the population unadjusted.
  testthat::expect_message(big <- band_adj_factor(12), "clamped to 1.0")
  testthat::expect_equal(big$value, 1.0)
  testthat::expect_equal(big$flag, "clamped")

  testthat::expect_message(small <- band_adj_factor(0.1))
  testthat::expect_equal(small$value, 1.0)
})

testthat::test_that("band_adj_factor clamps non-finite and non-positive factors", {
  for (bad in list(NA_real_, NaN, Inf, 0, -1)) {
    testthat::expect_equal(band_adj_factor(bad), list(value = 1.0, flag = "clamped"))
  }
})
