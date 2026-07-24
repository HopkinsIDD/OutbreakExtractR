testthat::test_that("match_children_to_lps strips ' Sanitary District' and keeps one LP per child", {
  child_tbl <- data.frame(
    composite_name = "AFR::BDI::Cankuzo::Cankuzo|Murore",
    location = c("AFR::BDI::Cankuzo::Cankuzo", "AFR::BDI::Cankuzo::Murore"),
    stringsAsFactors = FALSE
  )
  loc_lookup <- data.frame(
    location = c("AFR::BDI::Cankuzo::Cankuzo Sanitary District",
                 "AFR::BDI::Cankuzo::Murore Sanitary District"),
    location_period_id = c("15468", "15469"),
    pop = c(100, 200),
    stringsAsFactors = FALSE
  )
  res <- OutbreakExtractR:::match_children_to_lps(child_tbl, loc_lookup)

  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_equal(
    res$location_period_id[res$location == "AFR::BDI::Cankuzo::Cankuzo"], "15468")
  testthat::expect_equal(
    res$location_period_id[res$location == "AFR::BDI::Cankuzo::Murore"], "15469")
  testthat::expect_equal(
    res$pop[res$location == "AFR::BDI::Cankuzo::Cankuzo"], 100)
})

testthat::test_that("match_children_to_lps prefers an exact match over the normalized fallback", {
  child_tbl <- data.frame(composite_name = "c", location = "AFR::X::Foo",
                          stringsAsFactors = FALSE)
  loc_lookup <- data.frame(
    location = c("AFR::X::Foo", "AFR::X::Foo Sanitary District"),
    location_period_id = c("1", "2"),
    pop = c(9, 9),
    stringsAsFactors = FALSE
  )
  res <- OutbreakExtractR:::match_children_to_lps(child_tbl, loc_lookup)
  testthat::expect_equal(res$location_period_id, "1")
})

testthat::test_that("match_children_to_lps resolves one child to a single LP (prefer non-NA pop, lowest id)", {
  child_tbl <- data.frame(composite_name = "c", location = "AFR::X::Bar",
                          stringsAsFactors = FALSE)
  loc_lookup <- data.frame(
    location = rep("AFR::X::Bar Sanitary District", 3L),
    location_period_id = c("30", "20", "25"),
    pop = c(NA, 5, 5),
    stringsAsFactors = FALSE
  )
  res <- OutbreakExtractR:::match_children_to_lps(child_tbl, loc_lookup)
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_equal(res$location_period_id, "20")
})

testthat::test_that("match_children_to_lps returns NA LP for unmatched children", {
  child_tbl <- data.frame(composite_name = "c", location = "AFR::X::Ghost",
                          stringsAsFactors = FALSE)
  loc_lookup <- data.frame(location = "AFR::X::Other", location_period_id = "1",
                           pop = 1, stringsAsFactors = FALSE)
  res <- OutbreakExtractR:::match_children_to_lps(child_tbl, loc_lookup)
  testthat::expect_true(is.na(res$location_period_id))
})

testthat::test_that("build_composite_locations recovers child geometry + pop via suffix normalization", {
  testthat::skip_if_not_installed("sf")

  p1 <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  p2 <- sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
  raw_sf <- sf::st_sf(
    location_period_id = c("100", "200"),
    geometry = sf::st_sfc(p1, p2),
    crs = 4326
  )

  normalized <- data.frame(
    location = c("AFR::ZZZ::Prov::Alpha Sanitary District",
                 "AFR::ZZZ::Prov::Beta Sanitary District",
                 "AFR::ZZZ::Prov::Alpha|Beta"),
    location_period_id = c("100", "200", NA),
    pop = c(1000, 2000, NA),
    spatial_scale = c("admin2", "admin2", "admin2"),
    stringsAsFactors = FALSE
  )

  res <- build_composite_locations(normalized, raw_sf, "ZZZ")

  comp_row <- res$data[grepl("composite_loc_ZZZ", res$data$location_period_id), ]
  testthat::expect_equal(nrow(comp_row), 1L)
  testthat::expect_equal(comp_row$pop, 3000)     # summed child pops, no double count

  testthat::expect_false(is.null(res$geometry))
  testthat::expect_equal(nrow(res$geometry), 1L) # union of the two child squares
})

testthat::test_that("build_composite_locations uses WorldPop-on-geometry pop when raster_dir given", {
  testthat::skip_if_not_installed("sf")

  p1 <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  p2 <- sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
  raw_sf <- sf::st_sf(
    location_period_id = c("100", "200"),
    geometry = sf::st_sfc(p1, p2),
    crs = 4326
  )

  normalized <- data.frame(
    location = c("AFR::ZZZ::Prov::Alpha Sanitary District",
                 "AFR::ZZZ::Prov::Beta Sanitary District",
                 "AFR::ZZZ::Prov::Alpha|Beta"),
    location_period_id = c("100", "200", NA),
    pop = c(1000, 2000, NA),
    spatial_scale = c("admin2", "admin2", "admin2"),
    TL = as.Date(c("2018-01-01", "2018-01-01", "2018-01-01")),
    stringsAsFactors = FALSE
  )

  # Offline: mock the raster extraction to return a fixed sub-area denominator.
  testthat::local_mocked_bindings(
    estimate_pop_for_geometries = function(geom_sf, country_iso3, year,
                                           raster_dir = "worldpop") {
      rep(5555, nrow(geom_sf))
    }
  )

  res <- build_composite_locations(normalized, raw_sf, "ZZZ",
                                   raster_dir = "ignored")

  comp_row <- res$data[grepl("composite_loc_ZZZ", res$data$location_period_id), ]
  testthat::expect_equal(nrow(comp_row), 1L)
  # Geometry-derived pop (5555) overrides the summed-child pop (3000).
  testthat::expect_equal(comp_row$pop, 5555)
})

testthat::test_that("build_composite_locations keeps summed-child pop when geometry pop is NA/0", {
  testthat::skip_if_not_installed("sf")

  p1 <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  p2 <- sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
  raw_sf <- sf::st_sf(
    location_period_id = c("100", "200"),
    geometry = sf::st_sfc(p1, p2),
    crs = 4326
  )
  normalized <- data.frame(
    location = c("AFR::ZZZ::Prov::Alpha Sanitary District",
                 "AFR::ZZZ::Prov::Beta Sanitary District",
                 "AFR::ZZZ::Prov::Alpha|Beta"),
    location_period_id = c("100", "200", NA),
    pop = c(1000, 2000, NA),
    spatial_scale = c("admin2", "admin2", "admin2"),
    TL = as.Date(c("2018-01-01", "2018-01-01", "2018-01-01")),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    estimate_pop_for_geometries = function(geom_sf, country_iso3, year,
                                           raster_dir = "worldpop") {
      rep(NA_real_, nrow(geom_sf))
    }
  )

  res <- build_composite_locations(normalized, raw_sf, "ZZZ",
                                   raster_dir = "ignored")
  comp_row <- res$data[grepl("composite_loc_ZZZ", res$data$location_period_id), ]
  testthat::expect_equal(comp_row$pop, 3000)   # falls back to summed child pop
})
