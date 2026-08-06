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

# ---------------------------------------------------------------------------
# Population precedence and the parent fallback gate
# ---------------------------------------------------------------------------

# A composite whose children are never observed atomically: only the parent
# admin unit ("Prov") carries an LP, a geometry and a population.
orphan_composite_fixture <- function() {
  parent_poly <- sf::st_polygon(
    list(rbind(c(0, 0), c(2, 0), c(2, 1), c(0, 1), c(0, 0))))
  raw_sf <- sf::st_sf(
    location_period_id = "900",
    geometry = sf::st_sfc(parent_poly),
    crs = 4326
  )
  normalized <- data.frame(
    location = c("AFR::ZZZ::Prov", "AFR::ZZZ::Prov::Alpha|Beta"),
    location_period_id = c("900", NA),
    pop = c(7000, NA),
    spatial_scale = c("admin1", "admin2"),
    TL = as.Date(c("2018-01-01", "2018-01-01")),
    pop_source = c("worldpop_constrained", NA),
    stringsAsFactors = FALSE
  )
  list(raw_sf = raw_sf, normalized = normalized)
}

testthat::test_that("a composite with no child pop is left NA by default, not given its parent's", {
  testthat::skip_if_not_installed("sf")
  fx <- orphan_composite_fixture()

  res <- suppressMessages(
    build_composite_locations(fx$normalized, fx$raw_sf, "ZZZ"))

  comp <- res$data[grepl("composite_loc_ZZZ", res$data$location_period_id), ]
  testthat::expect_equal(nrow(comp), 1L)
  # The parent is a strictly larger area, so inheriting 7000 would overstate the
  # denominator by however much of Prov the composite does not cover. NA routes
  # the composite to the "low" surveillance class instead.
  testthat::expect_true(is.na(comp$pop))
  testthat::expect_equal(comp$pop_source, "none")
})

testthat::test_that("allow_parent_pop_fallback = TRUE opts into the parent population", {
  testthat::skip_if_not_installed("sf")
  fx <- orphan_composite_fixture()

  res <- suppressMessages(
    build_composite_locations(fx$normalized, fx$raw_sf, "ZZZ",
                              allow_parent_pop_fallback = TRUE))

  comp <- res$data[grepl("composite_loc_ZZZ", res$data$location_period_id), ]
  testthat::expect_equal(comp$pop, 7000)
  testthat::expect_equal(comp$pop_source, "parent_fallback")
})

testthat::test_that("WorldPop run on a parent polygon is classified parent_fallback, not composite_union", {
  testthat::skip_if_not_installed("sf")
  fx <- orphan_composite_fixture()

  # The composite has no child geometry, so step 6b puts it on the PARENT
  # polygon. Extracting the raster there returns the parent's population — it
  # must not be presented as a geometry-derived sub-area denominator.
  testthat::local_mocked_bindings(
    estimate_pop_for_geometries = function(geom_sf, country_iso3, year, ...) {
      rep(5555, nrow(geom_sf))
    }
  )

  gated <- suppressMessages(
    build_composite_locations(fx$normalized, fx$raw_sf, "ZZZ",
                              raster_dir = "ignored"))
  comp <- gated$data[grepl("composite_loc_ZZZ", gated$data$location_period_id), ]
  testthat::expect_true(is.na(comp$pop))
  testthat::expect_equal(comp$pop_source, "none")

  opted_in <- suppressMessages(
    build_composite_locations(fx$normalized, fx$raw_sf, "ZZZ",
                              raster_dir = "ignored",
                              allow_parent_pop_fallback = TRUE))
  comp2 <- opted_in$data[grepl("composite_loc_ZZZ", opted_in$data$location_period_id), ]
  testthat::expect_equal(comp2$pop_source, "parent_fallback")
  # The atomic parent pop (7000) is preferred over the raster-on-parent value.
  testthat::expect_equal(comp2$pop, 7000)
})

testthat::test_that("build_composite_locations never manufactures a zero denominator", {
  testthat::skip_if_not_installed("sf")

  p1 <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  p2 <- sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
  raw_sf <- sf::st_sf(
    location_period_id = c("100", "200"),
    geometry = sf::st_sfc(p1, p2),
    crs = 4326
  )
  # Both children have pop = NA. sum(na.rm = TRUE) over them returns 0, which
  # would give sCh/0 == Inf and flip the composite into the "high" class.
  normalized <- data.frame(
    location = c("AFR::ZZZ::Prov::Alpha", "AFR::ZZZ::Prov::Beta",
                 "AFR::ZZZ::Prov::Alpha|Beta"),
    location_period_id = c("100", "200", NA),
    pop = c(NA_real_, NA_real_, NA_real_),
    spatial_scale = rep("admin2", 3L),
    TL = as.Date(rep("2018-01-01", 3L)),
    stringsAsFactors = FALSE
  )

  res <- suppressMessages(build_composite_locations(normalized, raw_sf, "ZZZ"))
  comp <- res$data[grepl("composite_loc_ZZZ", res$data$location_period_id), ]

  testthat::expect_true(is.na(comp$pop))
  testthat::expect_false(isTRUE(comp$pop == 0))
  testthat::expect_equal(comp$pop_source, "none")
})

testthat::test_that("composite pop_source is recorded and atomic rows keep theirs", {
  testthat::skip_if_not_installed("sf")

  p1 <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  p2 <- sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
  raw_sf <- sf::st_sf(
    location_period_id = c("100", "200"),
    geometry = sf::st_sfc(p1, p2),
    crs = 4326
  )
  normalized <- data.frame(
    location = c("AFR::ZZZ::Prov::Alpha", "AFR::ZZZ::Prov::Beta",
                 "AFR::ZZZ::Prov::Alpha|Beta"),
    location_period_id = c("100", "200", NA),
    pop = c(1000, 2000, NA),
    pop_source = c("worldpop_constrained", "worldpop_constrained", NA),
    spatial_scale = rep("admin2", 3L),
    TL = as.Date(rep("2018-01-01", 3L)),
    stringsAsFactors = FALSE
  )

  res <- suppressMessages(build_composite_locations(normalized, raw_sf, "ZZZ"))

  comp   <- res$data[grepl("composite_loc_ZZZ", res$data$location_period_id), ]
  atomic <- res$data[res$data$location_period_id == "100", ]

  testthat::expect_equal(comp$pop_source, "child_sum")
  testthat::expect_equal(comp$pop, 3000)
  testthat::expect_equal(atomic$pop_source, "worldpop_constrained")
})

testthat::test_that("geometry-derived pop on a genuine child union is classified composite_union", {
  testthat::skip_if_not_installed("sf")

  p1 <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  p2 <- sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
  raw_sf <- sf::st_sf(
    location_period_id = c("100", "200"),
    geometry = sf::st_sfc(p1, p2),
    crs = 4326
  )
  normalized <- data.frame(
    location = c("AFR::ZZZ::Prov::Alpha", "AFR::ZZZ::Prov::Beta",
                 "AFR::ZZZ::Prov::Alpha|Beta"),
    location_period_id = c("100", "200", NA),
    pop = c(1000, 2000, NA),
    spatial_scale = rep("admin2", 3L),
    TL = as.Date(rep("2018-01-01", 3L)),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    estimate_pop_for_geometries = function(geom_sf, country_iso3, year, ...) {
      rep(5555, nrow(geom_sf))
    }
  )

  res <- suppressMessages(
    build_composite_locations(normalized, raw_sf, "ZZZ", raster_dir = "ignored"))
  comp <- res$data[grepl("composite_loc_ZZZ", res$data$location_period_id), ]

  testthat::expect_equal(comp$pop, 5555)
  testthat::expect_equal(comp$pop_source, "composite_union")
})
