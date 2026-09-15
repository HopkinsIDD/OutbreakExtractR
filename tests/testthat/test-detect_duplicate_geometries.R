sq <- function(x0, y0, w = 1) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + w),
                            c(x0, y0 + w), c(x0, y0))))
}

testthat::test_that("distinct geometries are all class 'unique' with dup_n 1", {
  testthat::skip_if_not_installed("sf")

  res <- detect_duplicate_geometries(
    lp_ids = c("1", "2", "3"),
    geoms  = sf::st_sfc(sq(0, 0), sq(2, 0), sq(4, 0), crs = 4326),
    lp_locations = c("AFR::ZZZ::A", "AFR::ZZZ::B", "AFR::ZZZ::C")
  )

  testthat::expect_equal(res$location_period_id, c("1", "2", "3"))
  testthat::expect_equal(res$pop_geom_dup_n, rep(1L, 3L))
  testthat::expect_equal(unique(res$pop_geom_dup_class), "unique")
})

testthat::test_that("duplicates spanning >1 admin depth are 'parent_inherited'", {
  testthat::skip_if_not_installed("sf")

  res <- detect_duplicate_geometries(
    lp_ids = c("parent", "child_a", "child_b"),
    geoms  = sf::st_sfc(sq(0, 0), sq(0, 0), sq(0, 0), crs = 4326),
    lp_locations = c("AFR::GIN::Conakry",
                     "AFR::GIN::Conakry::Dixinn",
                     "AFR::GIN::Conakry::Matam")
  )

  testthat::expect_equal(res$pop_geom_dup_n, rep(3L, 3L))
  testthat::expect_equal(unique(res$pop_geom_dup_class), "parent_inherited")
})

testthat::test_that("same-depth same-base-name duplicates are 'alias', not 'cross_unit'", {
  testthat::skip_if_not_installed("sf")

  # "GN-FR.Fria" and "Fria" name one real place under two conventions.
  res <- detect_duplicate_geometries(
    lp_ids = c("10", "11"),
    geoms  = sf::st_sfc(sq(0, 0), sq(0, 0), crs = 4326),
    lp_locations = c("AFR::GIN::GN-B::Fria", "AFR::GIN::GN-B::GN-FR.Fria")
  )

  testthat::expect_equal(unique(res$pop_geom_dup_class), "alias")
  testthat::expect_equal(res$pop_geom_dup_n, c(2L, 2L))
})

testthat::test_that("same-depth different-name duplicates are 'cross_unit'", {
  testthat::skip_if_not_installed("sf")

  res <- detect_duplicate_geometries(
    lp_ids = c("20", "21"),
    geoms  = sf::st_sfc(sq(0, 0), sq(0, 0), crs = 4326),
    lp_locations = c("AFR::NGA::Lagos::Shomolu", "AFR::NGA::Nasarawa::Awe")
  )

  testthat::expect_equal(unique(res$pop_geom_dup_class), "cross_unit")
})

testthat::test_that("duplicates are 'unknown' when no location names are supplied", {
  testthat::skip_if_not_installed("sf")

  res <- detect_duplicate_geometries(
    lp_ids = c("1", "2", "3"),
    geoms  = sf::st_sfc(sq(0, 0), sq(0, 0), sq(5, 5), crs = 4326)
  )

  testthat::expect_equal(res$pop_geom_dup_n, c(2L, 2L, 1L))
  testthat::expect_equal(res$pop_geom_dup_class, c("unknown", "unknown", "unique"))
})

testthat::test_that("empty geometries are never duplicates of one another", {
  testthat::skip_if_not_installed("sf")

  # Sharing "no footprint" says nothing about sharing a denominator.
  res <- detect_duplicate_geometries(
    lp_ids = c("e1", "e2", "real"),
    geoms  = sf::st_sfc(sf::st_polygon(), sf::st_polygon(), sq(0, 0), crs = 4326),
    lp_locations = c("AFR::ZZZ::A", "AFR::ZZZ::B", "AFR::ZZZ::C")
  )

  testthat::expect_equal(res$pop_geom_dup_n, rep(1L, 3L))
  testthat::expect_equal(unique(res$pop_geom_dup_class), "unique")
})

testthat::test_that("detect_duplicate_geometries accepts an sf object and zero rows", {
  testthat::skip_if_not_installed("sf")

  x <- sf::st_sf(location_period_id = c("1", "2"),
                 geometry = sf::st_sfc(sq(0, 0), sq(0, 0), crs = 4326))
  res <- detect_duplicate_geometries(x$location_period_id, x)
  testthat::expect_equal(res$pop_geom_dup_n, c(2L, 2L))

  empty <- detect_duplicate_geometries(character(0), sf::st_sfc(crs = 4326))
  testthat::expect_equal(nrow(empty), 0L)
  testthat::expect_named(empty, c("location_period_id", "pop_geom_dup_n",
                                  "pop_geom_dup_class"))
})
