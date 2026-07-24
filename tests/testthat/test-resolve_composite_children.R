# Unit tests for resolve_composite_children(). All network access is replaced by
# an injected pull_fn, so these run offline and deterministically.

# Build a fake API sf response for a single child location. `geom` is one of
# "polygon", "point", or "empty"; `lp` is the location_period_id (or NA).
fake_api_row <- function(lp, geom = "polygon", loc_name = "child") {
  g <- switch(
    geom,
    polygon = sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    point   = sf::st_point(c(0, 0)),
    empty   = sf::st_point()
  )
  sf::st_sf(
    attributes.location_name       = loc_name,
    attributes.location_period_id  = lp,
    geometry                       = sf::st_sfc(g, crs = 4326L)
  )
}

test_that("resolve_composite_children returns child LP + polygon geometry", {
  empty_resp <- sf::st_sf(attributes.location_name = character(0),
                          attributes.location_period_id = character(0),
                          geometry = sf::st_sfc(crs = 4326L))
  # pull_fn keyed on the requested location string.
  pull_fn <- function(username, api_key, locations, time_left, time_right) {
    if (grepl("Mbane", locations)) {
      return(fake_api_row("111", "polygon", "AFR::SEN::Saint-Louis::Dagana::Mbane"))
    }
    if (grepl("Ross-Bethio", locations)) {
      return(fake_api_row("222", "polygon", "AFR::SEN::Saint-Louis::Dagana::Ross-Bethio"))
    }
    empty_resp
  }

  res <- resolve_composite_children(
    composite_names = "AFR::SEN::Saint-Louis::Dagana::Mbane|Ross-Bethio",
    pull_fn = pull_fn, api_user = "x", api_key = "y"
  )
  testthat::expect_s3_class(res, "sf")
  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_setequal(res$location_period_id, c("111", "222"))
  testthat::expect_true(all(sf::st_dimension(sf::st_geometry(res)) == 2L))
})

test_that("resolve_composite_children drops NA-LP and non-polygon rows", {
  pull_fn <- function(username, api_key, locations, time_left, time_right) {
    if (grepl("::a$", locations)) return(fake_api_row(NA, "polygon"))   # no LP -> drop
    if (grepl("::b$", locations)) return(fake_api_row("9", "point"))    # point -> drop
    sf::st_sf(attributes.location_name = character(0),
              attributes.location_period_id = character(0),
              geometry = sf::st_sfc(crs = 4326L))
  }
  res <- resolve_composite_children("AFR::X::Y::a|b", pull_fn = pull_fn,
                                    api_user = "x", api_key = "y")
  testthat::expect_s3_class(res, "sf")
  testthat::expect_equal(nrow(res), 0L)
})

test_that("resolve_composite_children returns 0-row sf when no composites", {
  never_called <- function(...) stop("pull_fn should not be called")
  res <- resolve_composite_children(c("AFR::X::atomic", NA_character_),
                                    pull_fn = never_called)
  testthat::expect_s3_class(res, "sf")
  testthat::expect_equal(nrow(res), 0L)
})

test_that("resolve_composite_children survives a failing child pull", {
  pull_fn <- function(username, api_key, locations, time_left, time_right) {
    if (grepl("::good$", locations)) return(fake_api_row("42", "polygon"))
    stop("API 500")
  }
  res <- resolve_composite_children("AFR::X::Y::good|bad", pull_fn = pull_fn,
                                    api_user = "x", api_key = "y")
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_equal(res$location_period_id, "42")
})

test_that("resolve_composite_children caches raw pulls per child", {
  tmp <- tempfile("childcache"); dir.create(tmp)
  calls <- 0L
  pull_fn <- function(username, api_key, locations, time_left, time_right) {
    calls <<- calls + 1L
    fake_api_row("7", "polygon")
  }
  comp <- "AFR::X::Y::solo|other"
  r1 <- resolve_composite_children(comp, pull_fn = pull_fn, cache_dir = tmp,
                                   api_user = "x", api_key = "y")
  calls_after_first <- calls
  r2 <- resolve_composite_children(comp, pull_fn = pull_fn, cache_dir = tmp,
                                   api_user = "x", api_key = "y")
  testthat::expect_equal(calls, calls_after_first)          # no new API calls
  testthat::expect_equal(nrow(r1), nrow(r2))
  testthat::expect_gt(length(list.files(tmp, pattern = "^raw_api_cache_child_")), 0L)
})
