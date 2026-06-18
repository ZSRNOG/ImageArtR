test_that("stipple art supports sampling and color modes", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 32, height = 22)

  density <- image_stipple_art(path, n = 80, method = "density", seed = 123)
  expect_s3_class(density, "image_art")
  expect_equal(density$type, "stipple")
  expect_s3_class(density$plot, "ggplot")
  expect_equal(nrow(density$processed), 80)

  grid <- image_stipple_art(path, n = 60, method = "grid", color = "original")
  expect_s3_class(grid, "image_art")
  expect_true(all(c("x", "y", "hex", "point_size") %in% names(grid$processed)))

  edge <- image_stipple_art(path, n = 50, method = "edge", color = "single", seed = 1)
  expect_equal(unique(edge$processed$hex), "#000000FF")
})

test_that("stipple art validates arguments and preserves seeds", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 20, height = 20, transparent = TRUE)
  set.seed(44)
  before <- .Random.seed
  first <- image_stipple_art(path, n = 40, seed = 99, max_dimension = 50)
  after <- .Random.seed
  second <- image_stipple_art(path, n = 40, seed = 99, max_dimension = 50)
  expect_equal(after, before)
  expect_equal(first$processed[, c("x", "y", "hex")], second$processed[, c("x", "y", "hex")])
  expect_error(image_stipple_art(path, n = 0), "n")
  expect_error(image_stipple_art(path, point_size = c(2, 1)), "point_size")
})
