test_that("art functions return image_art objects", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 40, height = 24)

  mosaic <- image_mosaic_art(path, tile_size = 8, max_dimension = 80)
  expect_s3_class(mosaic, "image_art")
  expect_equal(mosaic$type, "mosaic")

  circle <- image_circle_art(path, n = 40, seed = 123, max_dimension = 80)
  expect_s3_class(circle, "image_art")
  expect_equal(circle$type, "circle")

  outline <- image_outline(path, method = "sobel", max_dimension = 80)
  expect_s3_class(outline, "image_art")
  expect_equal(outline$type, "outline")

  sketch <- image_sketch(path, style = "pencil", max_dimension = 80)
  expect_s3_class(sketch, "image_art")
  expect_equal(sketch$type, "sketch")
})

test_that("mosaic supports shapes and color methods", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image()
  expect_s3_class(image_mosaic_art(path, shape = "square", color_method = "mean"), "image_art")
  expect_s3_class(image_mosaic_art(path, shape = "circle", color_method = "median"), "image_art")
  expect_s3_class(image_mosaic_art(path, shape = "hexagon", color_method = "dominant"), "image_art")
})

test_that("outline methods and sketch styles run on small images", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 28, height = 28, grayscale = TRUE)
  expect_s3_class(image_outline(path, method = "sobel", max_dimension = 60), "image_art")
  expect_s3_class(image_outline(path, method = "laplacian", max_dimension = 60), "image_art")
  expect_s3_class(image_outline(path, method = "canny", max_dimension = 60), "image_art")
  expect_s3_class(image_sketch(path, style = "line", max_dimension = 60), "image_art")
  expect_s3_class(image_sketch(path, style = "pencil", max_dimension = 60), "image_art")
  expect_s3_class(image_sketch(path, style = "cartoon", max_dimension = 60), "image_art")
})

test_that("circle art is reproducible and preserves existing seed", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image()
  set.seed(99)
  before <- .Random.seed
  first <- image_circle_art(path, n = 35, seed = 123, max_dimension = 70)
  after <- .Random.seed
  second <- image_circle_art(path, n = 35, seed = 123, max_dimension = 70)
  expect_equal(after, before)
  expect_equal(
    first$processed$layout[, c("x", "y", "radius", "hex")],
    second$processed$layout[, c("x", "y", "radius", "hex")]
  )
})

test_that("invalid arguments fail clearly", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image()
  expect_error(image_mosaic_art(path, tile_size = 0), "tile_size")
  expect_error(image_outline(path, method = "unknown"), "method")
  expect_error(image_sketch(path, detail = 2), "detail")
})
