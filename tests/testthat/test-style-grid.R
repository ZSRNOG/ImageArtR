test_that("style grid creates a labeled comparison image", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 32, height = 24)
  grid <- image_style_grid(
    path,
    styles = c("original", "mosaic", "halftone", "duotone"),
    options = list(
      mosaic = list(tile_size = 8),
      halftone = list(cell_size = 8),
      duotone = list(shadow = "black", highlight = "white")
    ),
    ncol = 2,
    tile_width = 120,
    tile_height = 90,
    label_height = 18,
    label_size = 8,
    gap = 0
  )

  expect_s3_class(grid, "image_art")
  expect_equal(grid$type, "style_grid")
  expect_s3_class(grid$image, "magick-image")
  expect_s3_class(grid$plot, "ggplot")
  expect_equal(grid$processed$style, c("original", "mosaic", "halftone", "duotone"))

  info <- magick::image_info(grid$image)
  expect_equal(info$width, 240)
  expect_equal(info$height, 180)
})

test_that("style grid validates styles and per-style options", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 16, height = 12)
  expect_error(image_style_grid(path, styles = "lowpoly"), "styles")
  expect_error(image_style_grid(path, options = 1), "options")
  expect_error(image_style_grid(path, options = list(mosaic = 8)), "Each entry")
  expect_error(image_style_grid(path, labels = "Only one", styles = c("mosaic", "duotone")), "labels")
})
