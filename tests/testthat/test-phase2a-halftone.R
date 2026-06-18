test_that("halftone supports grayscale, color, and cmyk modes", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 40, height = 24)

  gray <- image_halftone(path, cell_size = 8, mode = "grayscale")
  expect_s3_class(gray, "image_art")
  expect_equal(gray$type, "halftone")
  expect_s3_class(gray$plot, "ggplot")
  out <- tempfile(fileext = ".png")
  expect_invisible(save_image_art(gray, out, width = 2, height = 1.2, overwrite = TRUE))
  pixels <- magick::image_data(magick::image_read(out), channels = "rgb")
  expect_true(any(as.integer(pixels) < 250))

  color <- image_halftone(path, cell_size = 8, mode = "color", shape = "square")
  expect_s3_class(color, "image_art")
  expect_true(length(unique(color$processed$hex)) > 1)

  cmyk <- image_halftone(path, cell_size = 10, mode = "cmyk", shape = "line")
  expect_s3_class(cmyk, "image_art")
  expect_true(all(c("cyan", "magenta", "yellow", "black") %in% cmyk$processed$channel))
})

test_that("halftone validates arguments and handles edge images", {
  testthat::skip_if_not_installed("magick")
  tiny <- make_test_image(width = 2, height = 2, grayscale = TRUE)
  wide <- make_test_image(width = 80, height = 8, transparent = TRUE)
  expect_s3_class(image_halftone(tiny, cell_size = 1), "image_art")
  expect_s3_class(image_halftone(wide, cell_size = 8, invert = TRUE), "image_art")
  expect_error(image_halftone(tiny, cell_size = 0), "cell_size")
  expect_error(image_halftone(tiny, max_size = -1), "max_size")
})
