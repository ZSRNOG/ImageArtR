test_that("read_art_image supports common input types", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image()

  expect_s3_class(read_art_image(path), "magick-image")
  expect_s3_class(read_art_image(make_file_url(path)), "magick-image")
  expect_s3_class(read_art_image(magick::image_read(path)), "magick-image")
  expect_error(read_art_image(file.path(tempdir(), "missing.png")), "does not exist")

  testthat::skip_if_not_installed("imager")
  cimg <- imager::load.image(path)
  expect_s3_class(read_art_image(cimg), "magick-image")
})

test_that("preprocess_image handles resize, grayscale, and transparent background", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 80, height = 20, transparent = TRUE)
  img <- preprocess_image(
    path,
    resize = 40,
    grayscale = TRUE,
    normalize = TRUE,
    blur = 0.2,
    contrast = 1.1,
    remove_background = TRUE,
    background_color = "white"
  )
  info <- magick::image_info(img)
  expect_lte(max(info$width, info$height), 40)
  expect_s3_class(img, "magick-image")
})
