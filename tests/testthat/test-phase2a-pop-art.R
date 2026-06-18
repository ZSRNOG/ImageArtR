test_that("pop art creates panels with built-in and custom palettes", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 32, height = 24)
  result <- image_pop_art(path, panels = 4, seed = 123, max_dimension = 80)
  expect_s3_class(result, "image_art")
  expect_equal(result$type, "pop_art")
  expect_s3_class(result$image, "magick-image")
  expect_equal(length(result$processed$palettes), 4)

  custom <- image_pop_art(path, panels = 2, palette = c("black", "white", "gold"))
  expect_s3_class(custom, "image_art")
  expect_equal(custom$processed$layout, c(1L, 2L))
})

test_that("pop art palette listing, image palette, validation, and seeds work", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image(width = 12, height = 12, transparent = TRUE)
  palettes <- list_pop_art_palettes()
  expect_s3_class(palettes, "tbl_df")
  expect_gte(nrow(palettes), 6)

  set.seed(4)
  before <- .Random.seed
  first <- image_pop_art(path, panels = 2, palette_method = "random", seed = 77)
  after <- .Random.seed
  second <- image_pop_art(path, panels = 2, palette_method = "random", seed = 77)
  expect_equal(after, before)
  expect_equal(first$processed$palettes, second$processed$palettes)
  expect_s3_class(image_pop_art(path, panels = 1, palette_method = "image"), "image_art")
  expect_error(image_pop_art(path, panels = 3), "panels")
  expect_error(image_pop_art(path, levels = 1), "levels")
})
