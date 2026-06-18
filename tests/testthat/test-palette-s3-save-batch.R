test_that("palette extraction returns expected columns and plot", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image()
  pal <- extract_image_palette(path, n = 4)
  expect_s3_class(pal, "tbl_df")
  expect_true(all(c(
    "hex", "red", "green", "blue", "hue", "saturation", "value",
    "frequency", "proportion", "colorspace"
  ) %in% names(pal)))
  expect_s3_class(plot_image_palette(pal), "ggplot")
})

test_that("S3 methods print, summarize, plot, and save", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image()
  result <- image_mosaic_art(path, tile_size = 8)
  expect_output(print(result), "image_art")
  expect_s3_class(summary(result), "tbl_df")

  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  plotted <- plot(result)
  expect_s3_class(plotted, "ggplot")

  out <- tempfile(fileext = ".png")
  expect_invisible(save_image_art(result, out, width = 3, height = 2))
  expect_true(file.exists(out))
})

test_that("image_artify dispatches and batch processing logs failures", {
  testthat::skip_if_not_installed("magick")
  path <- make_test_image()
  result <- image_artify(path, type = "outline", max_dimension = 60)
  expect_s3_class(result, "image_art")

  out_dir <- tempfile("imageartr-batch")
  log <- batch_image_artify(
    c(path, file.path(tempdir(), "missing.png")),
    output_dir = out_dir,
    type = "mosaic",
    tile_size = 8
  )
  expect_s3_class(log, "tbl_df")
  expect_true(any(log$success))
  expect_true(any(!log$success))
})
