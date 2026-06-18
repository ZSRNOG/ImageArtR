# This script recreates the README gallery images for development use.
# It is intentionally not run during package checks.

devtools::load_all(".", quiet = TRUE)

source(file.path("data-raw", "create-example-image.R"), local = TRUE)

output_dir <- file.path("gallery", "readme")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

example <- read_art_image(file.path("inst", "extdata", "example.png"))
target_width <- 720
target_height <- 540
render_width <- 4
render_height <- 3
dpi <- 180

normalize_png <- function(input, output) {
  image <- magick::image_read(input)
  image <- magick::image_background(image, "white", flatten = TRUE)
  image <- magick::image_trim(image)
  image <- magick::image_repage(image)
  image <- magick::image_resize(
    image,
    sprintf("%sx%s", target_width - 48, target_height - 48)
  )
  image <- magick::image_extent(
    image,
    sprintf("%sx%s", target_width, target_height),
    gravity = "center",
    color = "white"
  )
  magick::image_write(image, output)
}

save_gallery_image <- function(art, filename, width = render_width, height = render_height) {
  raw <- tempfile(fileext = ".png")
  if (is.null(art$image)) {
    save_image_art(art, raw, width = width, height = height, dpi = dpi, overwrite = TRUE)
  } else {
    save_image_art(art, raw, overwrite = TRUE)
  }
  normalize_png(raw, file.path(output_dir, filename))
}

save_gallery_image(
  image_stipple_art(example, n = 6500, color = "original", point_size = c(0.25, 1.2), seed = 123),
  "01_stipple.png"
)
save_gallery_image(
  image_halftone(example, cell_size = 8, mode = "color", max_size = 7.2, background = "white"),
  "02_halftone.png"
)
save_gallery_image(
  image_hex_mosaic(example, hex_size = 10, background = "white"),
  "03_hex_mosaic.png"
)
save_gallery_image(
  image_pop_art(example, panels = 4, seed = 123),
  "04_pop_art.png"
)
save_gallery_image(
  image_duotone(example, shadow = "#243B53", highlight = "#FFB84D"),
  "05_duotone.png"
)
save_gallery_image(
  image_ascii_art(example, width = 72, output = "ggplot", color = TRUE, font_size = 2.6),
  "06_ascii.png"
)
