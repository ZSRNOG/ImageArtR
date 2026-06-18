# Local portrait demo for manual use only.
# This script is not run during package build, tests, or checks.

library(ImageArtR)

portrait_path <- "path/to/portrait.jpg"
output_dir <- "demo-local/output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

img <- read_art_image(portrait_path)
styles <- c(
  "stipple", "halftone", "hex_mosaic", "pop_art", "duotone", "ascii",
  "voronoi", "lowpoly", "cartoon", "stained_glass", "woodcut"
)

run_style <- function(style) {
  if (!style %in% list_art_styles()$style) {
    message("Skipping unavailable style: ", style)
    return(NULL)
  }
  result <- switch(style,
    stipple = image_artify(img, type = style, n = 8000, color = "original", seed = 123),
    halftone = image_artify(img, type = style, cell_size = 10, mode = "color"),
    hex_mosaic = image_artify(img, type = style, hex_size = 12),
    pop_art = image_artify(img, type = style, panels = 4, seed = 123),
    duotone = image_artify(img, type = style),
    ascii = image_artify(img, type = style, width = 90, output = "ggplot"),
    image_artify(img, type = style)
  )
  ext <- if (identical(style, "ascii")) "html" else "png"
  save_image_art(result, file.path(output_dir, paste0(style, ".", ext)), overwrite = TRUE)
  result
}

results <- lapply(styles, run_style)
results <- Filter(Negate(is.null), results)

if (length(results) > 0) {
  comparison <- results[[1]]$plot
  save_image_art(results[[1]], file.path(output_dir, "style-comparison.png"), overwrite = TRUE)
}
