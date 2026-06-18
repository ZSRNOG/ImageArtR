make_test_image <- function(
  width = 36,
  height = 24,
  transparent = FALSE,
  grayscale = FALSE,
  path = tempfile(fileext = ".png")
) {
  base <- magick::image_blank(width, height, color = if (transparent) "none" else "white")
  colors <- if (grayscale) c("gray20", "gray55", "gray85") else c("tomato", "steelblue", "gold")
  block_w <- max(1, floor(width / 3))
  for (i in seq_along(colors)) {
    block <- magick::image_blank(block_w, height, color = colors[i])
    base <- magick::image_composite(base, block, offset = sprintf("+%s+0", (i - 1) * block_w))
  }
  if (transparent) {
    hole <- magick::image_blank(block_w, floor(height / 2), color = "none")
    base <- magick::image_composite(base, hole, operator = "copy", offset = "+0+0")
  }
  magick::image_write(base, path)
  path
}

make_file_url <- function(path) {
  paste0("file:///", gsub("\\\\", "/", normalizePath(path, winslash = "/")))
}
