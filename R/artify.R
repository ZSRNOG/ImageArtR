#' Apply an artistic transformation by type
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param type Transformation type.
#' @param ... Arguments passed to the selected transformation function.
#'
#' @return An `image_art` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_artify(path, type = "mosaic", tile_size = 16)
image_artify <- function(
  image,
  type = c(
    "circle", "mosaic", "outline", "sketch", "stipple", "halftone",
    "hex_mosaic", "pop_art", "duotone", "ascii"
  ),
  ...
) {
  type <- .arg_match(type, .art_style_choices(), "type")
  switch(type,
    circle = image_circle_art(image, ...),
    mosaic = image_mosaic_art(image, ...),
    outline = image_outline(image, ...),
    sketch = image_sketch(image, ...),
    stipple = image_stipple_art(image, ...),
    halftone = image_halftone(image, ...),
    hex_mosaic = image_hex_mosaic(image, ...),
    pop_art = image_pop_art(image, ...),
    duotone = image_duotone(image, ...),
    ascii = image_ascii_art(image, ...)
  )
}

#' List available art styles
#'
#' @return A tibble with style metadata.
#' @export
#'
#' @examples
#' list_art_styles()
list_art_styles <- function() {
  tibble::tibble(
    style = .art_style_choices(),
    function_name = c(
      "image_circle_art", "image_mosaic_art", "image_outline", "image_sketch",
      "image_stipple_art", "image_halftone", "image_hex_mosaic",
      "image_pop_art", "image_duotone", "image_ascii_art"
    ),
    description = c(
      "Circle packing art",
      "Regular square, circle, or hexagon mosaic",
      "Raster outline extraction",
      "Line, pencil, or cartoon sketch",
      "Point-based stippling",
      "Halftone dots, squares, or lines",
      "Regular hexagon mosaic",
      "Multi-panel posterized pop art",
      "Two-color tonal interpolation",
      "Text, ggplot, or HTML ASCII art"
    ),
    output_type = c(
      "ggplot", "ggplot", "raster", "raster", "ggplot", "ggplot",
      "ggplot", "raster", "raster", "text/ggplot/html"
    ),
    stochastic = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),
    difficulty = c(
      "medium", "easy", "easy", "medium", "medium", "medium",
      "medium", "medium", "easy", "easy"
    )
  )
}

.art_style_choices <- function() {
  c(
    "circle", "mosaic", "outline", "sketch", "stipple", "halftone",
    "hex_mosaic", "pop_art", "duotone", "ascii"
  )
}
