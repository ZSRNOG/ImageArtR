#' Create a regular hexagon mosaic
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param hex_size Hexagon radius in pixels.
#' @param color_method Color method: mean, median, or center.
#' @param palette_size Optional number of colors used to compress the result.
#' @param border Draw borders?
#' @param border_color Border color.
#' @param border_width Border width.
#' @param background Plot background color, or `"transparent"`.
#' @param max_dimension Maximum source image dimension before processing.
#'
#' @return An `image_art` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_hex_mosaic(path, hex_size = 12)
#' plot(result)
image_hex_mosaic <- function(
  image,
  hex_size = 12,
  color_method = c("mean", "median", "center"),
  palette_size = NULL,
  border = FALSE,
  border_color = "white",
  border_width = 0.1,
  background = "transparent",
  max_dimension = 1000
) {
  call <- match.call()
  color_method <- .arg_match(color_method, c("mean", "median", "center"), "color_method")
  .check_number(hex_size, "hex_size", min = 1)
  .check_number(palette_size, "palette_size", min = 1, integer = TRUE, allow_null = TRUE)
  .check_bool(border, "border")
  .check_color(border_color, "border_color")
  .check_number(border_width, "border_width", min = 0)
  .check_color(background, "background")
  .check_max_dimension(max_dimension)

  original <- read_art_image(image)
  working <- .limit_image_dimension(original, max_dimension)
  info <- .image_info(working)
  arr <- .image_data_array(working, "rgba")
  hexes <- .hex_grid(info$width, info$height, hex_size)
  hexes$hex <- vapply(seq_len(nrow(hexes)), function(i) {
    .sample_region_color(
      arr,
      cx = hexes$x[i],
      cy = hexes$y[i],
      radius = hex_size,
      method = color_method
    )
  }, character(1))
  hexes$hex <- .quantize_colors(hexes$hex, palette_size)
  vertices <- .hex_vertices(hexes)

  border_col <- if (border) border_color else NA
  plot <- ggplot2::ggplot(vertices) +
    ggplot2::geom_polygon(
      ggplot2::aes(x = x, y = y, group = id, fill = hex),
      color = border_col,
      linewidth = border_width
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, info$width), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(info$height, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    .plot_background_theme(background)

  .make_image_art(
    original = original,
    processed = list(hexes = hexes, vertices = vertices),
    plot = plot,
    image = NULL,
    type = "hex_mosaic",
    parameters = list(
      hex_size = hex_size,
      color_method = color_method,
      palette_size = palette_size,
      border = border,
      border_color = border_color,
      border_width = border_width,
      background = background,
      max_dimension = max_dimension
    ),
    call = call
  )
}
