#' Create mosaic art from an image
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param tile_size Tile size in pixels.
#' @param shape Tile shape: square, circle, or hexagon.
#' @param color_method Tile color summary: mean, median, or dominant.
#' @param palette_size Optional number of colors used to compress the result.
#' @param border Draw borders around tiles?
#' @param border_color Border color.
#' @param border_width Border width in ggplot units.
#' @param background Plot background color, or `"transparent"`.
#' @param max_dimension Maximum source image dimension before processing.
#'
#' @return An `image_art` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_mosaic_art(path, tile_size = 12)
#' plot(result)
image_mosaic_art <- function(
  image,
  tile_size = 10,
  shape = c("square", "circle", "hexagon"),
  color_method = c("mean", "median", "dominant"),
  palette_size = NULL,
  border = FALSE,
  border_color = "white",
  border_width = 0.1,
  background = "transparent",
  max_dimension = 1000
) {
  call <- match.call()
  shape <- .arg_match(shape, c("square", "circle", "hexagon"), "shape")
  color_method <- .arg_match(color_method, c("mean", "median", "dominant"), "color_method")
  .check_positive_integer(tile_size, "tile_size")
  .check_bool(border, "border")
  .check_color(border_color, "border_color")
  .check_number(border_width, "border_width", min = 0)
  .check_color(background, "background")
  .check_max_dimension(max_dimension)
  if (!is.null(palette_size)) {
    .check_number(palette_size, "palette_size", min = 1, integer = TRUE)
  }

  original <- read_art_image(image)
  working <- .limit_image_dimension(original, max_dimension)
  info <- .image_info(working)
  pixels <- .image_to_pixel_df(working)
  pixels$tile_x <- floor((pixels$x - 1) / tile_size)
  pixels$tile_y <- floor((pixels$y - 1) / tile_size)

  groups <- split(seq_len(nrow(pixels)), interaction(pixels$tile_x, pixels$tile_y, drop = TRUE))
  tiles <- purrr::map_dfr(groups, function(idx) {
    .summarise_mosaic_tile(pixels[idx, , drop = FALSE], color_method)
  })
  tiles <- .compress_tile_palette(tiles, palette_size)

  plot <- .mosaic_plot(
    tiles,
    width = info$width,
    height = info$height,
    shape = shape,
    border = border,
    border_color = border_color,
    border_width = border_width,
    background = background
  )

  .new_image_art(
    original = original,
    processed = tiles,
    plot = plot,
    image = NULL,
    type = "mosaic",
    parameters = list(
      tile_size = tile_size,
      shape = shape,
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

.summarise_mosaic_tile <- function(tile, color_method) {
  if (identical(color_method, "mean")) {
    red <- mean(tile$red)
    green <- mean(tile$green)
    blue <- mean(tile$blue)
    alpha <- mean(tile$alpha)
  } else if (identical(color_method, "median")) {
    red <- stats::median(tile$red)
    green <- stats::median(tile$green)
    blue <- stats::median(tile$blue)
    alpha <- stats::median(tile$alpha)
  } else {
    counts <- sort(table(tile$hex), decreasing = TRUE)
    rgba <- grDevices::col2rgb(names(counts)[1], alpha = TRUE)[, 1]
    red <- rgba[1]
    green <- rgba[2]
    blue <- rgba[3]
    alpha <- rgba[4]
  }

  x_min <- min(tile$x)
  x_max <- max(tile$x)
  y_min <- min(tile$y)
  y_max <- max(tile$y)
  tibble::tibble(
    tile_x = tile$tile_x[1],
    tile_y = tile$tile_y[1],
    x_min = x_min - 1,
    x_max = x_max,
    y_min = y_min - 1,
    y_max = y_max,
    x = (x_min + x_max - 1) / 2,
    y = (y_min + y_max - 1) / 2,
    width = x_max - x_min + 1,
    height = y_max - y_min + 1,
    red = red,
    green = green,
    blue = blue,
    alpha = alpha,
    hex = .as_hex(red, green, blue, alpha)
  )
}

.compress_tile_palette <- function(tiles, palette_size) {
  if (is.null(palette_size) || nrow(tiles) <= palette_size) {
    return(tiles)
  }
  rgb <- unique(as.matrix(tiles[, c("red", "green", "blue")]))
  centers <- min(palette_size, nrow(rgb))
  if (centers <= 1) {
    tiles$red <- mean(tiles$red)
    tiles$green <- mean(tiles$green)
    tiles$blue <- mean(tiles$blue)
  } else {
    km <- .with_seed(1L, stats::kmeans(as.matrix(tiles[, c("red", "green", "blue")]), centers = centers))
    tiles$red <- km$centers[km$cluster, "red"]
    tiles$green <- km$centers[km$cluster, "green"]
    tiles$blue <- km$centers[km$cluster, "blue"]
  }
  tiles$hex <- .as_hex(tiles$red, tiles$green, tiles$blue, tiles$alpha)
  tiles
}

.mosaic_plot <- function(
  tiles,
  width,
  height,
  shape,
  border,
  border_color,
  border_width,
  background
) {
  border_col <- if (border) border_color else NA

  if (identical(shape, "square")) {
    p <- ggplot2::ggplot(tiles) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = x_min,
          xmax = x_max,
          ymin = y_min,
          ymax = y_max,
          fill = hex
        ),
        color = border_col,
        linewidth = border_width
      )
  } else {
    vertices <- .tile_vertices(tiles, shape)
    p <- ggplot2::ggplot(vertices) +
      ggplot2::geom_polygon(
        ggplot2::aes(x = x, y = y, group = id, fill = hex),
        color = border_col,
        linewidth = border_width
      )
  }

  p +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, width), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(0, height), expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    .plot_background_theme(background)
}

.tile_vertices <- function(tiles, shape) {
  sides <- if (identical(shape, "hexagon")) 6 else 28
  phase <- if (identical(shape, "hexagon")) pi / 6 else 0
  purrr::pmap_dfr(
    list(
      id = seq_len(nrow(tiles)),
      x = tiles$x,
      y = tiles$y,
      width = tiles$width,
      height = tiles$height,
      hex = tiles$hex
    ),
    function(id, x, y, width, height, hex) {
      theta <- seq(0, 2 * pi, length.out = sides + 1)[-(sides + 1)] + phase
      radius_x <- width / 2
      radius_y <- height / 2
      if (identical(shape, "circle")) {
        radius_x <- min(radius_x, radius_y)
        radius_y <- radius_x
      }
      tibble::tibble(
        id = id,
        x = x + radius_x * cos(theta),
        y = y + radius_y * sin(theta),
        hex = hex
      )
    }
  )
}
