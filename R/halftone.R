#' Create halftone art from an image
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param cell_size Grid cell size in pixels.
#' @param shape Halftone mark shape: circle, square, or line.
#' @param mode Color mode: grayscale, color, or cmyk.
#' @param angle Line angle in degrees. CMYK mode offsets channels by common
#'   printing angles.
#' @param min_size Minimum mark size.
#' @param max_size Maximum mark size. Defaults to a size based on `cell_size`.
#' @param foreground Foreground color for grayscale mode.
#' @param background Background color.
#' @param invert Invert brightness-to-size mapping?
#' @param gamma Gamma adjustment applied to darkness.
#' @param max_dimension Maximum source image dimension before processing.
#'
#' @return An `image_art` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_halftone(path, cell_size = 10)
#' plot(result)
image_halftone <- function(
  image,
  cell_size = 8,
  shape = c("circle", "square", "line"),
  mode = c("grayscale", "color", "cmyk"),
  angle = 45,
  min_size = 0,
  max_size = NULL,
  foreground = "black",
  background = "white",
  invert = FALSE,
  gamma = 1,
  max_dimension = 1000
) {
  call <- match.call()
  shape <- .arg_match(shape, c("circle", "square", "line"), "shape")
  mode <- .arg_match(mode, c("grayscale", "color", "cmyk"), "mode")
  .check_positive_integer(cell_size, "cell_size")
  .check_number(angle, "angle")
  .check_number(min_size, "min_size", min = 0)
  .check_number(max_size, "max_size", min = 0, allow_null = TRUE)
  .check_color(foreground, "foreground")
  .check_color(background, "background")
  .check_bool(invert, "invert")
  .check_number(gamma, "gamma", min = 0.05, max = 10)
  .check_max_dimension(max_dimension)
  max_size <- max_size %||% (cell_size * 0.9)
  if (max_size < min_size) {
    rlang::abort("`max_size` must be greater than or equal to `min_size`.")
  }

  original <- read_art_image(image)
  working <- .limit_image_dimension(original, max_dimension)
  info <- .image_info(working)
  cells <- .grid_cell_summary(working, cell_size)
  cells <- .halftone_cells(cells, mode, min_size, max_size, foreground, invert, gamma)
  plot_data <- if (identical(mode, "cmyk")) {
    .halftone_cmyk_cells(cells, angle, min_size, max_size, gamma)
  } else {
    cells
  }
  plot <- .halftone_plot(
    plot_data,
    width = info$width,
    height = info$height,
    shape = shape,
    mode = mode,
    angle = angle,
    background = background
  )

  .make_image_art(
    original = original,
    processed = plot_data,
    plot = plot,
    image = NULL,
    type = "halftone",
    parameters = list(
      cell_size = cell_size,
      shape = shape,
      mode = mode,
      angle = angle,
      min_size = min_size,
      max_size = max_size,
      foreground = foreground,
      background = background,
      invert = invert,
      gamma = gamma,
      max_dimension = max_dimension
    ),
    call = call
  )
}

.halftone_cells <- function(cells, mode, min_size, max_size, foreground, invert, gamma) {
  darkness <- if (invert) cells$luminance else 1 - cells$luminance
  darkness <- .clip01(darkness^gamma)
  cells$size <- min_size + darkness * (max_size - min_size)
  cells$line_size <- pmax(0.05, cells$size / 3)
  cells$hex <- if (identical(mode, "color")) {
    .as_hex(cells$red, cells$green, cells$blue, cells$alpha)
  } else {
    fg <- .color_to_rgba(foreground)
    rep(.as_hex(fg["red"], fg["green"], fg["blue"], fg["alpha"]), nrow(cells))
  }
  cells
}

.halftone_cmyk_cells <- function(cells, angle, min_size, max_size, gamma) {
  r <- cells$red / 255
  g <- cells$green / 255
  b <- cells$blue / 255
  k <- 1 - pmax(r, g, b)
  denom <- pmax(1 - k, 1e-6)
  channels <- tibble::tibble(
    channel = c("cyan", "magenta", "yellow", "black"),
    hex = c("#00AEEF", "#EC008C", "#FFF200", "#000000"),
    angle = c(angle + 15, angle + 75, angle, angle + 45)
  )
  values <- list(
    cyan = .clip01((1 - r - k) / denom),
    magenta = .clip01((1 - g - k) / denom),
    yellow = .clip01((1 - b - k) / denom),
    black = .clip01(k)
  )

  purrr::map_dfr(seq_len(nrow(channels)), function(i) {
    value <- values[[channels$channel[i]]]^gamma
    out <- cells
    out$channel <- channels$channel[i]
    out$hex <- channels$hex[i]
    out$angle <- channels$angle[i]
    out$size <- min_size + value * (max_size - min_size)
    out$line_size <- pmax(0.05, out$size / 3)
    out
  })
}

.halftone_plot <- function(data, width, height, shape, mode, angle, background) {
  base <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y))
  if (identical(shape, "line")) {
    theta <- (if (identical(mode, "cmyk")) data$angle else angle) * pi / 180
    data$x_start <- data$x - data$size * cos(theta) / 2
    data$x_end <- data$x + data$size * cos(theta) / 2
    data$y_start <- data$y - data$size * sin(theta) / 2
    data$y_end <- data$y + data$size * sin(theta) / 2
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = x_start,
          xend = x_end,
          y = y_start,
          yend = y_end,
          color = hex,
          linewidth = line_size
        ),
        lineend = "round",
        alpha = if (identical(mode, "cmyk")) 0.65 else 1
      ) +
      ggplot2::scale_color_identity() +
      ggplot2::scale_linewidth_identity()
  } else {
    point_shape <- if (identical(shape, "square")) 22 else 21
    p <- base +
      ggplot2::geom_point(
        ggplot2::aes(size = size, fill = hex),
        shape = point_shape,
        color = NA,
        alpha = if (identical(mode, "cmyk")) 0.65 else 1
      ) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_size_identity()
  }

  p +
    ggplot2::scale_x_continuous(limits = c(0, width), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(height, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    .plot_background_theme(background)
}
