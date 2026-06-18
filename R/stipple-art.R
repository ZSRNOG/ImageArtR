#' Create stipple art from an image
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param n Number of points to draw.
#' @param method Sampling method: density, random, grid, or edge.
#' @param point_size Numeric length-two range for point sizes.
#' @param point_shape ggplot2 point shape.
#' @param color Point color mode: grayscale, original, or single.
#' @param single_color Color used when `color = "single"`.
#' @param background Plot background color.
#' @param gamma Gamma adjustment applied to darkness.
#' @param edge_weight Extra edge-gradient weight for density sampling.
#' @param seed Optional random seed. The previous user seed is restored.
#' @param max_dimension Maximum source image dimension before processing.
#'
#' @return An `image_art` object with a modifiable `ggplot2` plot.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_stipple_art(path, n = 600, seed = 123)
#' plot(result)
image_stipple_art <- function(
  image,
  n = 10000,
  method = c("density", "random", "grid", "edge"),
  point_size = c(0.2, 1.5),
  point_shape = 16,
  color = c("grayscale", "original", "single"),
  single_color = "black",
  background = "white",
  gamma = 1,
  edge_weight = 0,
  seed = NULL,
  max_dimension = 1000
) {
  call <- match.call()
  method <- .arg_match(method, c("density", "random", "grid", "edge"), "method")
  color <- .arg_match(color, c("grayscale", "original", "single"), "color")
  .check_positive_integer(n, "n")
  .check_number(gamma, "gamma", min = 0.05, max = 10)
  .check_number(edge_weight, "edge_weight", min = 0, max = 10)
  .check_number(max_dimension, "max_dimension", min = 10, integer = TRUE)
  .check_color(single_color, "single_color")
  .check_color(background, "background")
  if (!is.numeric(point_size) || length(point_size) != 2 || any(is.na(point_size)) || any(point_size < 0)) {
    rlang::abort("`point_size` must be a numeric length-two range with non-negative values.")
  }
  if (point_size[2] < point_size[1]) {
    rlang::abort("`point_size[2]` must be greater than or equal to `point_size[1]`.")
  }

  original <- read_art_image(image)
  working <- .limit_image_dimension(original, max_dimension)
  info <- .image_info(working)
  gray <- .image_luminance(working)
  darkness <- .clip01((1 - gray)^gamma)
  gradient <- .image_gradient(gray)

  points <- .with_seed(seed, {
    .stipple_points(
      image = working,
      darkness = darkness,
      gradient = gradient,
      n = n,
      method = method,
      point_size = point_size,
      color = color,
      single_color = single_color,
      edge_weight = edge_weight
    )
  })

  plot <- ggplot2::ggplot(points, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(
      ggplot2::aes(color = hex, size = point_size),
      shape = point_shape,
      alpha = points$alpha / 255,
      stroke = 0
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_continuous(limits = c(0, info$width), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(info$height, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    .plot_background_theme(background)

  .make_image_art(
    original = original,
    processed = points,
    plot = plot,
    image = NULL,
    type = "stipple",
    parameters = list(
      n = n,
      method = method,
      point_size = point_size,
      point_shape = point_shape,
      color = color,
      single_color = single_color,
      background = background,
      gamma = gamma,
      edge_weight = edge_weight,
      seed = seed,
      max_dimension = max_dimension
    ),
    call = call
  )
}

.stipple_points <- function(
  image,
  darkness,
  gradient,
  n,
  method,
  point_size,
  color,
  single_color,
  edge_weight
) {
  height <- nrow(darkness)
  width <- ncol(darkness)
  count <- width * height

  if (identical(method, "grid")) {
    side <- ceiling(sqrt(n * width / height))
    cols <- max(1, side)
    rows <- max(1, ceiling(n / cols))
    x <- seq(1, width, length.out = cols)
    y <- seq(1, height, length.out = rows)
    grid <- tidyr::expand_grid(x = x, y = y)
    idx <- (round(grid$y) - 1) * width + round(grid$x)
  } else {
    probability <- switch(method,
      random = rep(1, count),
      edge = as.vector(.clip01(darkness + pmax(1, edge_weight) * gradient)),
      density = as.vector(.clip01(darkness + edge_weight * gradient))
    )
    if (!any(probability > 0)) {
      probability <- rep(1, count)
    }
    idx <- sample.int(count, size = n, replace = n > count, prob = probability)
    y <- ((idx - 1) %% height) + 1
    x <- ((idx - 1) %/% height) + 1
    grid <- tibble::tibble(x = x, y = y)
  }

  coords <- .normalize_coordinates(grid$x, grid$y, width, height)
  local_darkness <- darkness[cbind(round(coords$y), round(coords$x))]
  local_size <- point_size[1] + local_darkness * diff(point_size)
  colors <- .stipple_colors(image, coords$x, coords$y, local_darkness, color, single_color)
  tibble::tibble(
    x = coords$x,
    y = coords$y,
    point_size = local_size,
    red = colors$red,
    green = colors$green,
    blue = colors$blue,
    alpha = colors$alpha,
    hex = colors$hex
  )
}

.stipple_colors <- function(image, x, y, darkness, color, single_color) {
  if (identical(color, "single")) {
    rgba <- .color_to_rgba(single_color)
    return(tibble::tibble(
      red = rep(rgba["red"], length(x)),
      green = rep(rgba["green"], length(x)),
      blue = rep(rgba["blue"], length(x)),
      alpha = rep(rgba["alpha"], length(x)),
      hex = rep(.as_hex(rgba["red"], rgba["green"], rgba["blue"], rgba["alpha"]), length(x))
    ))
  }
  if (identical(color, "original")) {
    return(.sample_pixel_color(image, x, y))
  }
  value <- round((1 - darkness) * 255)
  tibble::tibble(
    red = value,
    green = value,
    blue = value,
    alpha = 255,
    hex = .as_hex(value, value, value)
  )
}
