#' Create circle packing art from an image
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param n Number of circles.
#' @param size_distribution Circle size distribution.
#' @param alpha Alpha shape parameter for beta-distributed sizes.
#' @param beta Beta shape parameter for beta-distributed sizes.
#' @param min_size Minimum relative circle area.
#' @param max_size Maximum relative circle area.
#' @param color_method Circle color sampling method.
#' @param background Plot background color, or `"transparent"`.
#' @param remove_background Remove near-background color before sampling?
#' @param background_color Color used by background removal.
#' @param fuzz Background removal tolerance.
#' @param seed Optional random seed. The previous user random seed is restored.
#' @param max_dimension Maximum source image dimension before processing.
#'
#' @return An `image_art` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_circle_art(path, n = 200, seed = 123)
#' plot(result)
image_circle_art <- function(
  image,
  n = 2000,
  size_distribution = c("beta", "uniform", "fixed", "importance"),
  alpha = 1,
  beta = 1,
  min_size = NULL,
  max_size = NULL,
  color_method = c("center", "mean", "median"),
  background = "transparent",
  remove_background = FALSE,
  background_color = "white",
  fuzz = 10,
  seed = NULL,
  max_dimension = 800
) {
  call <- match.call()
  size_distribution <- .arg_match(
    size_distribution,
    c("beta", "uniform", "fixed", "importance"),
    "size_distribution"
  )
  color_method <- .arg_match(color_method, c("center", "mean", "median"), "color_method")
  .check_positive_integer(n, "n")
  .check_number(alpha, "alpha", min = 0.01)
  .check_number(beta, "beta", min = 0.01)
  .check_number(min_size, "min_size", min = 0.001, allow_null = TRUE)
  .check_number(max_size, "max_size", min = 0.001, allow_null = TRUE)
  .check_color(background, "background")
  .check_bool(remove_background, "remove_background")
  .check_color(background_color, "background_color")
  .check_number(fuzz, "fuzz", min = 0, max = 100)
  .check_max_dimension(max_dimension)

  original <- read_art_image(image)
  working <- preprocess_image(
    original,
    remove_background = remove_background,
    background_color = background_color,
    fuzz = fuzz
  )
  working <- .limit_image_dimension(working, max_dimension)
  info <- .image_info(working)

  result <- .with_seed(seed, {
    sizes <- .circle_sizes(
      image = working,
      n = n,
      size_distribution = size_distribution,
      alpha = alpha,
      beta = beta,
      min_size = min_size,
      max_size = max_size
    )
    layout <- packcircles::circleProgressiveLayout(sizes, sizetype = "area")
    layout <- .scale_circle_layout(layout, info$width, info$height)
    layout$id <- seq_len(nrow(layout))
    layout$hex <- .circle_colors(working, layout, color_method)
    vertices <- packcircles::circleLayoutVertices(layout[, c("x", "y", "radius", "id")], npoints = 28)
    if (!"id" %in% names(vertices)) {
      vertices$id <- rep(layout$id, each = nrow(vertices) / nrow(layout))
    }
    vertices$hex <- layout$hex[match(vertices$id, layout$id)]
    list(layout = layout, vertices = vertices)
  })

  plot <- ggplot2::ggplot(result$vertices) +
    ggplot2::geom_polygon(
      ggplot2::aes(x = x, y = y, group = id, fill = hex),
      color = NA
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, info$width), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(info$height, 0), expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    .plot_background_theme(background)

  .new_image_art(
    original = original,
    processed = result,
    plot = plot,
    image = NULL,
    type = "circle",
    parameters = list(
      n = n,
      size_distribution = size_distribution,
      alpha = alpha,
      beta = beta,
      min_size = min_size,
      max_size = max_size,
      color_method = color_method,
      background = background,
      remove_background = remove_background,
      background_color = background_color,
      fuzz = fuzz,
      seed = seed,
      max_dimension = max_dimension
    ),
    call = call
  )
}

.circle_sizes <- function(
  image,
  n,
  size_distribution,
  alpha,
  beta,
  min_size,
  max_size
) {
  min_size <- min_size %||% 0.5
  max_size <- max_size %||% 4
  if (max_size < min_size) {
    rlang::abort("`max_size` must be greater than or equal to `min_size`.")
  }

  if (identical(size_distribution, "fixed")) {
    values <- rep(1, n)
  } else if (identical(size_distribution, "uniform")) {
    values <- stats::runif(n, min = min_size, max = max_size)
  } else if (identical(size_distribution, "importance")) {
    # 基础版：用图像梯度强度调节尺寸分布，让更多小圆可落在高纹理区域附近。
    gray <- .grayscale_matrix(image)
    gx <- .convolve2d(gray, matrix(c(-1, 0, 1, -2, 0, 2, -1, 0, 1), 3, 3))
    gy <- .convolve2d(gray, matrix(c(-1, -2, -1, 0, 0, 0, 1, 2, 1), 3, 3))
    gradient <- sqrt(gx^2 + gy^2)
    q <- stats::quantile(as.vector(gradient), probs = c(0.25, 0.75), na.rm = TRUE)
    texture <- if (q[2] <= q[1]) 0.5 else (mean(gradient > q[2]) + mean(gradient > q[1])) / 2
    values <- stats::rbeta(n, shape1 = 0.8 + texture, shape2 = 2.5)
    values <- min_size + values * (max_size - min_size)
  } else {
    values <- stats::rbeta(n, shape1 = alpha, shape2 = beta)
    values <- min_size + values * (max_size - min_size)
  }
  pmax(values, .Machine$double.eps)
}

.scale_circle_layout <- function(layout, width, height) {
  x_min <- min(layout$x - layout$radius)
  x_max <- max(layout$x + layout$radius)
  y_min <- min(layout$y - layout$radius)
  y_max <- max(layout$y + layout$radius)
  sx <- width / (x_max - x_min)
  sy <- height / (y_max - y_min)
  radius_scale <- min(sx, sy)
  layout$x <- (layout$x - x_min) * sx
  layout$y <- (layout$y - y_min) * sy
  layout$radius <- layout$radius * radius_scale
  layout
}

.circle_colors <- function(image, layout, color_method) {
  arr <- .image_data_array(image, "rgba")
  width <- dim(arr)[2]
  height <- dim(arr)[3]
  if (identical(color_method, "center")) {
    x <- pmin(width, pmax(1, round(layout$x)))
    y <- pmin(height, pmax(1, round(layout$y)))
    red <- arr[cbind(rep(1L, length(x)), x, y)]
    green <- arr[cbind(rep(2L, length(x)), x, y)]
    blue <- arr[cbind(rep(3L, length(x)), x, y)]
    alpha <- arr[cbind(rep(4L, length(x)), x, y)]
    return(.as_hex(red, green, blue, alpha))
  }

  vapply(seq_len(nrow(layout)), function(i) {
    .circle_color_stat(arr, layout$x[i], layout$y[i], layout$radius[i], color_method)
  }, character(1))
}

.circle_color_stat <- function(arr, cx, cy, radius, color_method) {
  width <- dim(arr)[2]
  height <- dim(arr)[3]
  x_min <- pmax(1, floor(cx - radius))
  x_max <- pmin(width, ceiling(cx + radius))
  y_min <- pmax(1, floor(cy - radius))
  y_max <- pmin(height, ceiling(cy + radius))
  xs <- seq.int(x_min, x_max)
  ys <- seq.int(y_min, y_max)
  grid <- tidyr::expand_grid(x = xs, y = ys)
  keep <- (grid$x - cx)^2 + (grid$y - cy)^2 <= radius^2
  if (!any(keep)) {
    ix <- pmin(width, pmax(1, round(cx)))
    iy <- pmin(height, pmax(1, round(cy)))
    return(.as_hex(arr[1, ix, iy], arr[2, ix, iy], arr[3, ix, iy], arr[4, ix, iy]))
  }
  grid <- grid[keep, , drop = FALSE]
  red <- arr[cbind(rep(1L, nrow(grid)), grid$x, grid$y)]
  green <- arr[cbind(rep(2L, nrow(grid)), grid$x, grid$y)]
  blue <- arr[cbind(rep(3L, nrow(grid)), grid$x, grid$y)]
  alpha <- arr[cbind(rep(4L, nrow(grid)), grid$x, grid$y)]

  fun <- if (identical(color_method, "mean")) mean else stats::median
  .as_hex(fun(red), fun(green), fun(blue), fun(alpha))
}
