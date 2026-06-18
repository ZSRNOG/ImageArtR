#' Create an outline image
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param method Edge detector: sobel, laplacian, or approximate canny.
#' @param grayscale Convert input to grayscale before detection?
#' @param blur Gaussian blur sigma before edge detection.
#' @param threshold Optional normalized threshold in `[0, 1]`. `NULL` uses an
#'   automatic quantile threshold.
#' @param line_width Approximate line width in pixels.
#' @param invert Swap line and background colors?
#' @param simplify Morphological simplification amount in `[0, 1]`.
#' @param background Background color.
#' @param line_color Line color.
#' @param max_dimension Maximum source image dimension before processing.
#'
#' @return An `image_art` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_outline(path, method = "sobel")
#' plot(result)
image_outline <- function(
  image,
  method = c("sobel", "laplacian", "canny"),
  grayscale = TRUE,
  blur = 1,
  threshold = NULL,
  line_width = 1,
  invert = FALSE,
  simplify = 0,
  background = "white",
  line_color = "black",
  max_dimension = 1200
) {
  call <- match.call()
  method <- .arg_match(method, c("sobel", "laplacian", "canny"), "method")
  .check_bool(grayscale, "grayscale")
  .check_number(blur, "blur", min = 0)
  .check_number(threshold, "threshold", min = 0, max = 1, allow_null = TRUE)
  .check_number(line_width, "line_width", min = 0.1)
  .check_bool(invert, "invert")
  .check_number(simplify, "simplify", min = 0, max = 1)
  .check_color(background, "background")
  .check_color(line_color, "line_color")
  .check_max_dimension(max_dimension)

  original <- read_art_image(image)
  working <- .limit_image_dimension(original, max_dimension)
  if (grayscale) {
    working <- magick::image_convert(working, colorspace = "gray")
  }
  if (blur > 0) {
    working <- magick::image_blur(working, radius = 0, sigma = blur)
  }
  gray <- .grayscale_matrix(working)
  edges <- .edge_binary_from_matrix(gray, method = method, threshold = threshold)
  edges <- .adjust_edge_width(edges, line_width = line_width, simplify = simplify)

  bg <- background
  fg <- line_color
  if (invert) {
    bg <- line_color
    fg <- background
  }
  out <- .binary_to_magick(edges, background = bg, line_color = fg)

  .new_image_art(
    original = original,
    processed = edges,
    plot = .magick_to_ggplot(out, background = bg),
    image = out,
    type = "outline",
    parameters = list(
      method = method,
      grayscale = grayscale,
      blur = blur,
      threshold = threshold,
      line_width = line_width,
      invert = invert,
      simplify = simplify,
      background = background,
      line_color = line_color,
      max_dimension = max_dimension
    ),
    call = call
  )
}

.edge_binary_from_matrix <- function(
  gray,
  method = c("sobel", "laplacian", "canny"),
  threshold = NULL,
  auto_prob = 0.75
) {
  method <- .arg_match(method, c("sobel", "laplacian", "canny"), "method")
  if (identical(method, "laplacian")) {
    kernel <- matrix(c(0, 1, 0, 1, -4, 1, 0, 1, 0), nrow = 3, byrow = TRUE)
    score <- abs(.convolve2d(gray, kernel))
    score <- .normalize01(score)
    cut <- .edge_threshold(score, threshold, auto_prob)
    return(score >= cut)
  }

  sobel <- .sobel_components(gray)
  score <- .normalize01(sobel$magnitude)
  if (identical(method, "sobel")) {
    cut <- .edge_threshold(score, threshold, auto_prob)
    return(score >= cut)
  }

  suppressed <- .nonmax_suppression(score, sobel$gx, sobel$gy)
  high <- .edge_threshold(suppressed, threshold, auto_prob)
  low <- high * 0.4
  strong <- suppressed >= high
  weak <- suppressed >= low
  edges <- strong
  # 近似 Canny 的双阈值连接，纯 R 版本优先稳定和可测试。
  for (i in seq_len(4)) {
    edges <- edges | (weak & .dilate_binary(edges, radius = 1))
  }
  edges
}

.sobel_components <- function(gray) {
  gx_kernel <- matrix(c(-1, 0, 1, -2, 0, 2, -1, 0, 1), nrow = 3, byrow = TRUE)
  gy_kernel <- matrix(c(-1, -2, -1, 0, 0, 0, 1, 2, 1), nrow = 3, byrow = TRUE)
  gx <- .convolve2d(gray, gx_kernel)
  gy <- .convolve2d(gray, gy_kernel)
  list(gx = gx, gy = gy, magnitude = sqrt(gx^2 + gy^2))
}

.normalize01 <- function(mat) {
  range <- range(mat, finite = TRUE, na.rm = TRUE)
  if (!is.finite(range[1]) || !is.finite(range[2]) || range[2] <= range[1]) {
    return(matrix(0, nrow = nrow(mat), ncol = ncol(mat)))
  }
  (mat - range[1]) / (range[2] - range[1])
}

.edge_threshold <- function(score, threshold, auto_prob) {
  if (!is.null(threshold)) {
    return(threshold)
  }
  positive <- score[is.finite(score) & score > 0]
  if (length(positive) == 0) {
    return(1)
  }
  as.numeric(stats::quantile(positive, probs = auto_prob, names = FALSE))
}

.nonmax_suppression <- function(magnitude, gx, gy) {
  angle <- (atan2(gy, gx) * 180 / pi) %% 180
  direction <- matrix(0L, nrow = nrow(angle), ncol = ncol(angle))
  direction[(angle >= 22.5 & angle < 67.5)] <- 45L
  direction[(angle >= 67.5 & angle < 112.5)] <- 90L
  direction[(angle >= 112.5 & angle < 157.5)] <- 135L

  keep <- matrix(FALSE, nrow = nrow(magnitude), ncol = ncol(magnitude))
  checks <- list(
    `0` = list(a = .shift_matrix(magnitude, 0, -1), b = .shift_matrix(magnitude, 0, 1)),
    `45` = list(a = .shift_matrix(magnitude, -1, 1), b = .shift_matrix(magnitude, 1, -1)),
    `90` = list(a = .shift_matrix(magnitude, -1, 0), b = .shift_matrix(magnitude, 1, 0)),
    `135` = list(a = .shift_matrix(magnitude, -1, -1), b = .shift_matrix(magnitude, 1, 1))
  )
  for (name in names(checks)) {
    dir <- as.integer(name)
    local <- direction == dir
    keep <- keep | (local & magnitude >= checks[[name]]$a & magnitude >= checks[[name]]$b)
  }
  out <- magnitude
  out[!keep] <- 0
  out
}

.adjust_edge_width <- function(edges, line_width, simplify) {
  out <- edges > 0
  if (simplify > 0) {
    rounds <- max(1, ceiling(simplify * 2))
    out <- .dilate_binary(.erode_binary(out, radius = rounds), radius = rounds)
  }
  if (line_width > 1) {
    out <- .dilate_binary(out, radius = ceiling(line_width) - 1)
  }
  out
}
