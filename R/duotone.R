#' Create a duotone image
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param shadow Color used for dark tones.
#' @param highlight Color used for light tones.
#' @param midpoint Optional luminance midpoint in `[0, 1]`.
#' @param contrast Contrast multiplier.
#' @param gamma Gamma adjustment.
#' @param preserve_alpha Preserve the source alpha channel?
#' @param max_dimension Maximum source image dimension before processing.
#' @param colorspace Interpolation colorspace: RGB or Lab.
#'
#' @return An `image_art` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_duotone(path, shadow = "#172A3A", highlight = "#F4D35E")
#' plot(result)
image_duotone <- function(
  image,
  shadow = "#172A3A",
  highlight = "#F4D35E",
  midpoint = NULL,
  contrast = 1,
  gamma = 1,
  preserve_alpha = TRUE,
  max_dimension = 1200,
  colorspace = c("RGB", "Lab")
) {
  call <- match.call()
  colorspace <- .arg_match(colorspace, c("RGB", "Lab"), "colorspace")
  .check_color(shadow, "shadow")
  .check_color(highlight, "highlight")
  .check_number(midpoint, "midpoint", min = 0.001, max = 0.999, allow_null = TRUE)
  .check_number(contrast, "contrast", min = 0.1, max = 10)
  .check_number(gamma, "gamma", min = 0.05, max = 10)
  .check_bool(preserve_alpha, "preserve_alpha")
  .check_max_dimension(max_dimension)

  original <- read_art_image(image)
  working <- .limit_image_dimension(original, max_dimension)
  gray <- .clip01((.image_luminance(working) - 0.5) * contrast + 0.5)
  gray <- .clip01(gray^gamma)
  if (!is.null(midpoint)) {
    gray <- .duotone_midpoint(gray, midpoint)
  }

  colors <- .interpolate_colors(as.vector(gray), shadow, highlight, colorspace = colorspace)
  rgb <- grDevices::col2rgb(colors)
  red <- matrix(rgb[1, ], nrow = nrow(gray), ncol = ncol(gray))
  green <- matrix(rgb[2, ], nrow = nrow(gray), ncol = ncol(gray))
  blue <- matrix(rgb[3, ], nrow = nrow(gray), ncol = ncol(gray))
  alpha <- if (preserve_alpha) {
    t(.image_data_array(working, "rgba")[4, , ])
  } else {
    matrix(255, nrow = nrow(gray), ncol = ncol(gray))
  }
  out <- .rgba_matrices_to_magick(red, green, blue, alpha)

  .make_image_art(
    original = original,
    processed = list(luminance = gray, shadow = shadow, highlight = highlight),
    plot = .magick_to_ggplot(out),
    image = out,
    type = "duotone",
    parameters = list(
      shadow = shadow,
      highlight = highlight,
      midpoint = midpoint,
      contrast = contrast,
      gamma = gamma,
      preserve_alpha = preserve_alpha,
      max_dimension = max_dimension,
      colorspace = colorspace
    ),
    call = call
  )
}

.duotone_midpoint <- function(value, midpoint) {
  out <- ifelse(
    value < midpoint,
    0.5 * value / midpoint,
    0.5 + 0.5 * (value - midpoint) / (1 - midpoint)
  )
  .clip01(out)
}
