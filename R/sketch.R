#' Create line, pencil, or cartoon sketch art
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param style Sketch style: line, pencil, or cartoon.
#' @param detail Detail level in `[0, 1]`.
#' @param smooth Smoothing strength.
#' @param contrast Contrast multiplier.
#' @param line_width Approximate line width in pixels.
#' @param threshold Optional normalized edge threshold in `[0, 1]`.
#' @param shading Keep tonal shading for pencil sketches?
#' @param invert Invert the output tones?
#' @param background Background color.
#' @param max_dimension Maximum source image dimension before processing.
#'
#' @return An `image_art` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' result <- image_sketch(path, style = "pencil")
#' plot(result)
image_sketch <- function(
  image,
  style = c("line", "pencil", "cartoon"),
  detail = 0.5,
  smooth = 1,
  contrast = 1.2,
  line_width = 1,
  threshold = NULL,
  shading = TRUE,
  invert = FALSE,
  background = "white",
  max_dimension = 1200
) {
  call <- match.call()
  style <- .arg_match(style, c("line", "pencil", "cartoon"), "style")
  .check_number(detail, "detail", min = 0, max = 1)
  .check_number(smooth, "smooth", min = 0)
  .check_number(contrast, "contrast", min = 0.1, max = 10)
  .check_number(line_width, "line_width", min = 0.1)
  .check_number(threshold, "threshold", min = 0, max = 1, allow_null = TRUE)
  .check_bool(shading, "shading")
  .check_bool(invert, "invert")
  .check_color(background, "background")
  .check_max_dimension(max_dimension)

  original <- read_art_image(image)
  working <- .limit_image_dimension(original, max_dimension)
  out <- switch(style,
    line = .sketch_line(working, detail, smooth, line_width, threshold, invert, background),
    pencil = .sketch_pencil(working, smooth, contrast, shading, invert),
    cartoon = .sketch_cartoon(working, detail, smooth, threshold, line_width)
  )

  .new_image_art(
    original = original,
    processed = list(style = style),
    plot = .magick_to_ggplot(out, background = background),
    image = out,
    type = "sketch",
    parameters = list(
      style = style,
      detail = detail,
      smooth = smooth,
      contrast = contrast,
      line_width = line_width,
      threshold = threshold,
      shading = shading,
      invert = invert,
      background = background,
      max_dimension = max_dimension
    ),
    call = call
  )
}

.sketch_line <- function(image, detail, smooth, line_width, threshold, invert, background) {
  working <- magick::image_convert(image, colorspace = "gray")
  if (smooth > 0) {
    working <- magick::image_blur(working, radius = 0, sigma = smooth)
  }
  gray <- .grayscale_matrix(working)
  auto_prob <- 0.9 - detail * 0.35
  edges <- .edge_binary_from_matrix(gray, method = "sobel", threshold = threshold, auto_prob = auto_prob)
  edges <- .adjust_edge_width(edges, line_width = line_width, simplify = (1 - detail) * 0.5)
  bg <- if (invert) "black" else background
  fg <- if (invert) background else "black"
  .binary_to_magick(edges, background = bg, line_color = fg)
}

.sketch_pencil <- function(image, smooth, contrast, shading, invert) {
  gray <- .grayscale_matrix(magick::image_convert(image, colorspace = "gray"))
  inverted <- 1 - gray
  blur_sigma <- max(0.1, smooth * 4)
  blurred <- .grayscale_matrix(magick::image_blur(.gray_to_magick(inverted), radius = 0, sigma = blur_sigma))
  dodge <- matrix(
    pmin(1, as.vector(gray) / pmax(1 - as.vector(blurred), 0.02)),
    nrow = nrow(gray),
    ncol = ncol(gray)
  )
  if (shading) {
    dodge <- .clip01_matrix(0.82 * dodge + 0.18 * gray)
  }
  dodge <- .clip01_matrix((dodge - 0.5) * contrast + 0.5)
  if (invert) {
    dodge <- 1 - dodge
  }
  .gray_to_magick(dodge)
}

.clip01_matrix <- function(mat) {
  matrix(
    pmin(1, pmax(0, as.vector(mat))),
    nrow = nrow(mat),
    ncol = ncol(mat)
  )
}

.sketch_cartoon <- function(image, detail, smooth, threshold, line_width) {
  working <- image
  if (smooth > 0) {
    working <- magick::image_blur(working, radius = 0, sigma = smooth)
  }
  color_count <- max(4, round(6 + detail * 26))
  quantized <- magick::image_quantize(working, max = color_count, dither = FALSE)
  gray <- .grayscale_matrix(magick::image_convert(working, colorspace = "gray"))
  auto_prob <- 0.88 - detail * 0.3
  edges <- .edge_binary_from_matrix(gray, method = "sobel", threshold = threshold, auto_prob = auto_prob)
  edges <- .adjust_edge_width(edges, line_width = line_width, simplify = 0)

  arr <- .image_data_array(quantized, "rgba")
  red <- t(arr[1, , ])
  green <- t(arr[2, , ])
  blue <- t(arr[3, , ])
  alpha <- t(arr[4, , ])
  red[edges] <- 0
  green[edges] <- 0
  blue[edges] <- 0
  alpha[edges] <- 255
  .rgba_matrices_to_magick(red, green, blue, alpha)
}
