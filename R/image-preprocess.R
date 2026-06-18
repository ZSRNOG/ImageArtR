#' Preprocess an image
#'
#' @param image A path, URL, `magick-image`, or `imager::cimg` object.
#' @param resize `NULL`, a single maximum dimension, a length-two numeric
#'   vector of width and height, or an ImageMagick geometry string.
#' @param crop `NULL`, an ImageMagick geometry string, or numeric
#'   `c(width, height, x, y)`.
#' @param grayscale Convert to grayscale?
#' @param normalize Apply ImageMagick normalization?
#' @param blur Gaussian blur sigma. Use `0` for no blur.
#' @param contrast Contrast multiplier. Values above 1 sharpen contrast; values
#'   below 1 soften it.
#' @param remove_background Make pixels close to `background_color` transparent?
#' @param background_color Background color used by `remove_background`.
#' @param fuzz Color tolerance for background removal.
#'
#' @return A preprocessed `magick-image` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' img <- preprocess_image(path, resize = 200, grayscale = TRUE)
preprocess_image <- function(
  image,
  resize = NULL,
  crop = NULL,
  grayscale = FALSE,
  normalize = FALSE,
  blur = 0,
  contrast = 1,
  remove_background = FALSE,
  background_color = "white",
  fuzz = 10
) {
  .check_bool(grayscale, "grayscale")
  .check_bool(normalize, "normalize")
  .check_bool(remove_background, "remove_background")
  .check_number(blur, "blur", min = 0)
  .check_number(contrast, "contrast", min = 0.1, max = 10)
  .check_number(fuzz, "fuzz", min = 0, max = 100)
  .check_color(background_color, "background_color")

  out <- read_art_image(image)
  out <- .limit_image_dimension(out, 2000)

  if (!is.null(resize)) {
    geometry <- .resize_geometry(resize)
    out <- magick::image_resize(out, geometry = geometry)
  }

  if (!is.null(crop)) {
    geometry <- .crop_geometry(crop)
    out <- magick::image_crop(out, geometry = geometry)
  }

  if (remove_background) {
    out <- magick::image_transparent(out, color = background_color, fuzz = fuzz)
  }

  if (grayscale) {
    out <- magick::image_convert(out, colorspace = "gray")
  }

  if (normalize) {
    out <- magick::image_normalize(out)
  }

  if (blur > 0) {
    out <- magick::image_blur(out, radius = 0, sigma = blur)
  }

  if (!identical(contrast, 1)) {
    steps <- max(1, round(abs(contrast - 1) * 5))
    for (i in seq_len(steps)) {
      out <- magick::image_contrast(out, sharpen = contrast > 1)
    }
  }

  out
}

.resize_geometry <- function(resize) {
  if (is.character(resize) && length(resize) == 1) {
    return(resize)
  }
  if (!is.numeric(resize) || !(length(resize) %in% c(1, 2)) || any(is.na(resize))) {
    rlang::abort("`resize` must be NULL, a geometry string, one number, or two numbers.")
  }
  if (any(resize <= 0)) {
    rlang::abort("`resize` values must be positive.")
  }
  if (length(resize) == 1) {
    sprintf("%sx%s>", as.integer(resize), as.integer(resize))
  } else {
    sprintf("%sx%s!", as.integer(resize[1]), as.integer(resize[2]))
  }
}

.crop_geometry <- function(crop) {
  if (is.character(crop) && length(crop) == 1) {
    return(crop)
  }
  if (!is.numeric(crop) || length(crop) != 4 || any(is.na(crop))) {
    rlang::abort("`crop` must be NULL, a geometry string, or c(width, height, x, y).")
  }
  if (crop[1] <= 0 || crop[2] <= 0) {
    rlang::abort("`crop` width and height must be positive.")
  }
  sprintf("%sx%s+%s+%s", as.integer(crop[1]), as.integer(crop[2]), as.integer(crop[3]), as.integer(crop[4]))
}
