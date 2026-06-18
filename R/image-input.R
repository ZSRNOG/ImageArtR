#' Read an image for ImageArtR
#'
#' @description
#' Reads a local path, `file://` URL, HTTP(S) URL, `magick-image`, or
#' `imager::cimg` object into a standard `magick-image` object used internally
#' by ImageArtR.
#'
#' @param image A file path, URL, `magick-image`, or `imager::cimg` object.
#'
#' @return A `magick-image` object.
#' @export
#'
#' @examples
#' path <- system.file("extdata", "example.png", package = "ImageArtR")
#' img <- read_art_image(path)
read_art_image <- function(image) {
  if (.is_magick_image(image)) {
    return(image)
  }

  if (.is_cimg(image)) {
    raster <- grDevices::as.raster(image)
    return(magick::image_read(raster))
  }

  if (!is.character(image) || length(image) != 1 || is.na(image)) {
    rlang::abort("`image` must be a path, URL, magick-image, or imager cimg object.")
  }

  if (.is_file_url(image)) {
    image <- .file_url_to_path(image)
  }

  if (.is_url(image)) {
    return(tryCatch(
      magick::image_read(image),
      error = function(e) {
        rlang::abort(
          sprintf("Could not read image URL `%s`: %s", image, conditionMessage(e)),
          parent = e
        )
      }
    ))
  }

  if (!file.exists(image)) {
    rlang::abort(sprintf("Image path does not exist: `%s`.", image))
  }

  tryCatch(
    magick::image_read(image),
    error = function(e) {
      rlang::abort(
        sprintf("Could not read image file `%s`: %s", image, conditionMessage(e)),
        parent = e
      )
    }
  )
}
