#' Save an image art object
#'
#' @param x An `image_art` object.
#' @param path Output path. Supported extensions are png, jpg, jpeg, tif, tiff,
#'   pdf, svg, txt, and html.
#' @param width Optional output width in inches for plot-based output.
#' @param height Optional output height in inches for plot-based output.
#' @param dpi Plot resolution used by `ggplot2::ggsave()`.
#' @param overwrite Overwrite an existing output file?
#' @param ... Additional arguments passed to `ggplot2::ggsave()` when saving a
#'   plot.
#'
#' @return The output path, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' result <- image_mosaic_art(system.file("extdata", "example.png", package = "ImageArtR"))
#' save_image_art(result, "mosaic.png")
#' }
save_image_art <- function(
  x,
  path,
  width = NULL,
  height = NULL,
  dpi = 300,
  overwrite = FALSE,
  ...
) {
  if (!inherits(x, "image_art")) {
    rlang::abort("`x` must be an image_art object.")
  }
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    rlang::abort("`path` must be a single output path.")
  }
  ext <- tolower(tools::file_ext(path))
  supported <- c("png", "jpg", "jpeg", "tif", "tiff", "pdf", "svg", "txt", "html")
  if (!ext %in% supported) {
    rlang::abort(sprintf("Unsupported output extension `%s`.", ext))
  }
  .check_bool(overwrite, "overwrite")
  .check_number(width, "width", min = 0.1, allow_null = TRUE)
  .check_number(height, "height", min = 0.1, allow_null = TRUE)
  .check_number(dpi, "dpi", min = 1)
  if (file.exists(path) && !overwrite) {
    rlang::abort(sprintf("Output file already exists: `%s`.", path))
  }

  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (ext %in% c("txt", "html")) {
    if (!identical(x$type, "ascii")) {
      rlang::abort("Only ASCII image_art objects can be saved as `.txt` or `.html`.")
    }
    write_ascii_art(x, path, overwrite = overwrite)
    return(invisible(path))
  }

  raster_ext <- c("png", "jpg", "jpeg", "tif", "tiff")
  if (!is.null(x$image) && ext %in% raster_ext) {
    format <- if (ext == "jpg") "jpeg" else ext
    magick::image_write(x$image, path = path, format = format)
    return(invisible(path))
  }

  if (is.null(x$plot) && identical(x$type, "ascii")) {
    rlang::abort("ASCII objects created with `output = \"text\"` can only be saved as `.txt` or `.html`.")
  }
  p <- x$plot %||% .magick_to_ggplot(x$image)
  size <- .save_plot_size(x, width, height, dpi)
  ggplot2::ggsave(
    filename = path,
    plot = p,
    width = size$width,
    height = size$height,
    dpi = dpi,
    bg = "transparent",
    ...
  )
  invisible(path)
}

.save_plot_size <- function(x, width, height, dpi) {
  info <- tryCatch(
    {
      if (!is.null(x$image)) .image_info(x$image) else .image_info(x$original)
    },
    error = function(e) NULL
  )
  if (is.null(info)) {
    return(list(width = width %||% 7, height = height %||% 7))
  }
  aspect <- info$height / info$width
  out_width <- width %||% max(3, min(10, info$width / dpi))
  out_height <- height %||% (out_width * aspect)
  list(width = out_width, height = out_height)
}
