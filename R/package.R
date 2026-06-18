#' ImageArtR: turn images into artistic graphics
#'
#' ImageArtR reads images from files, URLs, `magick-image` objects, or
#' `imager::cimg` objects, then turns them into circle packing art, mosaics,
#' outlines, sketches, cartoons, and color palettes.
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "alpha", "angle", "blue", "cell_x", "cell_y", "channel", "char",
  "cluster", "frequency", "green", "hex", "hue", "id", "index",
  "label", "line_size", "point_size",
  "proportion", "red", "radius", "saturation", "size", "value", "x",
  "x_end", "x_max", "x_min", "x_start", "y", "y_end", "y_max", "y_min",
  "y_start"
))
