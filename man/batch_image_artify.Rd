\name{batch_image_artify}
\alias{batch_image_artify}
\title{Batch Process Images}
\usage{
batch_image_artify(
  images,
  output_dir,
  type = c(
    "circle", "mosaic", "outline", "sketch", "stipple", "halftone",
    "hex_mosaic", "pop_art", "duotone", "ascii"
  ),
  recursive = FALSE,
  pattern = "\\\\.(png|jpe?g|tiff?|bmp|gif)$",
  format = "png",
  overwrite = FALSE,
  parallel = FALSE,
  ...
)
}
\arguments{
\item{images}{A vector of image paths, or a single directory path.}
\item{output_dir}{Directory for generated files.}
\item{type}{Transformation type. Use \code{\link{list_art_styles}()} to see
available styles.}
\item{recursive}{Recursively read image files when \code{images} is a directory?}
\item{pattern}{File pattern used when \code{images} is a directory.}
\item{format}{Output format.}
\item{overwrite}{Overwrite existing files?}
\item{parallel}{Reserved for a future version; must be \code{FALSE}.}
\item{...}{Arguments passed to \code{image_artify()}.}
}
\value{A tibble log with input, output, success, elapsed time, and error.}
\description{
Sequentially processes image files and records failures without stopping the
whole batch.
}
