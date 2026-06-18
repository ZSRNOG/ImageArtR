\name{image_artify}
\alias{image_artify}
\title{Apply an Artistic Transformation by Type}
\usage{
image_artify(
  image,
  type = c(
    "circle", "mosaic", "outline", "sketch", "stipple", "halftone",
    "hex_mosaic", "pop_art", "duotone", "ascii"
  ),
  ...
)
}
\arguments{
\item{image}{A path, URL, \code{magick-image}, or \code{imager::cimg} object.}
\item{type}{Transformation type. Use \code{\link{list_art_styles}()} to see
available styles.}
\item{...}{Arguments passed to the selected transformation function.}
}
\value{An \code{image_art} object.}
\description{
Dispatches to the selected art function, including circle, mosaic, outline,
sketch, stipple, halftone, hex mosaic, pop art, duotone, and ASCII styles.
}
