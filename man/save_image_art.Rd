\name{save_image_art}
\alias{save_image_art}
\title{Save an Image Art Object}
\usage{
save_image_art(
  x,
  path,
  width = NULL,
  height = NULL,
  dpi = 300,
  overwrite = FALSE,
  ...
)
}
\arguments{
\item{x}{An \code{image_art} object.}
\item{path}{Output path ending in png, jpg, jpeg, tif, tiff, pdf, svg, txt, or html.}
\item{width, height}{Optional output size in inches for plot-based output.}
\item{dpi}{Plot resolution used by \code{ggplot2::ggsave()}.}
\item{overwrite}{Overwrite an existing output file?}
\item{...}{Additional arguments passed to \code{ggplot2::ggsave()}.}
}
\value{The output path, invisibly.}
\description{
Saves plot-based or raster image art objects to PNG, JPEG, TIFF, PDF, or SVG.
ASCII objects can also be saved to TXT or HTML.
}
