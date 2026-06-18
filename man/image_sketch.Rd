\name{image_sketch}
\alias{image_sketch}
\title{Create Sketch Art}
\usage{
image_sketch(
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
)
}
\arguments{
\item{image}{A path, URL, \code{magick-image}, or \code{imager::cimg} object.}
\item{style}{Sketch style.}
\item{detail}{Detail level in \code{[0, 1]}.}
\item{smooth}{Smoothing strength.}
\item{contrast}{Contrast multiplier.}
\item{line_width}{Approximate line width.}
\item{threshold}{Optional normalized threshold.}
\item{shading}{Keep tonal shading for pencil sketches?}
\item{invert}{Invert output tones?}
\item{background}{Background color.}
\item{max_dimension}{Maximum source image dimension before processing.}
}
\value{An \code{image_art} object.}
\description{
Creates line, pencil, and cartoon sketch styles.
}
