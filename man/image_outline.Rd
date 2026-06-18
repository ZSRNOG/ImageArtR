\name{image_outline}
\alias{image_outline}
\title{Create an Outline Image}
\usage{
image_outline(
  image,
  method = c("sobel", "laplacian", "canny"),
  grayscale = TRUE,
  blur = 1,
  threshold = NULL,
  line_width = 1,
  invert = FALSE,
  simplify = 0,
  background = "white",
  line_color = "black",
  max_dimension = 1200
)
}
\arguments{
\item{image}{A path, URL, \code{magick-image}, or \code{imager::cimg} object.}
\item{method}{Edge detector.}
\item{grayscale}{Convert input to grayscale before detection?}
\item{blur}{Gaussian blur sigma.}
\item{threshold}{Optional normalized threshold in \code{[0, 1]}.}
\item{line_width}{Approximate line width in pixels.}
\item{invert}{Swap line and background colors?}
\item{simplify}{Morphological simplification amount in \code{[0, 1]}.}
\item{background}{Background color.}
\item{line_color}{Line color.}
\item{max_dimension}{Maximum source image dimension before processing.}
}
\value{An \code{image_art} object.}
\description{
Creates Sobel, Laplacian, or approximate Canny-style outlines.
}
