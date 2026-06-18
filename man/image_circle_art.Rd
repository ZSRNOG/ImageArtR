\name{image_circle_art}
\alias{image_circle_art}
\title{Create Circle Packing Art}
\usage{
image_circle_art(
  image,
  n = 2000,
  size_distribution = c("beta", "uniform", "fixed", "importance"),
  alpha = 1,
  beta = 1,
  min_size = NULL,
  max_size = NULL,
  color_method = c("center", "mean", "median"),
  background = "transparent",
  remove_background = FALSE,
  background_color = "white",
  fuzz = 10,
  seed = NULL,
  max_dimension = 800
)
}
\arguments{
\item{image}{A path, URL, \code{magick-image}, or \code{imager::cimg} object.}
\item{n}{Number of circles.}
\item{size_distribution}{Circle size distribution.}
\item{alpha, beta}{Shape parameters for beta-distributed sizes.}
\item{min_size, max_size}{Optional relative area bounds.}
\item{color_method}{Color sampling method.}
\item{background}{Plot background color.}
\item{remove_background}{Remove near-background color before sampling?}
\item{background_color}{Background color used by removal.}
\item{fuzz}{Background removal tolerance.}
\item{seed}{Optional random seed.}
\item{max_dimension}{Maximum source image dimension before processing.}
}
\value{An \code{image_art} object.}
\description{
Uses \code{packcircles} to create non-overlapping circle layouts and samples
colors from the source image.
}
\examples{
path <- system.file("extdata", "example.png", package = "ImageArtR")
result <- image_circle_art(path, n = 200, seed = 123)
}
