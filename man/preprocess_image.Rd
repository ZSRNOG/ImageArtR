\name{preprocess_image}
\alias{preprocess_image}
\title{Preprocess an Image}
\usage{
preprocess_image(
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
)
}
\arguments{
\item{image}{A path, URL, \code{magick-image}, or \code{imager::cimg} object.}
\item{resize}{Optional resize geometry, maximum dimension, or width-height pair.}
\item{crop}{Optional crop geometry or \code{c(width, height, x, y)}.}
\item{grayscale}{Convert to grayscale?}
\item{normalize}{Apply ImageMagick normalization?}
\item{blur}{Gaussian blur sigma.}
\item{contrast}{Contrast multiplier.}
\item{remove_background}{Make pixels close to \code{background_color} transparent?}
\item{background_color}{Background color used for removal.}
\item{fuzz}{Color tolerance for background removal.}
}
\value{A preprocessed \code{magick-image} object.}
\description{
Resizes, crops, grayscales, normalizes, blurs, adjusts contrast, and optionally
removes a near-solid background color.
}
