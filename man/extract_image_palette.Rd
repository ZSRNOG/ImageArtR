\name{extract_image_palette}
\alias{extract_image_palette}
\title{Extract an Image Palette}
\usage{
extract_image_palette(
  image,
  n = 8,
  colorspace = c("RGB", "sRGB", "Lab", "HSL", "HSV"),
  sort_by = c("frequency", "hue", "value", "saturation"),
  remove_transparent = TRUE
)
}
\arguments{
\item{image}{A path, URL, \code{magick-image}, or \code{imager::cimg} object.}
\item{n}{Number of colors to return.}
\item{colorspace}{Clustering colorspace.}
\item{sort_by}{Sorting method.}
\item{remove_transparent}{Ignore fully transparent pixels?}
}
\value{
A tibble with \code{hex}, RGB, HSV, frequency, proportion, and colorspace
columns.
}
\description{
Extracts dominant colors by clustering image pixels.
}
