\name{read_art_image}
\alias{read_art_image}
\title{Read an Image for ImageArtR}
\usage{
read_art_image(image)
}
\arguments{
\item{image}{A file path, \code{file://} URL, HTTP(S) URL,
\code{magick-image}, or \code{imager::cimg} object.}
}
\value{A \code{magick-image} object.}
\description{
Reads image inputs into the standard internal image representation used by
ImageArtR.
}
\examples{
path <- system.file("extdata", "example.png", package = "ImageArtR")
img <- read_art_image(path)
}
