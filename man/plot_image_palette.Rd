\name{plot_image_palette}
\alias{plot_image_palette}
\title{Plot an Image Palette}
\usage{
plot_image_palette(palette, ...)
}
\arguments{
\item{palette}{A palette data frame returned by \code{extract_image_palette()}, or
an image input accepted by \code{extract_image_palette()}.}
\item{...}{Passed to \code{extract_image_palette()} when \code{palette} is an image.}
}
\value{A \code{ggplot2} object.}
\description{
Draws palette colors, hex codes, and color proportions.
}
