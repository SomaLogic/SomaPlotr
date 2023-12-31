% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style-gender.R
\name{style-gender}
\alias{style-gender}
\alias{palette_soma_gender}
\alias{scale_color_soma_gender}
\alias{scale_fill_soma_gender}
\title{SomaLogic Sex/Gender Color Palette}
\usage{
palette_soma_gender(n = 8)

scale_color_soma_gender(...)

scale_fill_soma_gender(...)
}
\arguments{
\item{n}{number of colors. This palette is set to the
\link{soma_colors2} object, which is currently 8 elements
in length, but with the elements reordered.
The palette is recycled as necessary.}

\item{...}{Arguments passed to \code{\link[ggplot2:discrete_scale]{ggplot2::discrete_scale()}},
\code{\link[ggplot2:scale_gradient]{ggplot2::scale_fill_gradient()}}, or \code{\link[ggplot2:scale_gradient]{ggplot2::scale_color_gradient()}}.}
}
\description{
The color palette of official SomaLogic sex/gender colors:
\tabular{lll}{
Sex      \tab Color  \tab Hex-color \cr
female   \tab yellow \tab "#FF8B04" \cr
male     \tab green  \tab "#14753D" \cr
}
}
\section{Functions}{
\itemize{
\item \code{scale_color_soma_gender()}: color \emph{discrete} scale to use for sex/gender.

\item \code{scale_fill_soma_gender()}: fill \emph{discrete} scale to use for sex/gender.

}}
\seealso{
Other palettes: 
\code{\link{style-greys}},
\code{\link{style-risk}},
\code{\link{style-soma}}
}
\concept{palettes}
