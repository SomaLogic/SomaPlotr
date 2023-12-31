% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style-soma.R
\name{style-soma}
\alias{style-soma}
\alias{palette_soma}
\alias{scale_color_soma}
\alias{scale_fill_soma}
\alias{scale_continuous_color_soma}
\alias{scale_continuous_fill_soma}
\title{SomaLogic Primary Color Palette}
\source{
Influenced by:
\url{https://github.com/jrnold/ggthemes}
}
\usage{
palette_soma(n, random_order = FALSE)

scale_color_soma(...)

scale_fill_soma(...)

scale_continuous_color_soma(
  low = soma_colors2$pink,
  high = soma_colors2$blue,
  ...
)

scale_continuous_fill_soma(
  low = soma_colors2$pink,
  high = soma_colors2$blue,
  ...
)
}
\arguments{
\item{n}{number of colors. This palette is set to the
\link{soma_colors2} object, which is currently 8 elements
in length. The palette is recycled as necessary.}

\item{random_order}{Logical. If the palette should be randomly
sampled prior to rendering each plot to generate more colorful plots.}

\item{...}{Arguments passed to \code{\link[ggplot2:discrete_scale]{ggplot2::discrete_scale()}},
\code{\link[ggplot2:scale_gradient]{ggplot2::scale_fill_gradient()}}, or \code{\link[ggplot2:scale_gradient]{ggplot2::scale_color_gradient()}}.}

\item{low, high}{Colours for low and high ends of the gradient.}
}
\description{
This is the primary color palette used in \pkg{SomaPlotr}.
It is used in the various \verb{scale_*_soma()} functions and
forms the basis of the SomaLogic theme.
}
\section{Functions}{
\itemize{
\item \code{scale_color_soma()}: color \emph{discrete} scale for SomaLogic graphics.

\item \code{scale_fill_soma()}: fill \emph{discrete} scale for SomaLogic graphics.

\item \code{scale_continuous_color_soma()}: color \emph{continuous} scale for SomaLogic graphics.

\item \code{scale_continuous_fill_soma()}: fill \emph{continuous} scale for SomaLogic graphics.

}}
\seealso{
Other palettes: 
\code{\link{style-gender}},
\code{\link{style-greys}},
\code{\link{style-risk}}
}
\concept{palettes}
