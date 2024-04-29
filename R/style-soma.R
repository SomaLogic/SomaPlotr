#' SomaLogic Primary Color Palette
#'
#' This is the primary color palette used in \pkg{SomaPlotr}.
#' It is used in the various `scale_*_soma()` functions and
#' forms the basis of the SomaLogic theme.
#'
#' @name style-soma
#' @family palettes
#' @param n number of colors. This palette is set to the
#'   [soma_colors2] object, which is currently 8 elements
#'   in length. The palette is recycled as necessary.
#' @param random_order Logical. If the palette should be randomly
#'   sampled prior to rendering each plot to generate more colorful plots.
#' @source Influenced by:
#'   [https://github.com/jrnold/ggthemes](https://github.com/jrnold/ggthemes)
#' @export
palette_soma <- function(n, random_order = FALSE) {
  cols <- unlist(soma_colors2, use.names = FALSE)
  if ( isTRUE(random_order) ) {
    cols <- sample(cols)
  }
  rep_len(cols, length.out = n)
}

#' @describeIn style-soma
#'   color _discrete_ scale for SomaLogic graphics.
#' @param ... Arguments passed to [ggplot2::discrete_scale()],
#'   [ggplot2::scale_fill_gradient()], or [ggplot2::scale_color_gradient()].
#' @export
scale_color_soma <- function(...) {
  ggplot2::discrete_scale(aesthetics = "color", palette = palette_soma, ...)
}


#' @describeIn style-soma
#'   fill _discrete_ scale for SomaLogic graphics.
#' @export
scale_fill_soma <- function(...) {
  ggplot2::discrete_scale(aesthetics = "fill", palette = palette_soma, ...)
}


#' @describeIn style-soma
#'   color _continuous_ scale for SomaLogic graphics.
#' @inheritParams ggplot2::scale_color_gradient
#' @export
scale_continuous_color_soma <- function(low  = soma_colors2$pink,
                                        high = soma_colors2$blue, ...) {
  ggplot2::scale_color_gradient(low = low, high = high, ...)
}


#' @describeIn style-soma
#'   fill _continuous_ scale for SomaLogic graphics.
#' @inheritParams ggplot2::scale_fill_gradient
#' @export
scale_continuous_fill_soma <- function(low  = soma_colors2$pink,
                                       high = soma_colors2$blue, ...) {
  ggplot2::scale_fill_gradient(low = low, high = high, ...)
}
