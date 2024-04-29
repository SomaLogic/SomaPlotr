#' SomaLogic Greyscale Color Palette
#'
#' The color palette of official SomaLogic greyscale colors.
#'
#' @name style-greys
#' @family palettes
#' @param n number of colors. This palette is set to the
#'   [soma_colors_greys] object, which is currently 6 elements
#'   in length.
#'   The palette is recycled as necessary.
#' @importFrom utils head
#' @export
palette_soma_greys <- function(n) {
  cols <- unlist(soma_colors_greys, use.names = FALSE)
  rep_len(cols, length.out = n)
}

#' @describeIn style-greys
#'   color _discrete_ scale for greyscale graphics.
#' @inheritParams style-soma
#' @export
scale_color_soma_greys <- function(...) {
  ggplot2::discrete_scale(aesthetics = "color",
                          palette    = palette_soma_greys, ...)
}

#' @describeIn style-greys
#'   fill _discrete_ scale for greyscale graphics.
#' @inheritParams style-soma
#' @export
scale_fill_soma_greys <- function(...) {
  ggplot2::discrete_scale(aesthetics = "fill",
                          palette    = palette_soma_greys, ...)
}
