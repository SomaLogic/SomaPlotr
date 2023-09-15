#' SomaLogic Sex/Gender Color Palette
#'
#' The color palette of official SomaLogic sex/gender colors:
#' \tabular{lll}{
#'   Sex      \tab Color  \tab Hex-color \cr
#'   female   \tab yellow \tab "#FF8B04" \cr
#'   male     \tab green  \tab "#14753D" \cr
#' }
#'
#' @name style-gender
#' @family palettes
#' @param n number of colors. This palette is set to the
#'   [soma_colors2] object, which is currently 8 elements
#'   in length, but with the elements reordered.
#'   The palette is recycled as necessary.
#' @export
palette_soma_gender <- function(n = 8) {
  cols <- c(soma_colors2$yellow, soma_colors2$green,
            soma_colors2$lightblue, soma_colors2$purple,
            soma_colors2$turq, soma_colors2$pink,
            soma_colors2$blue, soma_colors2$teal)
  rep_len(cols, length.out = n)
}

#' @describeIn style-gender
#'   color _discrete_ scale to use for sex/gender.
#' @inheritParams style-soma
#' @export
scale_color_soma_gender <- function(...) {
  ggplot2::discrete_scale(aesthetics = "color",
                          scale_name = "soma_gender",
                          palette    = palette_soma_gender, ...)
}

#' @describeIn style-gender
#'   fill _discrete_ scale to use for sex/gender.
#' @inheritParams style-soma
#' @export
scale_fill_soma_gender <- function(...) {
  ggplot2::discrete_scale(aesthetics = "fill",
                          scale_name = "soma_gender",
                          palette    = palette_soma_gender, ...)
}
