#' SomaLogic Risk Category Color Palette
#'
#' The color palette of official SomaLogic risk categories/trends.
#'
#' @name style-risk
#' @family palettes
#' @param direction Captures both number of colors and
#'   direction of those colors based on [soma_colors_risk] colors,
#'   which are currently 5 colors from high to low risk.
#'   The palette is recycled as necessary.
#'   A positive number maintains order from high (red) to
#'   low (green) risk. A negative number reverses that order.
#'   There is special handling for `n = 2, 3, and 4` colors for
#'   both directions. For more custom ordering, use
#'   [scale_color_manual()] or [scale_fill_manual()] with
#'   the [soma_colors_risk] colors.
#' @param n Not currently used. Included to make palette function work
#'   with [scale_color_soma_risk()] and [scale_fill_soma_risk()].
#' @export
palette_soma_risk <- function(direction = 4, n) {

  # special handling for cases from 2-4 to correctly order colors
  if ( direction == 2 ) {
    cols <- c(soma_colors_risk$red, soma_colors_risk$green)
  } else if ( direction == 3 ) {
    cols <- c(soma_colors_risk$red, soma_colors_risk$orange, soma_colors_risk$green)
  } else if ( direction == 4 ) {
    cols <- c(soma_colors_risk$red, soma_colors_risk$orange,
              soma_colors_risk$yellow, soma_colors_risk$green)
  } else if ( direction == -2 ) {
    cols <- c(soma_colors_risk$green, soma_colors_risk$red)
  } else if ( direction == -3 ) {
    cols <- c(soma_colors_risk$green, soma_colors_risk$orange, soma_colors_risk$red)
  } else if ( direction == -4 ) {
    cols <- c(soma_colors_risk$green, soma_colors_risk$yellow,
              soma_colors_risk$orange, soma_colors_risk$red)
  } else if ( direction == -1 || direction < -4 ) {
    cols <- c(soma_colors_risk$green, soma_colors_risk$yellow,
              soma_colors_risk$orange, soma_colors_risk$red, soma_colors_risk$blue)
  } else {
    cols <- unlist(soma_colors_risk, use.names = FALSE)   # chr; no names
  }

  rep_len(cols, length.out = abs(direction))
}


#' @describeIn style-risk
#'   color _discrete_ scale for risk categories.
#' @importFrom ggplot2 discrete_scale
#' @export
scale_color_soma_risk <- function(direction = 4, ...) {
  palette_soma_risk_ <- function(n) {
    palette_soma_risk(direction = direction, n = n)
  }
  discrete_scale(aesthetics = "color", scale_name = "soma_risk",
                 palette = palette_soma_risk_, ...)
}


#' @describeIn style-risk
#'   fill _discrete_ scale for risk categories.
#' @importFrom ggplot2 discrete_scale
#' @export
scale_fill_soma_risk <- function(direction = 4, ...) {
  palette_soma_risk_ <- function(n) {
    palette_soma_risk(direction = direction, n = n)
  }
  discrete_scale(aesthetics = "fill", scale_name = "soma_risk",
                 palette = palette_soma_risk_, ...)
}


#' @describeIn style-risk
#'   color _continuous_ scale for risk trends.
#'   Currently implemented for 2 colors (high risk red to low
#'   risk green). Multiple colors can be done in a gradient with
#'   [scale_color_gradientn()].
#' @inheritParams style-soma
#' @importFrom ggplot2 scale_color_gradient
#' @export
scale_continuous_color_soma_risk <- function(low  = soma_colors_risk$green,
                                             high = soma_colors_risk$red, ...) {
  ggplot2::scale_color_gradient(low = low, high = high, ...)
}


#' @describeIn style-risk
#'   fill _continuous_ scale for risk trends.
#' @inheritParams style-soma
#' @importFrom ggplot2 scale_fill_gradient
#' @export
scale_continuous_fill_soma_risk <- function(low  = soma_colors_risk$green,
                                            high = soma_colors_risk$red, ...) {
  ggplot2::scale_fill_gradient(low = low, high = high, ...)
}
