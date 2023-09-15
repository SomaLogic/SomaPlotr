
#' \pkg{SomaPlotr} Colors and Graphics
#'
#' This package contains numerous color palettes, themes, and base
#' \pkg{ggplot2} graphics objects that are used to coordinate and display
#' a consistent visual graphics presentation.
#' In most cases the color palettes have been determined by the SomaLogic
#' Commercial and Marketing teams.
#' The \pkg{ggplot2} objects are intended to be used for palette and
#' theme testing. See the description of each object below.
#'
#' @name objects
#' @source SomaLogic Operating Co., Inc.
#' @docType data
NULL


#' Soma-Colors
#'
#' @section soma_colors2:
#' The new official `"Soma-Colors"` after a 2020 re-branding.
#' Includes a new `"Soma-Blue"` plus other blues, a greens, yellow, pink, and purple:
#' \tabular{ll}{
#'   Color       \tab Hex-code \cr
#'   `blue`      \tab `#4067E2`\cr
#'   `teal`      \tab `#1FA8BC`\cr
#'   `pink`      \tab `#BC10E0`\cr
#'   `yellow`    \tab `#FF8B04`\cr
#'   `green`     \tab `#14753D`\cr
#'   `turquoise` \tab `#02444F`\cr
#'   `lightblue` \tab `#0051D2`\cr
#'   `purple`    \tab `#170BA5`
#' }
#' Note the following common color-to-group mappings:
#' \tabular{lcl}{
#'   Controls/Non-Events/Placebo \tab `-->` \tab blue \cr
#'   Cases/Events/Intervention   \tab `-->` \tab teal \cr
#'   Female                      \tab `-->` \tab yellow \cr
#'   Male                        \tab `-->` \tab green
#' }
#'
#' @rdname objects
#' @format NULL
#' @examples
#' barplot(rep_len(1, length(soma_colors2)), axes = FALSE,
#'         col = unlist(soma_colors2),
#'         names = names(soma_colors2), las = 2)
#'
"soma_colors2"


#' Soma-Colors (retired 2020)
#'
#' @section soma_colors:
#' The former official `"Soma-Colors"`,
#' includes `"Soma-Purple"` plus a green, blue, and two shades of grey:
#' \tabular{ll}{
#'   Color        \tab Hex-code \cr
#'   `purple`     \tab `#24135F`\cr
#'   `lightgreen` \tab `#00A499`\cr
#'   `lightgrey`  \tab `#707372`\cr
#'   `magenta`    \tab `#840B55`\cr
#'   `lightblue`  \tab `#006BA6`\cr
#'   `yellow`     \tab `#D69A2D`\cr
#'   `darkgreen`  \tab `#007A53`\cr
#'   `darkblue`   \tab `#1B365D`\cr
#'   `darkgrey`   \tab `#54585A`\cr
#'   `blue`       \tab `#004C97`
#' }
#' @rdname objects
#' @format NULL
#' @examples
#' barplot(rep_len(1, length(soma_colors)), axes = FALSE,
#'         col = unlist(soma_colors),
#'         names = names(soma_colors), las = 2)
#'
"soma_colors"


#' Soma-Risk Colors
#'
#' @section soma_colors_risk:
#' The official `"Soma-Risk Colors"` for risk designations.
#' Includes red, orange, yellow, green, blue:
#' \tabular{ll}{
#'   Color    \tab Hex-code \cr
#'   `red`    \tab `#B33D26`\cr
#'   `orange` \tab `#D57800`\cr
#'   `yellow` \tab `#D9C756`\cr
#'   `green`  \tab `#5CAA7F`\cr
#'   `blue`   \tab `#004C97`
#' }
#' @rdname objects
#' @format NULL
#' @examples
#' barplot(rep_len(1, length(soma_colors_risk)), axes = FALSE,
#'         col = unlist(soma_colors_risk),
#'         names = names(soma_colors_risk), las = 2)
#'
"soma_colors_risk"


#' Soma-Greys Colors
#'
#' @section soma_colors_greys:
#' The official neutral palette of grey-scale colors:
#' \tabular{ll}{
#'   Color        \tab Hex-code \cr
#'   `black`      \tab `#000000`\cr
#'   `darkgrey2`  \tab `#333333`\cr
#'   `darkgrey1`  \tab `#444444`\cr
#'   `grey`       \tab `#9DABB2`\cr
#'   `lightgrey2` \tab `#C4CDD1`\cr
#'   `lightgrey1` \tab `#EEF1F2`
#' }
#' @rdname objects
#' @format NULL
#' @examples
#' barplot(rep_len(1, length(soma_colors_greys)), axes = FALSE,
#'         col = unlist(soma_colors_greys),
#'         names = names(soma_colors_greys), las = 2)
#'
"soma_colors_greys"


#' Soma Blue Color
#'
#' @section soma_blue: The official `"Soma-Blue"` color.
#' @rdname objects
#' @format NULL
#' @examples
#' barplot(c(1, 1), col = c(soma_blue, ggplot2::alpha(soma_blue, 0.5)), axes = FALSE,
#'         names = paste("soma_blue", c("(alpha = 1)", "(alpha = 0.5)")))
#'
"soma_blue"


#' Soma Purple Color
#'
#' @section soma_purple:
#' The former official `"Soma-Purple"` color (retired in 2020).
#' Also the final entry in [col_string].
#' @rdname objects
#' @format NULL
#' @examples
#' barplot(c(1, 1), col = c(soma_purple, ggplot2::alpha(soma_purple, 0.5)), axes = FALSE,
#'         names = paste("soma_purple", c("(alpha = 1)", "(alpha = 0.5)")))
#'
"soma_purple"


#' Color String (vector string)
#'
#' @section col_string:
#' A vector of colors used (sometimes internally) in various plotting routines:
#' \preformatted{
#'   [1] "dodgerblue" [2] "red"     [3] "darkgreen" [4] "darkorchid4"
#'   [5] "cyan"       [6] "orange"  [7] "black"     [8] "grey"
#'   [9] "#990066"   [10] "green"  [11] "#5A556E"
#' }
#' @rdname objects
#' @format NULL
#' @examples
#' barplot(rep_len(1, length(col_string)), axes = FALSE,
#'         col = col_string, names = col_string, las = 2)
#'
"col_string"


#' Sample `ggplot` Objects
#'
#' @section gg: A list of 3 sample `ggplot` figures based on the `iris` data set for
#' use in testing various themes, color palettes, and general plotting:
#'   \itemize{
#'     \item A scatter plot
#'     \item A bar plot
#'     \item A boxplot
#'   }
#' @rdname objects
#' @format NULL
#' @examples
#' gg$point
#' gg$bar
#' gg$box
#' gg$point + ggplot2::theme_dark()
#' gg$bar + ggplot2::theme_classic()
"gg"
