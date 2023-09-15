#' Create a "Beeswarm" Boxplot
#'
#' Plot a series of boxplots with "beeswarm"-style points added to the boxes.
#'
#' @family boxplots
#' @param .data Either a `data.frame`/`tbl_df` object where each column is
#'   a numeric vector containing values for each box, or a _named_
#'   `list` object which can be converted to one.
#' @param notch Should notches be drawn in the boxplots.
#' @param label Character. A label for the grouping variable, i.e. what
#'   the columns of the data frame represent.
#' @param main Character. Main title for the plot.
#'   See [ggtitle()] for `ggplot2` style graphics.
#' @param x.lab Character. Optional string for the x-axis. Otherwise
#'   one is automatically generated (default).
#' @param y.lab Character. Optional string for the y-axis. Otherwise
#'   one is automatically generated (default).
#' @param cols Character. A _vector_ of colors for the groups/boxes.
#'   For [plotDoubleHist()], must be `length = 2`.
#' @param pt.size Numeric. A size for the points. See [geom_point()].
#' @param pt.color Character. A _fill_ color for the points. See [geom_point()].
#' @param pt.shape Numeric or Character. Recognized `pch` shapes for the
#'   points. Recall that `pch = 21 - 25` only are "fill-able". Other point
#'   characters will _not_ take on the color from `pt.color`.
#'   See [geom_point()].
#' @param ... Additional arguments passed to [geom_boxplot()].
#' @return Boxplot with "beeswarm" style points.
#' @author Stu Field
#' @seealso [geom_boxplot()], [geom_jitter()]
#' @examples
#' df <- lapply(setNames(LETTERS[1:5], letters[1:5]), \(x) rnorm(10, 10, 3)) |>
#'   data.frame() |>
#'   dplyr::select(d, dplyr::everything())   # move `d` to the front
#' df
#'
#' df |> boxplotBeeswarm(main = "Title")
#' df |> boxplotBeeswarm(pt.color = "cyan")
#' df |> boxplotBeeswarm(cols = "grey")     # all boxes 1 color
#' df |> boxplotBeeswarm(label = "Disease Level", y.lab = "Y", notch = FALSE)
#'
#' # Non-fill-able `pt.shape`
#' df |> boxplotBeeswarm(pt.shape = 13)
#'
#' # shapes 21 -> 25 are `fill-capable`
#' df |> boxplotBeeswarm(cols = rep("blue", ncol(df)), pt.size = 5,
#'                        pt.shape = 23, pt.color = "red")
#' @importFrom rlang sym !! :=
#' @importFrom dplyr mutate select everything
#' @importFrom ggplot2 geom_boxplot scale_fill_manual aes ggplot
#' @importFrom ggplot2 position_jitter geom_jitter
#' @importFrom tidyr gather
#' @export
boxplotBeeswarm <- function(.data,
                            notch = TRUE,
                            label = "Group",
                            main = NULL,
                            y.lab = "value",
                            x.lab = label,
                            cols,
                            pt.size = 2.5,
                            pt.color = "black",
                            pt.shape = 21, ...) {

  if ( !inherits(.data, c("data.frame", "list")) ) {
    stop(
      "`.data` *must* either be a list of vectors or a data frame.",
      call. = FALSE
    )
  }

  # fix for jagged vector lengths
  plot_df <- lapply(.data, "length<-", max(lengths(.data))) |>
    data.frame(check.names = FALSE)
  n <- ncol(plot_df)
  orig_ord <- names(plot_df)  # keep original ordering of elements

  p <- plot_df |>
    gather(key = !!label, na.rm = TRUE) |>
    mutate(!!sym(label) := factor(!!sym(label), levels = orig_ord)) |>
    ggplot(aes(x = !!sym(label),
               y = value,
               fill = !!sym(label))) +
    geom_boxplot(notch = notch, alpha = 0.7, ...,
                 outlier.color = NA) +
    geom_jitter(position = position_jitter(width = 0.05, seed = 101),
                alpha = 0.5, shape = pt.shape,
                fill = pt.color, size = pt.size) +
    labs(x = x.lab, y = y.lab, title = main) +
    theme_soma() +
    NULL

  if ( missing(cols) ) {
    p <- p + scale_fill_soma()
  } else {
    if ( length(cols) == 1L ) {    # duplicate if 1 color passed
      cols <- rep(cols, n)
    }
    p <- p + scale_fill_manual(values = head(unname(cols), n))
  }

  p
}
