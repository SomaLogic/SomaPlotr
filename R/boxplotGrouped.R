#' Grouped Boxplots by Factor(s)
#'
#' Generate boxplots of a response variable
#' by one or two factor variables.
#'
#' @family boxplots
#' @inheritParams boxplotBeeswarm
#' @param y Character. The response variable for the `y` axis.
#' @param group.var Character. String of length 1 or 2 representing the
#'   names of the grouping variable(s). Variables must be either factor
#'   or character class vectors.
#' @param beeswarm Logical. Add points as overlay on top of boxplots?
#' @return A series of boxplots grouped by `group.var`(s).
#' @author Stu Field
#' @seealso [geom_boxplot()], [geom_jitter()]
#' @examples
#' size <- c("small", "medium", "large")
#' time <- c("baseline", "6 months", "12 months", "24 months")
#' df <- data.frame(
#'   Sample      = sample(size, 40, replace = TRUE),
#'   TimePoint   = rep(time, each = 10),
#'   seq.1234.56 = stats::rnorm(40, mean = 25, sd = 3.5)
#' )
#'
#' # factor levels determine group order
#' df$Sample    <- factor(df$Sample, levels = size)
#' df$TimePoint <- factor(df$TimePoint, levels = time)
#'
#' # single factor
#' boxplotGrouped(df, y = "seq.1234.56", group.var = "Sample")
#'
#' # double factor
#' boxplotGrouped(df, y = "seq.1234.56", group.var = c("Sample", "TimePoint"))
#'
#' # with "beeswarm" points
#' boxplotGrouped(df, y = "seq.1234.56", group.var = "TimePoint", beeswarm = TRUE)
#' boxplotGrouped(df, y = "seq.1234.56", group.var = c("Sample", "TimePoint"),
#'                beeswarm = TRUE)
#' @importFrom ggplot2 aes geom_boxplot geom_jitter geom_point ggplot ggtitle xlab
#' @importFrom ggplot2 position_jitter position_jitterdodge scale_fill_manual ylab
#' @importFrom rlang sym
#' @export
boxplotGrouped <- function(.data, y, group.var, notch = FALSE, y.lab = NULL,
                           x.lab = NULL, beeswarm = FALSE, main = NULL,
                           pt.shape = 21, pt.size = 2.5) {

  stopifnot(
    "`.data`, `y`, and `group.var` must be provided." =
      all(!c(missing(.data), missing(y), missing(group.var)))
  )

  Lvar <- length(group.var)
  stopifnot(
    "`.data` must be a `data.frame`."         = is.data.frame(.data),
    "`y` not in provided `data`."             = y %in% names(.data),
    "Only 1 or 2 variables can be specified in `group.var`." = Lvar <= 2,
    "At least 1 variable must be specified in `group.var`."  = Lvar > 0,
    "Grouping variables must be unique."      = sum(duplicated(group.var)) == 0,
    "`group.var` not in provided `data`."     = all(group.var %in% names(.data))
  )

  tst <- vapply(.data[group.var], inherits, what = c("factor", "character"), NA)
  tst <- which(!tst)

  if ( length(tst) > 0L ) {
    stop("Grouping variable(s) ", value(names(tst)),
         " must be character or factor class.", call. = FALSE)
  }

  # if single grouping variable
  if ( Lvar == 1L ) {
    pt.color <- soma_colors[[1L]]
    p <- .data |>
      ggplot(aes(x = !!sym(group.var[1L]),
                 y = !!sym(y))) +
      geom_boxplot(notch = notch, alpha = 0.7, fill = pt.color,
                   outlier.shape = if ( beeswarm ) NA else NULL)
  } else {
    p <- .data |>
      ggplot(aes(x    = !!sym(group.var[1L]),
                 y    = !!sym(y),
                 fill = !!sym(group.var[2L]))) +
      geom_boxplot(notch = notch, alpha = 0.7,
                   outlier.shape = if ( beeswarm ) NA else NULL) +
      scale_fill_manual(values = unlist(soma_colors, use.names = FALSE)) +
      # uncomment for the soma_colors2 palette
      #scale_fill_soma() +    # nolint: commented_code_linter
      NULL
  }

  if ( beeswarm ) {
    if ( Lvar == 1L ) {
      p <- p + geom_point(position = position_jitter(width = 0.05, seed = 101),
                          alpha = 0.5, fill = pt.color, shape = pt.shape,
                          size = pt.size)
    } else {
      p <- p + geom_point(position = position_jitterdodge(seed = 101),
                          alpha = 0.5, shape = pt.shape, size = pt.size)
    }
  }

  if ( is.null(main) ) {
    main <- paste(y, "~", paste(group.var, collapse = " * "))
  }

  if ( !is.null(y.lab) ) {
    p <- p + ylab(y.lab)
  }

  if ( !is.null(x.lab) ) {
    p <- p + xlab(x.lab)
  }

  p + ggtitle(label = main) + theme_soma()
}
