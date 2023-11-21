#' Plot Longitudinal Samples by Subject
#'
#' A samples plot by grouping the subjects across time in a
#' longitudinal study. Can be as few as two longitudinal samples
#' (e.g. paired samples). See _examples_ for various plotting options.
#' In order to generate a longitudinal plot by sample, 4 variables
#' (columns of the data frame `data`) _must_ be provided:
#'   \itemize{
#'     \item `y`: Typically the field containing the y-axis.
#'     \item `time`: The field containing the time-series information, the x-axis.
#'     \item `id`: The field containing `subject` identifiers, i.e. how the points
#'       in each line should be connected.
#'     \item `color`: A field describing how groups should be split/colored. The
#'       only argument with a default value.
#'   }
#'
#' @inheritParams boxplotBeeswarm
#' @param data A data frame containing RFU values to plot, typically
#'   a `soma_adat` class object.
#' @param y A quoted or unquoted variable name in `data` for the column
#'   containing the y-axis (i.e. typically the "Response" variable).
#' @param time A quoted or unquoted variable name in `data` for the column
#'   containing the x-axis (i.e. typically the "time" variable).
#' @param id A quoted or unquoted variable name in `data` for the column
#'   containing the subject IDs (i.e. how the samples should be connected).
#' @param color A quoted or unquoted variable name in `data` for the column
#'   indicating how to color/group the lines. Default is by `Sex`. If
#'   _no grouping_ is desired, simply create a dummy variable and pass that
#'   column. See example below.
#' @param summary.line A function describing how to summarize the lines.
#'   Typically [mean()] or [median()]. Set to `NULL` to suppress summary lines.
#' @param size Numeric. The size for the points on the subject lines.
#' @param y.lab Character. Optional string to set the y-axis label.
#'   Defaults to `log10(RFU)` if a `SeqId` name is detected.
#' @param add.box Logical. Should boxplots be drawn for each time point?
#'   __Note__: this groups the subjects together by time point and looks only at
#'   the differences across time points, thus `time` _must_ be a factor.
#' @return A longitudinal plot of samples by subject.
#' @author Stu Field
#' @seealso [geom_line()], [theme()]
#' @examples
#' df <- withr::with_seed(100, data.frame(
#'   Pop       = rep_len(utils::head(LETTERS, 10), 40),
#'   Sample    = sample(c("small", "medium", "large"), 40, replace = TRUE),
#'   TimePoint = rep(c("baseline", "6 months", "12 months", "24 months"), each = 10),
#'   seq.1234.56 = stats::rnorm(40, mean = 25, sd = 3.5)
#' ))
#'
#' df$TimePoint <- factor(df$TimePoint, levels = c("baseline", "6 months",
#'                                                 "12 months", "24 months"))
#'
#' new <- df |>  # Map 'TimePoint' var (character) to a numeric
#'  dplyr::left_join(data.frame(TimePoint = c("baseline", "6 months", "12 months", "24 months"),
#'                              time      = c(0,           6,          12,          24)
#'                              ),
#'                   by = "TimePoint") |>
#'  dplyr::mutate(id     = rep(1:10, times = 4),
#'                Sample = dplyr::case_when(id <= 4 ~ "small", # Recode Sample to correlate with IDs
#'                                          id > 7  ~ "large",
#'                                          TRUE    ~ "medium"))
#'
#' # No title; no x-axis label; nothing fancy
#' plotLongitudinal(new, y = "seq.1234.56", time = "TimePoint", id = "id", color = "Sample")
#'
#' # Color lines by the 'Sample' variable, using the 'time' numeric x variable
#' plotLongitudinal(new, y = "seq.1234.56", time = "time", id = "id",
#'                  color = "Sample")
#'
#' # Use unquoted variable names
#' plotLongitudinal(new, y = seq.1234.56, time = time, id = id,
#'                  color = Sample)
#'
#' # Use `mean` as the summary lines
#' plotLongitudinal(new, y = "seq.1234.56", id = "id", time = "time",
#'                  color = "Sample", summary.line = mean)
#'
#' # Suppress summary line with `NULL`
#' plotLongitudinal(new, y = "seq.1234.56", id = "id", time = "time", color = "Sample",
#'                  summary.line = NULL, main = "No Summary Lines")
#'
#' # Boxplots by time point; `time` variable (TimePoint) must be a factor
#' plotLongitudinal(new, "seq.1234.56", id = "id", time = "TimePoint",
#'                  color = "Sample", add.box = TRUE, summary.line = NULL,
#'                  main = "With Time Point Boxplots")
#'
#' # Suppress group-wise colors
#' # by setting up `dummy` column
#' new$dummy <- "A"     # call it anything
#' plotLongitudinal(new, "seq.1234.56", id = "id", time = "TimePoint",
#'                  color = "dummy", add.box = TRUE, summary.line = NULL,
#'                  main = "Boxplots | Suppress Group Color") +
#'   ggplot2::theme(legend.pos = "none")
#'
#' # Facet by `Sample`
#' p <- plotLongitudinal(new, y = "seq.1234.56", id = "id", time = "time",
#'                       color = "Sample", summary.line = mean)
#'
#' p + ggplot2::facet_wrap(~Sample) +
#'   ggplot2::ggtitle("Facet by `Sample`")
#'
#' @importFrom dplyr select arrange group_by summarise
#' @importFrom ggplot2 ggplot labs aes geom_boxplot geom_line geom_point
#' @importFrom rlang enquo quo_is_null ensym get_expr !! !!!
#' @importFrom tidyr pivot_longer
#' @importFrom SomaDataIO is.apt
#' @export
plotLongitudinal <- function(data, y, time, id, color = Sex,
                             summary.line = stats::median,
                             size = 2.5, main = NULL,
                             y.lab = bquote(italic(log)[10] ~ (RFU)),
                             x.lab = NULL, add.box = FALSE) {

  # Set up quosures
  id    <- ensym(id)
  y     <- ensym(y)
  time  <- ensym(time)
  color <- enquo(color)

  if ( quo_is_null(color) ) { # Prevents forced evaluation of color
    data$.plot_group_var <- ""
    .plot_group_var <- ".plot_group_var"
    keys  <- c(y, time, id, ensym(.plot_group_var))
  } else {
    .plot_group_var <- ensym(color)
    data$.plot_group_var <- data[[get_expr(color)]]
    keys  <- c(y, time, id, .plot_group_var)
  }

  # Perform check for is.apt() on `y`
  if ( missing(y.lab) && !is.apt(y) ) {
    y.lab <- y
  }

  pdata <- data |>
    dplyr::select(!!!keys) |>
    pivot_longer(cols = y, names_to = "key", values_to = "value") |>
    arrange(!!id, !!time)

  p <- pdata |>
    ggplot() +
    geom_line(aes(y      = value,
                  x      = !!time,
                  group  = !!id,
                  colour = !!.plot_group_var)) +
    geom_point(aes(y     = value,
                   x     = !!time,
                   color = !!.plot_group_var),
               alpha = 0.5, size = size) +
    labs(x = x.lab, title = main, y = y.lab) +
    scale_color_soma() +
    theme_soma() +
    NULL

  # Group data summary
  if ( !is.null(summary.line) ) {
    gd <- data |>
      dplyr::select(!!!keys) |>
      group_by(!!.plot_group_var, !!time) |>
      summarise(group_mean = summary.line(!!y))
    p <- p +
      geom_line(data = gd,
                aes(x      = !!time,
                    y      = group_mean,
                    group  = !!.plot_group_var,
                    colour = !!.plot_group_var),
                linewidth = 1, linetype = "dashed")
  }

  if ( add.box ) {
    p <- p +
      geom_boxplot(aes(x = .data[[time]], y = value),
                   fill = NA, colour = "grey")
  }

  if ( quo_is_null(color)) {        # Remove legend when no color/grouping
    p <- p + guides(color = "none") # var is specified
  }

  p
}
