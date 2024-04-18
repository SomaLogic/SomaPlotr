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
#'     \item `id`: The field containing `subject` identifiers,
#'       i.e. how the points in each line should be connected.
#'     \item `color.by` (optional): A field describing how groups
#'       should be split/colored. The only argument with a default value.
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
#' @param color.by A quoted or unquoted variable name in `data` for the column
#'   indicating how to color/group the lines.
#' @param summary.line A function describing how to summarize the lines.
#'   Typically [base::mean()] or [stats::median()], but can be any function
#'   that takes a numeric vector and returns a scalar.
#'   To suppress, set to `NULL`.
#' @param size Numeric. The size for the points on the subject lines.
#' @param y.lab Character. Optional string to set the y-axis label.
#'   Defaults to `log10(RFU)` if a `SeqId` name is detected for `y`.
#' @param add.box Logical. Should boxplots be drawn for each time point?
#'   __Note__: this groups the subjects together by time point,
#'   thus the `time` variable _must_ be a factor.
#' @return A longitudinal plot of samples by subject.
#' @author Stu Field
#' @seealso [geom_line()], [theme()]
#' @examples
#' fct_vec <- factor(c("baseline", "6 months", "12 months", "24 months"))
#' levels(fct_vec) <- fct_vec
#' df <- withr::with_seed(100,
#'   data.frame(
#'     sample_id   = rep(1:10L, times = 4L),
#'     pop         = rep_len(utils::head(LETTERS, 10L), 40L),
#'     time_point  = rep(fct_vec, each = 10L),
#'     seq.1234.56 = stats::rnorm(40, mean = 25, sd = 3.5)
#'   )
#' )
#'
#' # Map 'time_point' (chr) to 'time_dbl' (numeric)
#' new <- df |>
#'  dplyr::left_join(
#'    data.frame(time_point = fct_vec, time_dbl = c(0, 6, 12, 24)),
#'    by = "time_point"
#'  ) |>
#'  # code 'size' to correlate with IDs
#'  dplyr::mutate(size = dplyr::case_when(sample_id <= 4 ~ "small",
#'                                         sample_id > 7 ~ "large",
#'                                                  TRUE ~ "medium"))
#'
#' # No title; no x-axis label; nothing fancy
#' plotLongitudinal(new, y = "seq.1234.56", time = "time_point", id = "sample_id")
#'
#' # Color lines by the 'size' variable
#' plotLongitudinal(new, y = "seq.1234.56", time = "time_point", id = "sample_id",
#'                  color.by = "size")
#'
#' # Color lines by the 'size' variable, using the 'time_dbl' x-variable
#' plotLongitudinal(new, y = "seq.1234.56", time = "time_dbl", id = "sample_id",
#'                  color.by = "size")
#'
#' # Can use unquoted variable names
#' plotLongitudinal(new, y = seq.1234.56, time = time_dbl, id = sample_id,
#'                  color.by = size)
#'
#' # Summary lines
#' plotLongitudinal(new, y = seq.1234.56, id = sample_id, time = time_dbl,
#'                  color.by = size, summary.line = base::mean,
#'                  main = "Use `mean()` Summary")
#'
#' plotLongitudinal(new, y = seq.1234.56, id = sample_id, time = time_dbl,
#'                  color.by = size, summary.line = NULL,
#'                  main = "Suppress Summary Lines")
#'
#' plotLongitudinal(new, y = seq.1234.56, id = sample_id, time = time_dbl,
#'                  color.by = size, summary.line = function(x) 15,
#'                  main = "Fixed Summary (y = 15)")
#'
#' # Add boxplots by time point
#' plotLongitudinal(new, seq.1234.56, id = sample_id, time = time_point,
#'                  color.by = size, add.box = TRUE, summary.line = NULL,
#'                  main = "With Time Point Boxplots")
#'
#' # Facet by `size`
#' p <- plotLongitudinal(new, y = seq.1234.56, id = sample_id, time = time_dbl,
#'                       color.by = size, summary.line = base::mean)
#'
#' p + ggplot2::facet_wrap(~size) +
#'   ggplot2::ggtitle("Facet by `size`")
#'
#' @importFrom dplyr select arrange group_by summarise
#' @importFrom ggplot2 ggplot labs aes geom_boxplot geom_line geom_point
#' @importFrom rlang !!!
#' @importFrom tidyr pivot_longer
#' @export
plotLongitudinal <- function(data, y, time, id, color.by = NULL, size = 2.5,
                             summary.line = stats::median, main = NULL,
                             y.lab = NULL, x.lab = NULL, add.box = FALSE) {

  .call <- match.call(expand.dots = FALSE)
  no_colors <- is.null(.call$color.by)   # if color.by is NULL
  var_str   <- vapply(.call[c("id", "time", "y")], base::toString, "") # str variables
  var_quos  <- lapply(var_str, str2lang) # lang variables

  if ( no_colors ) {
    data$tmp_color <- ""
    var_quos$color.by <- str2lang("tmp_color")
  } else {
    var_quos$color.by <- .call$color.by
  }

  if ( is.null(y.lab) ) {
    y.lab <- var_str[["y"]]
  }

  # carry forward variables not used in plotting
  # to enable downstream use via ggplot
  clin_vars <- setdiff(getMeta(data), c(var_str, "tmp_color"))

  pdata <- data |>
    dplyr::select(!!! var_quos) |> # select & rename essential vars
    dplyr::bind_cols(data[, clin_vars]) |> # add back clin vars w orig names
    pivot_longer(cols = y, names_to = "var", values_to = "value") |>
    arrange(id, time)

  p <- pdata |>
    ggplot() +
    geom_line(aes(y = value, x = time, group = id, colour = color.by),
              lineend = "round", linejoin = "round") +
    geom_point(aes(y = value, x = time, color = color.by),
               alpha = 0.5, size = size) +
    labs(x = x.lab, title = main, y = y.lab) +
    scale_color_soma() +
    theme_soma() +
    NULL

  # Group data summary
  if ( !is.null(summary.line) ) {
    group_data <- data |>
      dplyr::select(!!! var_quos) |>
      group_by(color.by, time) |>
      summarise(group_stat = summary.line(y))
    p <- p +
      geom_line(data       = group_data,
                aes(x      = time,
                    y      = group_stat,
                    group  = color.by,
                    colour = color.by),
                linewidth = 1, linetype = "dashed")
  }

  if ( add.box ) {
    p <- p + geom_boxplot(aes(x = time, y = value), fill = NA, colour = "grey")
  }

  if ( no_colors ) {  # rm legend if no group coloring
    p <- p + theme(legend.position = "none")
  }

  p
}
