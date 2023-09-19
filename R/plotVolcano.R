#' Create Volcano Plot
#'
#' Create a volcano plot given a vector of log2-transformed
#' fold-changes (`FC`) and linear space p-values.
#'
#' @inheritParams boxplotBeeswarm
#' @param data A `data.frame` object containing at _least_ two columns
#'   containing 1) log2-transformed fold-changes, 2) linear-space p-values.
#'   A third column containing point labels _must_ be supplied,
#'   if `identify = TRUE`.
#' @param identify Logical. Should significant points be identified?
#' @param FC An unquoted string identifying the column in `data` containing
#'   a vector [log2()]-transformed fold-changes.
#' @param p.value An unquoted string identifying the column in `data` containing
#'   a vector of p-values.
#' @param labels An unquoted string identifying the column in `data` containing
#'   point labels, typically "Target" or "Analyte" names.
#' @param cutoff Horizontal statistical significance cutoff for coloring
#'   points. Defaults to Bonferroni corrected significance at `alpha = 0.05`
#'   in "p-value" linear space \verb{[0, 1]}.
#' @param fc.cutoff Placement for the cutoff for coloring points along the
#'   fold-change x-axis. Defaults to doubling in fold-change (`1`).
#' @param pt.size Numeric. The size for the points.
#' @param text.size Numeric. The size for the identifying text.
#' @param ... Arguments passed to [geom_point()].
#' @author Stu Field
#' @seealso [geom_point()].
#' @examples
#' withr::with_seed(101, {
#'   fc1 <- sort(runif(500, -2.5, 0))   # Z-scores as dummy fold-changes
#'   fc2 <- sort(runif(500, 0, 2.5))    # Z-scores as dummy fold-changes
#'   p1  <- pnorm(fc1)                  # p-values for neg. scores
#'   p2  <- pnorm(fc2, lower.tail = FALSE) # p-values for pos. scores
#'   p   <- jitter(c(p1, p2), amount = 0.1) # add noise
#'   p[p < 0] <- runif(sum(p < 0), 1e-05, 1e-02) # floor p > 0 after jitter
#'   df <- data.frame(fc = c(fc1, fc2), p = p)
#' })
#'
#' plotVolcano(df, fc, p, cutoff = 0.1)  # lower p-value cutoff
#'
#' # add some random labels to `df`
#' df <- dplyr::mutate(df, pt_label = dplyr::row_number())
#' plotVolcano(df, fc, p, labels = pt_label, identify = TRUE, cutoff = 0.01)
#' @importFrom rlang enquo !!
#' @importFrom dplyr pull mutate row_number case_when filter
#' @importFrom ggplot2 geom_point ggplot aes labs scale_color_manual
#' @export
plotVolcano <- function(data,
                        FC,
                        p.value,
                        labels,
                        identify = FALSE,
                        fc.cutoff = 1,
                        pt.size = 2.5,
                        text.size = 3,
                        cutoff = 0.05 / nrow(data),
                        main = NULL,
                        x.lab = NULL, ...) {

  .fc <- enquo(FC)
  .p  <- enquo(p.value)

  if ( all(pull(data, !!.fc) >= 0) ) {
    warning(
      "It appears you are not passing log2-transformed ",
      "fold-change values. Please check.", call. = FALSE
    )
  }

  if ( is.null(main) ) {
    main <- "Volcano Plot of Significant Fold Changes"
  }

  if ( is.null(x.lab) ) {
    x.lab <- bquote(italic(log)[2] ~ (Fold~Change))
  }

  y.lab <- bquote(-italic(log)[10] ~ (p-value))  # nolint: infix_spaces_linter2

  plot_df <- data |>
    mutate(
      # Order matters here!
      group = case_when(
        (-log10(!!.p) >= -log10(cutoff)) & (abs(!!.fc) >= fc.cutoff) ~
          "Significant & Fold-Change",
        -log10(!!.p) >= -log10(cutoff) ~ "Significant",
        abs(!!.fc) >= fc.cutoff ~ "Fold-Change",
        TRUE ~ "Non-Significant"),
      type  = grepl("^Significant", group)
    )

  cols <- c(
    "Non-Significant"           = soma_colors$lightgrey,
    "Fold-Change"               = soma_colors$lightgreen,
    "Significant"               = soma_colors$purple,
    "Significant & Fold-Change" = soma_colors$yellow
    )

  p <- plot_df |>
    ggplot(aes(x = !!.fc, y = -log10(!!.p), color = group)) +
    geom_point(alpha = 0.5, size = pt.size, ...) +
    scale_color_manual(values = cols, name = "") +
    labs(x = x.lab, y = y.lab, title = main) +
    geom_vline(xintercept = c(-1, 1) * fc.cutoff, color = "grey",
               linetype = "longdash", alpha = 0.75) +
    geom_vline(xintercept = 0, color = "grey50",
               linetype = "solid") +
    theme_soma() +
    NULL

  if ( identify ) {
    p <- p + geom_text(data = dplyr::filter(plot_df, type),
                       aes(label = !!enquo(labels)),
                       hjust = 0, nudge_x = 0.05,
                       size = text.size, color = "black",
                       check_overlap = TRUE)
  }

  p
}
