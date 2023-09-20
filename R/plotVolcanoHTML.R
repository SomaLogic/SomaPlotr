#' Generate HTML-based Volcano Plot
#'
#' @family volcano
#' @inheritParams boxplotBeeswarm
#' @inheritParams plotVolcano
#' @param data A data frame containing log2-transformed fold-changes and
#'   corresponding p-values. An optional third column
#'   containing, e.g. "Target" Names, can be passed if specified by
#'   the `labels =` parameter.
#' @param ... Additional arguments passed to [plotly::plotly()].
#' @author Leigh Alexander, Stu Field
#' @seealso [plotly::plotly()]
#' @examples
#' # Dummy up a fake table with minimal variables
#' adat <- SomaDataIO::example_data
#' seqs <- SomaDataIO::getAnalytes(adat)
#' target_map <- SomaDataIO::getTargetNames(SomaDataIO::getAnalyteInfo(adat))
#' df <- withr::with_seed(101, {
#'   fc1 <- sort(runif(500, -2.5, 0))   # Z-scores as dummy fold-changes
#'   fc2 <- sort(runif(500, 0, 2.5))    # Z-scores as dummy fold-changes
#'   p1  <- pnorm(fc1)                  # p-values for neg. scores
#'   p2  <- pnorm(fc2, lower.tail = FALSE) # p-values for pos. scores
#'   p   <- jitter(c(p1, p2), amount = 0.1) # add noise
#'   p[p < 0] <- runif(sum(p < 0), 1e-05, 1e-02) # floor p > 0 after jitter
#'   seq_vec <- sample(seqs, length(p))   # random seqIds
#'   data.frame(
#'     AptName = seq_vec,
#'     t_stat  = runif(50, 10, 20),
#'     log2_fc = c(fc1, fc2),
#'     p_value = p,
#'     target  = unlist(target_map)[seq_vec]  # map random target names
#'   )
#' })
#'
#' # S3 `data.frame` method
#' # No TargetNames -> `NA`
#' plotVolcanoHTML(df, log2_fc, p_value, cutoff = 0.1, fc.cutoff = 0.5)
#'
#' # Add TargetNames via `labels=`
#' plotVolcanoHTML(df, log2_fc, p_value, cutoff = 0.1, fc.cutoff = 0.5, labels = target)
#' @importFrom stats setNames
#' @importFrom rlang enquo !!
#' @importFrom dplyr pull case_when ungroup select left_join
#' @export
plotVolcanoHTML <- function(data, FC, p.value, cutoff, fc.cutoff, main, x.lab, ...) {
  UseMethod("plotVolcanoHTML")
}


#' S3 plotVolcanoHTML default method
#' @noRd
#' @export
plotVolcanoHTML.default <- function(data, ...) {
  stop(
    "Couldn't find a S3 method for this class object: ",
    value(class(data)), call. = FALSE
  )
}


#' @describeIn plotVolcanoHTML
#'   Plot method for objects of class `data.frame`.
#' @importFrom SomaDataIO rn2col col2rn
#' @export
plotVolcanoHTML.data.frame <- function(data, FC, p.value,
                                       cutoff    = 0.05 / nrow(data),
                                       fc.cutoff = 1,
                                       main      = NULL,
                                       x.lab     = NULL,
                                       labels, ...) {

  if ( !"AptName" %in% names(data) ) {
    data <- rn2col(data, "AptName")
  }

  p_vec <- pull(data, !!enquo(p.value))
  FC    <- pull(data, !!enquo(FC))

  if ( !missing(labels) ) {
    labels <- data |> pull(!!enquo(labels))
  } else {
    labels <- NA_character_
  }

  text_labels <- paste(
    "AptName:", data$AptName,
    "<br>TargetName:", labels,
    "<br>Fold Change:", format(2^FC, digits = 2),  # linear space
    "<br>p-value:", format(p_vec, digits = 2),
    "<br>"
  )

  if ( is.null(main) ) {
    main <- "Volcano Plot"
  }

  if ( is.null(x.lab) ) {
    x.lab <- "<i>log<sub>2</sub></i> Fold-Change"
  }

  p_vec <- -log10(p_vec)

  if ( all(FC >= 0) ) {
    warning(
      "It appears you are not passing log2-transformed ",
      "fold-change values. Please check.", call. = FALSE
    )
  }

  color.by <- case_when(
    (p_vec >= -log10(cutoff)) & (abs(FC) >= fc.cutoff) ~ "Significant & Fold Change",
    p_vec >= -log10(cutoff) ~ "Significant",
    abs(FC) >= fc.cutoff ~ "Fold Change",
    TRUE ~ "Non-Significant"
  )

  # matched to `plotVolcano()` palette
  cols <- .volcano_cols()

  plotly::plot_ly() |>
    plotly::add_trace(
      x = FC, y = p_vec,           # primary scatter plot of ps and fold changes
      color = color.by,
      colors = cols,
      mode = "markers", type = "scatter",
      marker = list(opacity = 0.7, size = 9),
      text = text_labels, ...) |>
    plotly::layout(
      title = main,
      xaxis = list(title = x.lab, showgrid = FALSE),
      yaxis = list(title = "-<i>log<sub>10</sub></i> p-value",
                   showgrid = FALSE),
      margin = list(b = 60, t = 60), ...)
}


#' @describeIn plotVolcanoHTML
#'   Plot method for objects of class `stat_table` (SomaLogic internal).
#' @inheritParams SomaDataIO::getTargetNames
#' @importFrom SomaDataIO rn2col
#' @export
plotVolcanoHTML.stat_table <- function(data, FC, p.value,
                                       cutoff    = 0.05 / nrow(data$stat.table),
                                       fc.cutoff = 1,
                                       main      = NULL,
                                       x.lab     = NULL,
                                       tbl, ...) {

  calc_object <- data
  stat_table  <- rn2col(calc_object$stat.table, "AptName")

  levels    <- calc_object$counts
  ref_group <- names(levels)[1L]
  test      <- calc_object$test
  x_pred    <- if ( is.null(calc_object$response) ) {
    calc_object$x.predictor
  } else {
    calc_object$response
  }

  tbl <- dplyr::select(ungroup(tbl), AptName, TargetFullName)
  stat_table <- left_join(stat_table, tbl, by = "AptName")
  p_vec <- pull(stat_table, !!enquo(p.value))
  p_vec <- -log10(p_vec)
  FC    <- pull(stat_table, !!enquo(FC))

  text_labels <- paste(
    "AptName:", stat_table$AptName,
    "<br>TargetName:", stat_table$TargetFullName,
    "<br>Fold Change:", format(2^FC, digits = 2L),  # linear space
    "<br>p-value:", format(p_vec, digits = 2L),
    "<br>"
  )

  color.by <- case_when(
    (p_vec >= -log10(cutoff)) & (abs(FC) >= fc.cutoff) ~ "Significant & Fold Change",
    p_vec >= -log10(cutoff) ~ "Significant",
    abs(FC) >= fc.cutoff ~ "Fold Change",
    TRUE ~ "Non-Significant"
  )

  # matched to `plotVolcano` palette
  cols <- .volcano_cols()

  if ( is.null(main) ) {
    if ( grepl("Student t-test|Wilcoxon|Logistic|Kolmog", test) ) {
      main <- sprintf(
        "%s (n = %s) vs. %s (n = %s)",
        names(levels)[1L], levels[[1L]], names(levels)[2L], levels[[2L]]
      )
    } else if ( grepl("Linear Regression|Spearman", test) ) {
      main <- sprintf("%s\n%s", test, x_pred)
    } else if ( grepl("Cox|AFT", test) ) {
      main <- sprintf(
        "Event: %s | Time: %s", calc_object$status, calc_object$time
      )
    }
  }

  if ( is.null(x.lab) ) {
    x.lab <- paste("<i>log</i><sub>2</sub> Fold Change<br>Reference Group:",
                   ref_group)
  } else {
    paste(x.lab, "<br>Reference Group:", ref_group)
  }

  if ( all(FC >= 0) ) {
    warning(
      "It appears you are not passing log2-transformed ",
      "fold-change values. Please check `data` argument.",
      call. = FALSE
    )
  }

  plotly::plot_ly() |>
    plotly::add_trace(
      x = FC, y = p_vec,
      color = color.by,
      colors = cols,
      mode = "markers", type = "scatter",
      marker = list(opacity = 0.7, size = 9),
      text = text_labels, ...) |>
    plotly::layout(
      title = main,
      xaxis = list(title = x.lab, showgrid = FALSE),
      yaxis = list(title = "-<i>log</i><sub>10</sub> p-value", showgrid = FALSE),
      margin = list(b = 60, t = 60), ...)
}


.volcano_cols <- function() {
  c("Non-Significant" = soma_colors$lightgrey,
    "Significant"     = soma_colors$purple,
    "Fold Change"     = soma_colors$lightgreen,
    "Significant & Fold Change" = soma_colors$yellow)
}
