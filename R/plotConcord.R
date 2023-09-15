#' Plot Concordance
#'
#' A concordance plot of 2 numeric vectors, passed either as a
#' 2-column `data.frame`/`tibble` object _or_ separately as `x` and `y`.
#'
#' @param x Numeric. First vector comparison (x-axis). Alternatively, `x` can
#'   be a __two column__ `data frame` containing vector data to compare.
#' @param ... For extensibility to S3 methods.
#' @return A concordance plot with the squared correlation coefficient
#'   and related statistics as a sub-title, a __robust__ linear regression
#'   fit line, the unit line, and optionally labeled points.
#' @author Stu Field
#' @seealso [cor()], [rlm()], [cor.test()]
#' @references Lin, Lawrence I-Kuei. 1989. A Concordance Correlation
#'   Coefficient to Evaluate Reproducibility. __Biometrics__. 45:255-268.
#' @examples
#' x <- withr::with_seed(1234L, rnorm(500, 400, 30))
#'
#' # bias
#' plotConcord(x, x + 20)
#'
#' # scale
#' plotConcord(x, x * 1.2)
#'
#' # random
#' y <- x + withr::with_seed(2345L, rnorm(500, sd = sd(x) / 2))
#' plotConcord(x, y - (y * 0.02))
#'
#' # using Spearman's Correlation Coefficient
#' plotConcord(x, y - (y * 0.02), cc.type = "spearman")
#'
#' # using Pearson's Correlation Coefficient
#' plotConcord(x, y - (y * 0.02), cc.type = "pearson")
#'
#' # using Kendall's Correlation Coefficient
#' plotConcord(x, y - (y * 0.02), cc.type = "kendall")
#'
#' # Label points by number (default)
#' plotConcord(x, y - (y * 0.02), identify = TRUE, spread = 25)
#'
#' # S3 data frame method
#' df <- data.frame(x = x, y = y - (y * 0.02))
#' plotConcord(df, identify = TRUE, spread = 25)
#' @importFrom ggplot2 aes expand_limits geom_abline geom_point
#' @importFrom ggplot2 geom_rug geom_smooth geom_text labs
#' @export
plotConcord <- function(x, ...) UseMethod("plotConcord")

#' @noRd
#' @export
plotConcord.default <- function(x, ...) {
  stop("Couldn't find a S3 method for this class object: ", value(class(x)),
       call. = FALSE)
}

#' S3 numeric method
#'
#' @rdname plotConcord
#' @param y Numeric. Second vector comparison (y-axis).
#'   Omitted if `x` is a `data.frame`.
#' @export
plotConcord.numeric <- function(x, y, ...) {
  stopifnot("`x` and `y` lengths are unequal." = length(x) == length(y))
  plotConcord(data.frame(x = x, y = y), ...)
}

#' S3 data frame method
#'
#' For `cc.type = "ccc"`, Lin's Concordance Correlation Coefficient as
#'   is used to estimate the correlation coefficient.
#'   For all other allowed values of `cc.type`,
#'   [stats::cor.test()] is used.
#'
#' @rdname plotConcord
#' @inheritParams boxplotBeeswarm
#' @param add.rug Logical. If `TRUE`, axis includes ticks for each point.
#' @param pt.col Character. The color to be used for the points.
#' @param pt.size Numeric. The point size.
#' @param identify Logical. If `TRUE`, points are auto-identified.
#' @param all.labels Character. The labels for __each__ point if
#'   `identify = TRUE`. The points that are actually labeled is determined
#'   by the `spread =` argument. The default simply numbers the points.
#' @param spread Numeric. The width from the unit line to start
#'   labeling points. The default (`spread = 1`) is reasonable for
#'   log10-transformed RFU data.
#' @param label.size Numeric. The size for the point labels.
#' @param cc.type Character. The correlation coefficient estimator to use.
#'   Must be one of `c("ccc", "pearson", "spearman", "kendall")`.
#' @importFrom stats coef cor.test setNames
#' @export
plotConcord.data.frame <- function(x,
                                   x.lab      = "x",
                                   y.lab      = "y",
                                   main       = NULL,
                                   pt.col     = soma_purple,
                                   pt.size    = 2,
                                   add.rug    = TRUE,
                                   identify   = FALSE,
                                   all.labels = seq_len(nrow(x)),
                                   spread     = 1,
                                   label.size = 3,
                                   cc.type    = c("ccc",
                                                  "pearson", "spearman", "kendall"),
                                   ...) {

  # data must be 2 columns and contain only numeric data

  if ( ncol(x) != 2L ) {
    stop("The `data frame` object you are passing *must* contain ",
         "only 2 columns, 1 for the x-axis and 1 for the y-axis.",
         call. = FALSE)
  }

  tst <- vapply(x, inherits, what = "numeric", FUN.VALUE = NA)
  if ( !all(tst) ) {
    stop("The correlation coefficient estimators assume that ",
         "`x` and `y` are continuous variables.\n",
         "The data were provided as: class(x) = ", value(class(x[, 1L])),
         " and class(y) = ", value(class(x[, 2L])), "\n",
         "If ", paste(c("`x` ", "`y` ")[!tst], collapse = "and "),
         ifelse(sum(!tst) > 1L, "are ", "is "),
         "to be treated as continuous, convert using ",
         "`as.numeric()` prior to calling this function.",
         call. = FALSE)
  }

  # testing input types

  character_test <- list(is.character, "character")
  numeric_test   <- list(is.numeric, "numeric")
  logical_test   <- list(is.logical, "logical")

  args <- list("x.lab"     = character_test,
               "y.lab"     = character_test,
               "main"      = list(function(x) {
                                  is.null(x) || is.character(x)
                             }, "NULL or character"),
               "pt.size"    = numeric_test,
               "add.rug"    = logical_test,
               "identify"   = logical_test,
               "spread"     = numeric_test,
               "label.size" = numeric_test)

  .test_input <- function(x_name) {
    task <- args[[x_name]]
    if ( !do.call(task[[1L]], args = list(as.name(x_name))) ) {
      stop("`", x_name, "` must be ", task[[2L]], "; provided ",
           value(class(eval(as.name(x_name)))), ".", call. = FALSE)
    }
    TRUE
  }
  vapply(names(args), .test_input, FUN.VALUE = NA)

  cc.type <- tolower(cc.type)
  cc.type <- match.arg(cc.type)

  # closure to calculate the determinant for a 2x2
  calc_outside <- function(x, y, y.int) {
    a  <- 100000
    b  <- (100000 + 1) - y.int
    b2 <- (100000 - 1) + y.int
    c  <- x
    d  <- y - y.int
    d2 <- y + y.int
    # first is above upper; second is below lower
    (a * d  >= b * c) | (a * d2 <= b2 * c)
  }

  df   <- setNames(x, c("x", "y"))   # in case named otherwise
  lims <- range(df, na.rm = TRUE)

  if ( identify ) {
    df$y.int <- spread
    if ( length(all.labels) != nrow(df) ) {
      stop("If `identify = TRUE`, `all.labels` must be length =", value(nrow(df)),
           call. = FALSE)
    }
    ident_lgl <- calc_outside(df$x, df$y, df$y.int)

    .done(
      "Auto-labeling", value(sum(ident_lgl)), "points by spread =", value(spread)
    )
    df$labels <- all.labels
  }

  rho <- switch(cc.type,
                "ccc" = .calcCCC(df$x, df$y),
                stats::cor.test(df$x, df$y, method = cc.type))
  estimate <- switch(cc.type, "ccc" = rho$rho.c, rho$estimate)
  coeffs <- suppressWarnings(MASS::rlm(y ~ x, data = df, init = "ls", maxit = 100)) |>
    stats::coef()
  eqn_titl <- paste("y =", format(coeffs[[2L]], digits = 3L), "x",
                    ifelse(coeffs[[1L]] < 0, "-", "+"),
                    format(abs(coeffs[[1L]]), digits = 3L))
  cc_titl <- bquote(.(switch(cc.type,
                             "pearson" = "r",
                             "kendall" = "\u03c4",
                             "\u03c1"))
                    [.(toupper(substr(cc.type, 1L, 1L)))] ^ 2 ==
                      .(format(estimate^2, digits = 3L)))
  pval_titl <- paste("p =", format(rho$p.value, digits = 3L))
  subtitle  <- bquote(.(eqn_titl)~~~~.(cc_titl)~~~~.(pval_titl))

  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(color = pt.col, alpha = 0.25, size = pt.size) +
    expand_limits(x = lims, y = lims) +
    labs(title = main, x = x.lab, y = y.lab, subtitle = subtitle) +
    geom_abline(slope = 1, intercept = 0, color = "black") +
    geom_abline(slope = coeffs[[2L]], intercept = coeffs[[1L]],
                linetype = "longdash", color = soma_colors$lightgreen)

  if ( add.rug ) {
    p <- p + geom_rug(color = soma_colors$lightblue, linewidth = 0.2)
  }

  if ( identify ) {
    p <- p + geom_text(data = df[ident_lgl, ], aes(label = labels), hjust = 0,
                       nudge_x = 2, size = label.size, check_overlap = TRUE)
  }

  p + theme_soma()
}
