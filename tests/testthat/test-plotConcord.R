# Setup ------
n  <- 100L
df <- data.frame(
  x_integer = withr::with_seed(1234L, sample(1:10L, n, TRUE)),
  x_factor  = withr::with_seed(2345L, factor(sample(1:3, n, TRUE))),
  x_char    = withr::with_seed(3456L, sample(LETTERS, n, TRUE)),
  x_logical = withr::with_seed(4567L, sample(c(TRUE, FALSE), n, TRUE)),
  x_numeric = withr::with_seed(5678L, stats::runif(100, min = 0, max = 10)),
  y         = withr::with_seed(6789L, stats::runif(100, min = 0, max = 10))
)

# Testing ------
test_that("`plotConcord()` fails with expected messages", {
  expect_error(plotConcord(df[, 5L:6L], x.lab = 1),
               "`x.lab` must be character; provided 'numeric'.")
  expect_error(plotConcord(df[, 5L:6L], y.lab = 1),
               "`y.lab` must be character; provided 'numeric'.")
  expect_error(plotConcord(df[, 5L:6L], main = 1),
               "`main` must be NULL or character; provided 'numeric'.")
  expect_error(plotConcord(df[, 5L:6L], pt.size = "a"),
               "`pt.size` must be numeric; provided 'character'.")
  expect_error(plotConcord(df[, 5L:6L], add.rug = "a"),
               "`add.rug` must be logical; provided 'character'.")
  expect_error(plotConcord(df[, 5L:6L], identify = "a"),
               "`identify` must be logical; provided 'character'.")
  expect_error(plotConcord(df[, 5L:6L], spread = "a"),
               "`spread` must be numeric; provided 'character'.")
  expect_error(plotConcord(df[, 5L:6L], label.size = "a"),
               "`label.size` must be numeric; provided 'character'.")

  expect_error(plotConcord(df[, 1L:3L]),
               paste("The `data frame` object you are passing *must*",
                     "contain only 2 columns, 1 for the x-axis and 1",
                     "for the y-axis."),
               fixed = TRUE)
  expect_error(plotConcord(df[, 1L, drop = FALSE]),
               paste("The `data frame` object you are passing *must*",
                     "contain only 2 columns, 1 for the x-axis and 1",
                     "for the y-axis."),
               fixed = TRUE)

  error_msg1 <- paste0("The correlation coefficient ",
                       "estimators assume that ",
                       "`x` and `y` are continuous variables.\n",
                       "The data were provided as: ")
  error_msg2x <- paste0("\n",
                        "If `x` is to be treated as continuous, ",
                        "convert using ",
                        "`as.numeric()` prior to calling this function.")
  error_msg2y <- paste0("\n",
                        "If `y` is to be treated as continuous, ",
                        "convert using ",
                        "`as.numeric()` prior to calling this function.")
  error_msg2xy <- paste0("\n",
                        "If `x` and `y` are to be treated as continuous, ",
                        "convert using ",
                        "`as.numeric()` prior to calling this function.")

  expect_error(plotConcord(df[, c("x_integer", "y")]),
               paste0(error_msg1,
                      "class(x) = 'integer' and class(y) = 'numeric'",
                      error_msg2x),
               fixed = TRUE)
  expect_error(plotConcord(df[, c("x_factor", "y")]),
               paste0(error_msg1,
                      "class(x) = 'factor' and class(y) = 'numeric'",
                      error_msg2x),
               fixed = TRUE)
  expect_error(plotConcord(df[, c("x_char", "y")]),
               paste0(error_msg1,
                      "class(x) = 'character' and class(y) = 'numeric'",
                      error_msg2x),
               fixed = TRUE)
  expect_error(plotConcord(df[, c("x_logical", "y")]),
               paste0(error_msg1,
                      "class(x) = 'logical' and class(y) = 'numeric'",
                      error_msg2x),
               fixed = TRUE)
  expect_error(plotConcord(df[, c("y", "x_integer")]),
               paste0(error_msg1,
                      "class(x) = 'numeric' and class(y) = 'integer'",
                      error_msg2y),
               fixed = TRUE)
  expect_error(plotConcord(df[, c("x_factor", "x_integer")]),
               paste0(error_msg1,
                      "class(x) = 'factor' and class(y) = 'integer'",
                      error_msg2xy),
               fixed = TRUE)
})

test_that("`plotConcord()` works correctly for simplest input", {
  plot <- plotConcord(df[, c("x_numeric", "y")])
  expect_snapshot_plot(plot, "plot_simple")
})

test_that("`plotConcord()` works correctly for simplest input with x.lab", {
  plot <- plotConcord(df[, c("x_numeric", "y")], x.lab = "My Fun Label")
  expect_snapshot_plot(plot, "plotConcord_xLab")
})

test_that("`plotConcord()` works correctly for simplest input with y.lab", {
  plot <- plotConcord(df[, c("x_numeric", "y")], y.lab = "My Fun Label")
  expect_snapshot_plot(plot, "plotConcord_yLab")
})

test_that("`plotConcord()` works correctly for simplest input with main", {
  plot <- plotConcord(df[, c("x_numeric", "y")], main = "My Fun Title")
  expect_snapshot_plot(plot, "plotConcord_main")
})

test_that("`plotConcord()` works correctly for simplest input with pt.col", {
  plot <- plotConcord(df[, c("x_numeric", "y")], pt.col = soma_colors$magenta)
  expect_snapshot_plot(plot, "plotConcord_ptCol")
})

test_that("`plotConcord()` works correctly for simplest input with pt.size", {
  plot <- plotConcord(df[, c("x_numeric", "y")], pt.size = 5)
  expect_snapshot_plot(plot, "plotConcord_ptSize")
})

test_that("`plotConcord()` works correctly for simplest input with add.rug = FALSE", {
  plot <- plotConcord(df[, c("x_numeric", "y")], add.rug = FALSE)
  expect_snapshot_plot(plot, "plotConcord_addRug")
})

test_that("`plotConcord()` works correctly for simplest input with identify", {
  expect_message(
    plot <- plotConcord(df[, c("x_numeric", "y")], identify = TRUE),
    "Auto-labeling '79' points by spread = '1'"
  )
  expect_snapshot_plot(plot, "plotConcord_identify")
})

test_that("`plotConcord()` works correctly for simplest input with all.labels", {
  expect_message(
    plot <- plotConcord(df[, c("x_numeric", "y")], identify = TRUE,
                          all.labels = seq_len(n) * 10),
    "Auto-labeling '79' points by spread = '1'"
  )
  expect_snapshot_plot(plot, "plotConcord_allLabels")
})

test_that("`plotConcord()` works correctly for simplest input with spread", {
  expect_message(
    plot <- plotConcord(df[, c("x_numeric", "y")], identify = TRUE, spread = 2),
    "Auto-labeling '63' points by spread = '2'"
  )
  expect_snapshot_plot(plot, "plotConcord_spread")
})

test_that("`plotConcord()` works correctly for simplest input with label.size", {
  expect_message(
    plot <- plotConcord(df[, c("x_numeric", "y")], identify = TRUE,
                          label.size = 5),
    "Auto-labeling '79' points by spread = '1'"
    )
  expect_snapshot_plot(plot, "plotConcord_labelSize")
})

test_that("`plotConcord()` ignores inputs when identify = FALSE", {
  plot <- plotConcord(df[, c("x_numeric", "y")], identify = FALSE,
                      all.labels = seq_len(n) * 100)
  expect_snapshot_plot(plot, "plotConcord_identifyFalse")
})

test_that("`plotConcord()` works correctly for other concordence cc.types", {
  plot <- plotConcord(df[, c("x_numeric", "y")], cc.type = "spearman")
  expect_snapshot_plot(plot, "plotConcord_spearman")
})
