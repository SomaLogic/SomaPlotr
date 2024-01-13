# Setup -----
df <- withr::with_seed(100, data.frame(
  ID        = rep(1:10, times = 4),
  Pop       = rep_len(utils::head(LETTERS, 10), 40),
  Sample    = sample(c("small", "medium", "large"), 40, replace = TRUE),
  Sex       = sample(c("male", "female"), 40, replace = TRUE),
  TimePoint = rep(c("baseline", "6 months", "12 months", "24 months"), each = 10),
  seq.1234.56 = stats::rnorm(40, mean = 25, sd = 3.5)
))

df <- df |>  # map TimePoint -> numeric
  dplyr::left_join(
    data.frame(TimePoint = c("baseline", "6 months", "12 months", "24 months"),
               TimeNum   = c(0,           6,          12,          24)), by = "TimePoint")

df$TimePoint <- factor(df$TimePoint, levels = c("baseline", "6 months",
                                                "12 months", "24 months"))

# Testing ------
test_that("plotLongitudinal() defaults produce the expected plot", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", time = "TimePoint", id = "ID"),
    "plotLongitudinal_defaults"
  )
})

test_that("plotLongitudinal() colors can be specified with 'color' argument", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", time = "TimePoint", id = "ID", color = "Sample"),
    "plotLongitudinal_color"
  )
})

test_that("plotLongitudinal() time variable specification works as expected", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", time = "TimeNum", id = "ID"),
    "plotLongitudinal_time"
  )
})

test_that("plotLongitudinal() summary line types can be specified", {
  expect_snapshot_plot({
    plotLongitudinal(df, y = "seq.1234.56", id = "ID", time = "TimePoint",
                     summary.line = mean)
  }, "plotLongitudinal_summaryLine")
})
#
test_that("plotLongitudinal() summary lines can be suppressed with 'summary.line=NULL'", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", id = "ID", time = "TimePoint",
                     summary.line = NULL),
    "plotLongitudinal_suppressSummaryLine"
  )
})

test_that("plotLongitudinal() displays boxplots at each time point when 'add.box=TRUE'", {
  expect_snapshot_plot(
    plotLongitudinal(df, "seq.1234.56", id = "ID", time = "TimePoint", add.box = TRUE),
    "plotLongitudinal_addBox"
  )
})

test_that("plotLongitudinal() can be facetted by a color/grouping variable", {
  expect_snapshot_plot({
    p <- plotLongitudinal(df, y = "seq.1234.56", id = "ID", time = "TimePoint",
                          color = "Sample")
    p + ggplot2::facet_wrap(~Sample)
  }, "plotLongitudinal_facetWrap")
})

test_that("plotLongitudinal() boxes remain when facetted by a color/grouping variable", {
  expect_snapshot_plot({
    p <- plotLongitudinal(df, y = "seq.1234.56", id = "ID", time = "TimePoint",
                          color = "Sample", add.box = TRUE)
    p + ggplot2::facet_wrap(~Sample)
  }, "plotLongitudinal_facetWrap_boxes")
})

test_that("plotLongitudinal() can accept all unquoted arguments", {
  expect_snapshot_plot(plotLongitudinal(df, y = seq.1234.56, id = ID,
                                        time = TimePoint, color = Sample),
                       "plotLongitudinal_unquoted_args")
})

test_that("plotLongitudinal() can accept all quoted arguments", {
  expect_snapshot_plot(plotLongitudinal(df, y = "seq.1234.56", id = "ID",
                                        time = "TimePoint", color = "Sample"),
                       "plotLongitudinal_quoted_args")
})

test_that("plotLongitudinal() can accept a mix of quoted and unquoted arguments", {
  expect_snapshot_plot(plotLongitudinal(df, y = seq.1234.56, id = "ID",
                                        time = TimePoint, color = "Sample"),
                       "plotLongitudinal_mixQuote_args")
})
