# Setup -----
fct_vec <- factor(c("baseline", "6 months", "12 months", "24 months"))
levels(fct_vec) <- fct_vec

df <- withr::with_seed(100,
  data.frame(
    sample_id   = rep(1:10L, times = 4L),
    pop         = rep_len(utils::head(LETTERS, 10L), 40L),
    time_point  = rep(fct_vec, each = 10L),
    seq.1234.56 = stats::rnorm(40, mean = 25, sd = 3.5)
  )
)

# Map 'time_point' (chr) to 'time_dbl' (numeric)
df <- df |>
 dplyr::left_join(
   data.frame(time_point = fct_vec, time_dbl = c(0, 6, 12, 24)),
   by = "time_point"
 ) |>
 # code 'size' to correlate with IDs
 dplyr::mutate(size = dplyr::case_when(sample_id <= 4 ~ "small",
                                        sample_id > 7 ~ "large",
                                                 TRUE ~ "medium"))

# Testing ------
test_that("`plotLongitudinal()` defaults produce the expected plot", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", time = "time_point", id = "sample_id"),
    "plotLongitudinal-defaults"
  )
})


test_that("`plotLongitudinal()` colors can be specified with `color.by` argument", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", time = "time_point",
                     id = "sample_id", color.by = "size"),
    "plotLongitudinal-color-by"
  )
})

test_that("`plotLongitudinal()` time numeric variable specification works as expected", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", time = "time_dbl", id = "sample_id"),
    "plotLongitudinal-time-dbl"
  )
})

test_that("`plotLongitudinal()` summary.line types can be specified", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", id = "sample_id",
                     time = "time_point", summary.line = base::mean),
    "plotLongitudinal-summary-line")
})


test_that("`plotLongitudinal()` summary.line can be suppressed with `summary.line=NULL`", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", id = "sample_id",
                     time = "time_point", summary.line = NULL),
    "plotLongitudinal-suppress-summary-line"
  )
})

test_that("`plotLongitudinal()` summary.line can be fixed with on-the-fly function", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = "seq.1234.56", id = "sample_id",
                     time = "time_point", summary.line = function(x) 20),
    "plotLongitudinal-fixed-summary-line"
  )
})

test_that("`plotLongitudinal()` displays boxplots at time points with `add.box=TRUE`", {
  expect_snapshot_plot(
    plotLongitudinal(df, "seq.1234.56", id = "sample_id",
                     time = "time_point", add.box = TRUE),
    "plotLongitudinal-addBox-factor"
  )
})

test_that("`plotLongitudinal()` can be facetted by a color/grouping variable", {
  expect_snapshot_plot({
    plotLongitudinal(df, y = "seq.1234.56", id = "sample_id",
                     time = "time_dbl", color.by = "size") +
      ggplot2::facet_wrap(~size)
  }, "plotLongitudinal-facet-wrap-size")
})

test_that("`plotLongitudinal()` boxes remain when facetted by a color/grouping variable", {
  expect_snapshot_plot({
    plotLongitudinal(df, y = "seq.1234.56", id = "sample_id",
                     time = "time_point", color.by = "size", add.box = TRUE) +
      ggplot2::facet_wrap(~size)
  }, "plotLongitudinal-facet-wrap-boxes")
})

test_that("`plotLongitudinal()` can accept all unquoted string args", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = seq.1234.56, id = sample_id,
                     time = time_dbl, color.by = size),
    "plotLongitudinal-unquoted-args"
  )
})

test_that("`plotLongitudinal()` can accept a mix of quoted and unquoted strings", {
  expect_snapshot_plot(
    plotLongitudinal(df, y = seq.1234.56, id = "sample_id",
                     time = time_dbl, color.by = "size"),
    "plotLongitudinal-mix-quote-args"
  )
})
