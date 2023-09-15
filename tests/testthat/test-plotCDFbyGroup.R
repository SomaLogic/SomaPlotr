# Setup --------
data <- log10(SomaDataIO::example_data) |> dplyr::filter(SampleType == "Sample")
apt <- withr::with_seed(101, sample(getAnalytes(data), 1))
map <- SomaDataIO::getTargetNames(getAnalyteInfo(data))
title <- sprintf("Empirical CDF Plot of %s (%s)", apt, map[[apt]])

# Snapshot testing ------
test_that("`plotCDFbyGroup()` produces the expected plot with defaults", {
  expect_snapshot_plot(
    plotCDFbyGroup(data, apt, group.var = Sex),
    "plotCDFbyGroup_defaults"
  )
})

test_that("`plotCDFbyGroup()` produces the expected plot when a title is defined", {
  expect_snapshot_plot(
    plotCDFbyGroup(data, apt, Sex, main = title),
    "plotCDFbyGroup_main"
  )
})

# Error catching -----
test_that("`plotCDFbyGroup()` throws an error when `apt` is not a character", {
  expect_error(
    plotCDFbyGroup(x, apt = 1234, Sex),
    "`apt` must be a character string"
  )
})

test_that("`plotCDFbyGroup()` throws an error when an unexpected `group.var` is used", {
  expect_error(
    plotCDFbyGroup(data, apt, group.var = "TimeStatus"),
    "The `group.var =` argument must be an unquoted name contained in `data =` argument."
  )
})

test_that("`plotCDFbyGroup()` throws an error when a categorical feature is used", {
  data$seq.5489.18 <- cut(data$seq.5489.18, c(3, 3.5, 4, 4.5),
                          labels = c("none", "some", "more"))

  expect_error(
    plotCDFbyGroup(data, "seq.5489.18", Sex),
    "Data appears non-continuous with ('3') categories ['none', 'some', 'more']",
    fixed = TRUE
  )
})

test_that("`plotCDFbyGroup()` throws warning when non-positive RFU values are detected", {
  data$seq.5489.18 <- seq(-10L, 0L, length.out = length(data$seq.5489.18))

  expect_warning(
    plotCDFbyGroup(data, "seq.5489.18", Sex),
    paste0("Non-positive values detected ...\n",
           "If RFU data, possibly multi-logging?")
  )
})
