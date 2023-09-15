# Setup ------
x <- withr::with_seed(123, rnorm(100, mean = 5))

# Testing ----
test_that("`plotCDF()` produces the expected plot when all default parameters are used", {
  expect_snapshot_plot(plotCDF(x), "plotCDF_default")
})

test_that("`plotCDF()` produces the expected plot when `add.gauss = TRUE`", {
  plot <- plotCDF(x, add.gauss = TRUE)
  expect_snapshot_plot(plot, "plotCDF_addGauss")
})

test_that("`plotCDF()` produces the expected plot when a custom color palette is used", {
  plot <- plotCDF(x, col = "red")
  expect_snapshot_plot(plot, "plotCDF_col")
})

test_that("`plotCDF()` produces the expected plot when a custom linetype is specified", {
  plot <- plotCDF(x, lty = "dotted")
  expect_snapshot_plot(plot, "plotCDF_lty")
})

test_that("`plotCDF()` produces the expected plot when all arguments are specified", {
  plot <- plotCDF(x,
                  add.gauss = TRUE,
                  col = "blue",
                  lty = "dotted",
                  x.lab = "test x",
                  y.lab = "test y",
                  main = "test main")
  expect_snapshot_plot(plot, "plotCDF_allArgs")
})
