# Setup ------
x <- withr::with_seed(123, rnorm(100, mean = 5))

# Testing ----
test_that("`plotPDF()` produces the expected plot when all default parameters are used", {
  expect_snapshot_plot(plotPDF(x), "plotPDF_default")
})

test_that("`plotPDF()` produces the expected plot when `add.gauss = TRUE`", {
  plot <- plotPDF(x, add.gauss = TRUE)
  expect_snapshot_plot(plot, "plotPDF_addGauss")
})

test_that("`plotPDF()` produces the expected plot when `fill = TRUE`", {
  plot <- plotPDF(x, fill = TRUE)
  expect_snapshot_plot(plot, "plotPDF_fillDefault")
})

test_that("`plotPDF()` produces the expected plot with custom color palette", {
  plot <- plotPDF(x, col = "red")
  expect_snapshot_plot(plot, "plotPDF_col")
})

test_that("`plotPDF()` produces the expected plot with custom color palette and `fill = TRUE`", {
  plot <- plotPDF(x, col = "red", fill = TRUE)
  expect_snapshot_plot(plot, "plotPDF_fillCol")
})

test_that("`plotPDF()` produces the expected plot when all arguments are specified", {
  plot <- plotPDF(x,
                  add.gauss = TRUE,
                  col = "blue",
                  lty = "dotted",
                  x.lab = "test x",
                  y.lab = "test y",
                  main = "test main")
  expect_snapshot_plot(plot, "plotCDF_allArgs")
})
