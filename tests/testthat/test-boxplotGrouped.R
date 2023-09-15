# Setup ------
n  <- 100L
df <- data.frame(
  y           = withr::with_seed(1234, stats::rnorm(n)),
  grp_factor1 = withr::with_seed(2345, sample(1:4L, n, replace = TRUE)),
  grp_factor2 = withr::with_seed(3456, sample(1:3L, n, replace = TRUE))
)
df$grp_int     <- df$grp_factor2
df$grp_lgl     <- df$y > 0
df$grp_char    <- LETTERS[df$grp_int]
df$grp_factor1 <- factor(df$grp_factor1, levels = 1:4L)
df$grp_factor2 <- factor(df$grp_factor2, levels = 1:4L)


# Testing ------
test_that("`boxplotGrouped()` fails appropriately", {
  expect_error(
    boxplotGrouped(df, "y"), "`.data`, `y`, and `group.var` must be provided."
  )
  expect_error(
    boxplotGrouped(df, "y", "grp_int"),
    "Grouping variable(s) 'grp_int' must be character or factor class.",
    fixed = TRUE
  )
  expect_error(
    boxplotGrouped(df, "y", c("grp_factor1", "grp_int")),
    "Grouping variable(s) 'grp_int' must be character or factor class.",
    fixed = TRUE
  )
  expect_error(
    boxplotGrouped(df, "y", c("grp_int", "grp_char")),
    "Grouping variable(s) 'grp_int' must be character or factor class.",
    fixed = TRUE
  )
  expect_error(
    boxplotGrouped(df, "y", c("grp_int", "grp_lgl")),
    "Grouping variable(s) 'grp_int', 'grp_lgl' must be character or factor class.",
    fixed = TRUE
  )
  expect_error(
    boxplotGrouped(df, "y", c("grp_int", "grp_int")),
    "Grouping variables must be unique."
  )
  expect_error(
    boxplotGrouped(df, "y",  character(0)),
    "At least 1 variable must be specified in `group.var`."
  )
  expect_error(
    boxplotGrouped(df, "y", LETTERS[1:3]),
    "Only 1 or 2 variables can be specified in `group.var`."
  )
  expect_error(boxplotGrouped(df, "y", "foo"), "`group.var` not in provided `data`")
  expect_error(boxplotGrouped(df, "foo", "grp_factor1"), "`y` not in provided `data`")
  expect_error(boxplotGrouped(1:10, "y", "grp_int"), "`.data` must be a `data.frame`")

  # no errors
  expect_error(boxplotGrouped(df, "y", "grp_char"), regexp = NA)
  expect_error(boxplotGrouped(df, "y", "grp_factor1"), regexp = NA)
  expect_error(boxplotGrouped(df, "y", "grp_factor2"), regexp = NA)
})

test_that("`boxplotGrouped()` works correctly for simplest input", {
  # data.frame input
  object <- boxplotGrouped(df, "y", "grp_factor1")

  # the expected data is complicated for this plot. using the first
  # creation as the expected object.
  expected_dat <- ggplot2::ggplot_build(object)$data[[1L]]

  expect_equal(object$labels$title, "y ~ grp_factor1")
  expect_equal(object$labels$x, "grp_factor1")
  expect_equal(object$labels$y, "y")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 1L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)

  object <- boxplotGrouped(df, "y", "grp_factor1")
  object_dat <- ggplot2::ggplot_build(object)$data[[1L]]
  expect_equal(object_dat, expected_dat)
  expect_equal(object$labels$title, "y ~ grp_factor1")
  expect_equal(object$labels$x, "grp_factor1")
  expect_equal(object$labels$y, "y")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 1L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)

  # character grouping variable
  object <- boxplotGrouped(df, "y", "grp_char")
  expect_equal(object$labels$title, "y ~ grp_char")
  expect_equal(object$labels$x, "grp_char")
  expect_equal(object$labels$y, "y")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 1L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)
})

test_that("`boxplotGrouped()` works correctly for two group vars", {
  object <- boxplotGrouped(df, "y", c("grp_factor1", "grp_factor2"))

  # ensure components have been modified as expected
  # data object should now have more rows
  object_dat <- ggplot2::ggplot_build(object)$data[[1L]]
  expect_equal(nrow(object_dat), 12L)
  # and different time and labels
  expect_equal(object$labels$title, "y ~ grp_factor1 * grp_factor2")
  expect_equal(object$labels$x, "grp_factor1")
  expect_equal(object$labels$y, "y")
  expect_equal(object$label$fill, "grp_factor2")
  expect_length(object$labels, 4L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 1L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)
})

test_that("`boxplotGrouped()` works correctly notch", {
  object <- boxplotGrouped(df, "y", "grp_factor1", notch = TRUE)

  # ensure components have been modified as expected
  expect_equal(object$labels$title, "y ~ grp_factor1")
  expect_equal(object$labels$x, "grp_factor1")
  expect_equal(object$labels$y, "y")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 1L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_true(object$layers[[1L]]$geom_params$notch)
})

test_that("`boxplotGrouped()` works correctly y.lab", {
  object <- boxplotGrouped(df, "y", "grp_factor1", y.lab = "My Fun Label")

  # ensure components have been modified as expected
  expect_equal(object$labels$title, "y ~ grp_factor1")
  expect_equal(object$labels$x, "grp_factor1")
  expect_equal(object$labels$y, "My Fun Label")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 1L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)
})

test_that("`boxplotGrouped()` works correctly x.lab", {
  object <- boxplotGrouped(df, "y", "grp_factor1", x.lab = "My Fun Label")

  # ensure components have been modified as expected
  expect_equal(object$labels$title, "y ~ grp_factor1")
  expect_equal(object$labels$x, "My Fun Label")
  expect_equal(object$labels$y, "y")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 1L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)
})

test_that("`boxplotGrouped()` works correctly main", {
  object <- boxplotGrouped(df, "y", "grp_factor1", main = "My Fun Title")

  # ensure components have been modified as expected
  expect_equal(object$labels$title, "My Fun Title")
  expect_equal(object$labels$x, "grp_factor1")
  expect_equal(object$labels$y, "y")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 1L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)
})

test_that("`boxplotGrouped()` works correctly beeswarm", {
  object <- withr::with_seed(
    1234L, boxplotGrouped(df, "y", "grp_factor1", beeswarm = TRUE)
  )

  # ensure components have been modified as expected
  expect_equal(object$labels$title, "y ~ grp_factor1")
  expect_equal(object$labels$x, "grp_factor1")
  expect_equal(object$labels$y, "y")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 2L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)
  expect_equal(object$layers[[2L]]$aes_params$shape, 21L)
  expect_equal(object$layers[[2L]]$aes_params$size, 2.5)
})

test_that("`boxplotGrouped()` works correctly pt.shape", {
  object <- withr::with_seed(
    1234L, boxplotGrouped(df, "y", "grp_factor1", beeswarm = TRUE, pt.shape = 18L)
  )

  # ensure components have been modified as expected
  expect_equal(object$labels$title, "y ~ grp_factor1")
  expect_equal(object$labels$x, "grp_factor1")
  expect_equal(object$labels$y, "y")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 2L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)
  expect_equal(object$layers[[2L]]$aes_params$shape, 18L)
  expect_equal(object$layers[[2L]]$aes_params$size, 2.5)
  expect_length(object$layers, 2L)
})

test_that("`boxplotGrouped()` works correctly pt.size", {
  object <- withr::with_seed(
    1234L, boxplotGrouped(df, "y", "grp_factor1", beeswarm = TRUE, pt.size = 3.0)
  )

  # ensure components have been modified as expected
  expect_equal(object$labels$title, "y ~ grp_factor1")
  expect_equal(object$labels$x, "grp_factor1")
  expect_equal(object$labels$y, "y")
  expect_length(object$labels, 3L)
  expect_equal(object$theme, theme_soma())
  expect_length(object$layers, 2L)
  expect_s3_class(object$layers[[1L]]$geom, "GeomBoxplot")
  expect_false(object$layers[[1L]]$geom_params$notch)
  expect_equal(object$layers[[2L]]$aes_params$shape, 21L)
  expect_equal(object$layers[[2L]]$aes_params$size, 3.0)
})
