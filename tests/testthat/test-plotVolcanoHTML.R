# Setup --------
data <- SomaDataIO::example_data
apts <- getAnalytes(data)
ad   <- getAnalyteInfo(data)

# Dummy up a fake `data.frame` with minimal variables
withr::with_seed(100,
  df <- data.frame(
    AptName = sample(apts, 50),
    t_stat = runif(50, 10, 20),
    beta   = rnorm(50, 0, 3),
    p_value = runif(50),
    log2_fc = rnorm(50),
    Zscore = rnorm(50),
    # generate fake target names
    target = paste0("Target", "_", sample(LETTERS, 50, replace = TRUE),
                    sample(1:50, 50), "-",
                    sample(letters, 50, replace = TRUE))
  )
)

# dummy up fake `stat_table` objects
# S3 `stat_table` method; calc.* family - t-test
stbl_t <- list()
stbl_t$stat.table <- col2rn(df, "AptName")
stbl_t$test <- "Student t-test"
stbl_t$data.dim <- dim(df)
stbl_t$y.response <- "LHS"
stbl_t$counts <- c(GroupA = 25, GroupB = 25)
stbl_t$log <- TRUE
stbl_t$data.frame <- "df"
class(stbl_t) <- c("stat_table", class(stbl_t))

# S3 `stat_table` method; calc.* family - Cox
# modify to have expected surv elements
stbl_surv <- stbl_t
stbl_surv$test   <- "Cox Proportional Hazard Test"
stbl_surv$status <- "status"
stbl_surv$time   <- "time"

# Testing ------
test_that("`plotVolcanoHTML.data.frame()` S3 method works without target labels", {
  out <- plotVolcanoHTML(df, FC = log2_fc, p.value = p_value, cutoff = 0.1, fc.cutoff = 0.5)

  plot_obj_dat <- plotly::plotly_build(out)$x$data
  plot_attrs   <- out$x$layoutAttrs
  plotly_nm    <- names(out$x$visdat)
  expect_s3_class(out, c("plotly", "htmlwidget"))
  expect_equal(plot_attrs[[plotly_nm]]$title, "Volcano Plot")
  expect_equal(plot_attrs[[plotly_nm]]$xaxis$title,
               "<i>log<sub>2</sub></i> Fold-Change")
  expect_equal(plot_obj_dat %[[% "name",
               c("Fold Change", "Non-Significant",
                 "Significant", "Significant & Fold Change"))
  expect_equal(lengths(lapply(plot_obj_dat, function(.x) .x$text)),
               c(21, 25, 3, 1))
  # no target name labels
  expect_equal(grepl("TargetName: NA", plot_obj_dat %[[% "text"), rep(TRUE, 50))
})

test_that("`plotVolcanoHTML.data.frame()` S3 method works with target labels", {
  out <- plotVolcanoHTML(df, FC = log2_fc, p.value = p_value, cutoff = 0.1,
                         fc.cutoff = 0.1, labels = target)

  plot_obj_dat <- plotly::plotly_build(out)$x$data
  plot_attrs   <- out$x$layoutAttrs
  plotly_nm    <- names(out$x$visdat)
  expect_s3_class(out, c("plotly", "htmlwidget"))
  expect_equal(plot_attrs[[plotly_nm]]$title, "Volcano Plot")
  expect_equal(plot_attrs[[plotly_nm]]$xaxis$title,
               "<i>log<sub>2</sub></i> Fold-Change")
  expect_equal(plot_obj_dat %[[% "name",
               c("Fold Change", "Non-Significant", "Significant", "Significant & Fold Change"))
  expect_equal(lengths(lapply(plot_obj_dat, function(.x) .x$text)), c(39, 7, 1, 3))
  expect_equal(
    (plot_obj_dat %[[% "text")[1L], # check first target name label
    "AptName: seq.5939.42 <br>TargetName: Target_S23-n <br>Fold Change: 1.58 <br>p-value: 0.381 <br>"
  )
})

test_that("`plotVolcanoHTML.stat_table()` S3 method works with basic args", {
  out <- plotVolcanoHTML(stbl_t, FC = log2_fc, p.value = p_value,
                         tbl = ad, cutoff = 0.1, fc.cutoff = 0.5)

  plot_obj_dat <- plotly::plotly_build(out)$x$data
  plot_attrs   <- out$x$layoutAttrs
  plotly_nm    <- names(out$x$visdat)
  expect_s3_class(out, c("plotly", "htmlwidget"))
  expect_equal(plot_attrs[[plotly_nm]]$title,
               "GroupA (n = 25) vs. GroupB (n = 25)")
  expect_equal(plot_attrs[[plotly_nm]]$xaxis$title,
               "<i>log</i><sub>2</sub> Fold Change<br>Reference Group: GroupA")
  expect_equal(plot_obj_dat %[[% "name",
               c("Fold Change", "Non-Significant",
                 "Significant", "Significant & Fold Change"))
  expect_equal(lengths(lapply(plot_obj_dat, function(.x) .x$text)),
               c(21, 25, 3, 1))
  expect_equal(
    (plot_obj_dat %[[% "text")[1L], # check first target name label
    "AptName: seq.5939.42 <br>TargetName: Tumor necrosis factor ligand superfamily member 12 <br>Fold Change: 1.58 <br>p-value: 0.419 <br>"
  )
})

test_that("`plotVolcanoHTML.stat_table()` S3 method works with extra args", {
  out <- plotVolcanoHTML(stbl_t, FC = Zscore, p.value = p_value, tbl = ad,
                         cutoff = 0.1, fc.cutoff = 0.5, x.lab = "Slope")

  plot_obj_dat <- plotly::plotly_build(out)$x$data
  plot_attrs   <- out$x$layoutAttrs
  plotly_nm    <- names(out$x$visdat)
  expect_s3_class(out, c("plotly", "htmlwidget"))
  expect_equal(plot_attrs[[plotly_nm]]$title,
               "GroupA (n = 25) vs. GroupB (n = 25)")
  expect_equal(plot_attrs[[plotly_nm]]$xaxis$title, "Slope")
  expect_equal(plot_obj_dat %[[% "name",
               c("Fold Change", "Non-Significant",
                 "Significant", "Significant & Fold Change"))
  expect_equal(lengths(lapply(plot_obj_dat, function(.x) .x$text)),
               c(31, 15, 2, 2))
  expect_equal(
    (plot_obj_dat %[[% "text")[1L], # check first target name label
    "AptName: seq.11465.4 <br>TargetName: Probable G-protein coupled receptor 135 <br>Fold Change: 3.72 <br>p-value: 0.154 <br>"
  )
})

test_that("`plotVolcanoHTML.stat_table()` S3 method works with surv stat table", {
  out <- plotVolcanoHTML(stbl_surv, FC = beta, p.value = p_value, tbl = ad,
                         cutoff = 0.1, fc.cutoff = 2, x.lab = "beta")

  plot_obj_dat <- plotly::plotly_build(out)$x$data
  plot_attrs   <- out$x$layoutAttrs
  plotly_nm    <- names(out$x$visdat)
  expect_s3_class(out, c("plotly", "htmlwidget"))
  expect_equal(plot_attrs[[plotly_nm]]$title,
               "Event: status | Time: time")
  expect_equal(plot_attrs[[plotly_nm]]$xaxis$title,
               "beta")
  expect_equal(plot_obj_dat %[[% "name",
               c("Fold Change", "Non-Significant",
                 "Significant", "Significant & Fold Change"))
  expect_equal(lengths(lapply(plot_obj_dat, function(.x) .x$text)),
               c(24, 22, 1, 3))
  expect_equal(
    (plot_obj_dat %[[% "text")[1L], # check first target name label
    "AptName: seq.5669.26 <br>TargetName: Signal-regulatory protein beta-2 <br>Fold Change:   0.109 <br>p-value: 0.112 <br>"
  )
})
