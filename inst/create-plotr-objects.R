#' ---------------------------------
#' Generate and save "Plot" package objects in `data/plot.rda`
#' Generate color palettes and standard ggplot objects
#' @author Stu Field
#' ---------------------------------
#' run: make objects
#' ---------------------------------
soma_purple <- "#24135F"
soma_blue   <- "#4067E2"

col_string <- c(
  "dodgerblue",
  "red",
  "darkgreen",
  "darkorchid4",
  "cyan",
  "orange",
  "black",
  "grey",
  "#990066",
  "green",
  soma_purple
)

soma_colors <- list(
  purple     = "#24135F",
  lightgreen = "#00A499",
  lightgrey  = "#707372",
  magenta    = "#840B55",
  lightblue  = "#006BA6",
  yellow     = "#D69A2D",
  darkgreen  = "#007A53",
  darkblue   = "#1B365D",
  darkgrey   = "#54585A",
  blue       = "#004C97"
)

soma_colors2 <- list(
  blue      = "#4067E2",
  teal      = "#59CFDB",
  pink      = "#DB40EF",
  yellow    = "#FFAA0F",
  green     = "#2F9862",
  turq      = "#0B6975",
  lightblue = "#0077E0",
  purple    = "#170BA5"
)

soma_colors_risk <- list(
  red   = "#B33D26",
  orange = "#D57800",
  yellow = "#D9C756",
  green  = "#5CAA7F",
  blue   = "#004C97"
)

soma_colors_greys <- list(
  black      = "#000000",
  darkgrey2  = "#333333",
  darkgrey1  = "#444444",
  grey       = "#9DABB2",
  lightgrey2 = "#C4CDD1",
  lightgrey1 = "#EEF1F2"
)

gg <- list()

# Define a set of figures to play with using the Iris dataset
gg$point <- ggplot2::ggplot(iris, ggplot2::aes(y      = Sepal.Width,
                                               x      = Sepal.Length,
                                               colour = Species)) +
  ggplot2::geom_point(alpha = 0.75, size = 3) +
  ggplot2::labs(x = "Sepal Width (cm)", y = "Sepal Length (cm)",
                colour = "Species", title = "Iris Dataset")

gg$bar <- dplyr::group_by(iris, Species) |>
  dplyr::summarise(mean = mean(Sepal.Width)) |>
  ggplot2::ggplot(ggplot2::aes(x = Species, y = mean, fill = Species)) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "Species", y = "Mean Sepal Width (cm)",
                fill = "Species", title = "Iris Dataset")

gg$box <- withr::with_seed(1,    # for the `geom_jitter()` below
  ggplot2::ggplot(iris, ggplot2::aes(x    = Species,
                                     y    = Sepal.Width,
                                     fill = Species)) +
    ggplot2::geom_boxplot(notch = TRUE, alpha = 0.8, outlier.colour = NA) +
    ggplot2::geom_jitter(alpha = 0.5, width = 0.05, size = 2) +
    ggplot2::labs(x = "Species", y = "Sepal Width (cm)",
                  fill = "Species", title = "Iris Dataset")
)

save(
  col_string,
  gg,
  soma_purple,
  soma_blue,
  soma_colors,
  soma_colors2,
  soma_colors_risk,
  soma_colors_greys,
  file = "data/plotr.rda",
  compress = "xz"
)
