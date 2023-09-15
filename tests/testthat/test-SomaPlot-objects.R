test_that("col.string is a vector with the correct values", {
  expect_type(col_string, "character")
  expect_equal(col_string,
               c("dodgerblue",
                 "red",
                 "darkgreen",
                 "darkorchid4",
                 "cyan",
                 "orange",
                 "black",
                 "grey",
                 "#990066",
                 "green",
                 soma_purple))
})

test_that("soma_colors is a list with the correct values", {
  expect_type(soma_colors, "list")
  expect_equal(soma_colors,
               list(purple     = "#24135F",
                    lightgreen = "#00A499",
                    lightgrey  = "#707372",
                    magenta    = "#840B55",
                    lightblue  = "#006BA6",
                    yellow     = "#D69A2D",
                    darkgreen  = "#007A53",
                    darkblue   = "#1B365D",
                    darkgrey   = "#54585A",
                    blue       = "#004C97"))
})

test_that("soma_colors2 is a list with the correct values", {
  expect_type(soma_colors2, "list")
  expect_equal(soma_colors2,
               list(blue      = "#4067E2",
                    teal      = "#59CFDB",
                    pink      = "#DB40EF",
                    yellow    = "#FFAA0F",
                    green     = "#2F9862",
                    turq      = "#0B6975",
                    lightblue = "#0077E0",
                    purple    = "#170BA5"))
})

test_that("soma_colors_risk is a list with the correct values", {
  expect_type(soma_colors_risk, "list")
  expect_equal(soma_colors_risk,
               list(red    = "#B33D26",
                    orange = "#D57800",
                    yellow = "#D9C756",
                    green  = "#5CAA7F",
                    blue   = "#004C97"))
})

test_that("soma_colors_greys is a list with the correct values", {
  expect_type(soma_colors_greys, "list")
  expect_equal(soma_colors_greys,
               list(black      = "#000000",
                    darkgrey2  = "#333333",
                    darkgrey1  = "#444444",
                    grey       = "#9DABB2",
                    lightgrey2 = "#C4CDD1",
                    lightgrey1 = "#EEF1F2"))
})


test_that("palette_soma_risk returns the correct colors in the correct order", {
  expect_equal(palette_soma_risk(), c("#B33D26",
                                      "#D57800",
                                      "#D9C756",
                                      "#5CAA7F"))

  expect_equal(palette_soma_risk(1), "#B33D26")

  expect_equal(palette_soma_risk(2), c("#B33D26",
                                       "#5CAA7F"))

  expect_equal(palette_soma_risk(3), c("#B33D26",
                                       "#D57800",
                                       "#5CAA7F"))

  expect_equal(palette_soma_risk(4), c("#B33D26",
                                       "#D57800",
                                       "#D9C756",
                                       "#5CAA7F"))

  expect_equal(palette_soma_risk(5), c("#B33D26",
                                       "#D57800",
                                       "#D9C756",
                                       "#5CAA7F",
                                       "#004C97"))

  expect_equal(palette_soma_risk(-1), "#5CAA7F")

  expect_equal(palette_soma_risk(-2), c("#5CAA7F",
                                        "#B33D26"))

  expect_equal(palette_soma_risk(-3), c("#5CAA7F",
                                        "#D57800",
                                        "#B33D26"))

  expect_equal(palette_soma_risk(-4), c("#5CAA7F",
                                        "#D9C756",
                                        "#D57800",
                                        "#B33D26"))

  expect_equal(palette_soma_risk(10), c("#B33D26",
                                        "#D57800",
                                        "#D9C756",
                                        "#5CAA7F",
                                        "#004C97",
                                        "#B33D26",
                                        "#D57800",
                                        "#D9C756",
                                        "#5CAA7F",
                                        "#004C97"))

  expect_equal(palette_soma_risk(-10), c("#5CAA7F",
                                         "#D9C756",
                                         "#D57800",
                                         "#B33D26",
                                         "#004C97",
                                         "#5CAA7F",
                                         "#D9C756",
                                         "#D57800",
                                         "#B33D26",
                                         "#004C97"))


})

test_that("palette_soma_gender returns the correct colors in the correct order", {
  expect_equal(palette_soma_gender(),
               c("#FFAA0F",
                 "#2F9862",
                 "#0077E0",
                 "#170BA5",
                 "#0B6975",
                 "#DB40EF",
                 "#4067E2",
                 "#59CFDB"))
})
