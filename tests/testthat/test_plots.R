library("ghgvcr")
library("ggplot2")
library("gridExtra")
library("jsonlite")
context("test that plots work without generating errors.")

single_json_file <- "../data/single_site.json"

test_that("plots are generated without errors", {
  eco <- fromJSON(single_json_file)
  out_json <- calc_ghgv(toJSON(eco, auto_unbox = FALSE))
  
  plot_data <- json_to_df(toJSON(fromJSON(out_json)$results))
  
  p <- plot_ghgv(plot_data)
  grid.arrange(p)
  expect_is(p, c('gtable', 'grob', 'gDesc'))
})

test_that("sites are ordered correctly", {
})
  


