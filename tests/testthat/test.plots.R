library("ghgvcr")
library("ggplot2")
library("gridExtra")
library("jsonlite")
context("test that plots work without generating errors.")

test_that("plots are generated without errors", {
  config_file <- system.file("multi_site.xml", package = "ghgvcr")
  if (!file.exists(config_file)) config_file <- "/opt/ghgvc/ghgvcR/inst/config/multi_site.xml"
  
  config <- XML::xmlToList(XML::xmlParse(config_file))  
  
  #Calculator
  y <- ghgvc(config, write_data = FALSE, make_plots = FALSE)

  plot_data <- json2DF(toJSON(y))
  
  p <- plot_ghgv(plot_data, save=FALSE)
  grid.arrange(p)
  expect_is(p, c('gtable', 'grob', 'gDesc'))
})

test_that("sites are ordered correctly", {
  longdata <- data.frame(
    Biome = c("Grass Site 1", "Forest Site 1", "Grass Site 2"), 
    Order = c(12, 11, 22), 
    Location = c(1,1,2))
})
  


