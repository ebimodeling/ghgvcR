library(ghgvcr)
library(XML)
library(jsonlite)
context("ghgvc")

test_that("calc_ghgv works for test config file",{
  #config file location 
  config_file <- system.file("config.xml", package = "ghgvcr")
  if (!file.exists(config_file)) config_file <- "inst/config/single_site.xml"
  
  config <- XML::xmlToList(XML::xmlParse(config_file, validate=F))
  
  #Calculator
  x <- calc_ghgv(toJSON(config, auto_unbox = TRUE))
  
  expect_true(class(x) == "json")
  expect_true(grepl("Tropical Forest", names(x$ecosystem_data)))
  config_output <- "list(`Tropical Forest` = list(name = \"Tropical Forest\", S_CO2 = 715.481930448906, S_CH4 = 18.5897095138668, S_N2O = 11.2808552885403, F_CO2 = 264.088404311696, F_CH4 = 4.53053004775435, F_N2O = -43.9477537844218, D_CO2 = 0, D_CH4 = 0, D_N2O = 0, swRFV = 279.415509014268, latent = 238.297006650775, crv = 928.905173462847))"
  expect_equal(as.character(x), config_output)
})

test_that("run.ghgvc works for test multisite_config file",{
  #config file location 
  ms_config_file <- system.file("multisite_config3.xml", package = "ghgvcr")
  if (!file.exists(ms_config_file)) ms_config_file <- "/opt/ghgvc/ghgvcR/inst/config/multisite_config3.xml"
  
  ms_config <- XML::xmlToList(XML::xmlParse(ms_config_file))  
  
  #Calculator
  y <- calc_ghgv(toJSON(ms_config, auto_unbox = TRUE))
  
  expect_true(class(y) == "list")
  expect_equal(c("BR_sugarcane", "BR_soy"), names(y[["site_1_data"]]))
  config_output <- "list(temperate_forest = list(name = \"temperate_forest\", lat = numeric(0), lng = numeric(0), S_CO2 = 1069.36707552074, S_CH4 = 33.62052295685, S_N2O = 22.2154492623296, F_CO2 = 295.249835209251, F_CH4 = 4.53044980599371, F_N2O = -13.8697519920716, D_CO2 = 0, D_CH4 = 0, D_N2O = 0, swRFV = 279.415509014268, latent = 238.297006650775, crv = 1369.99507839959), temperate_cropland = list(name = \"temperate_cropland\", S_CO2 = 20.2650656873448, S_CH4 = 0.602342148439195, S_N2O = 0.186545240525767, F_CO2 = -26.3094433541775, F_CH4 = 2.51698342701926, \n    F_N2O = -81.5458484040942, D_CO2 = 0, D_CH4 = 0, D_N2O = 0, swRFV = 0, latent = numeric(0), crv = numeric(0)))"
  expect_equal(as.character(y[1]), config_output)
})
