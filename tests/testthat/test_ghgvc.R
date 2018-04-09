library(ghgvcr)
library(XML)
library(jsonlite)
context("ghgvc")

### LOAD TEST FILES ###
single_xml_file <- "../data/single_site.xml"
single_json_file <- "../data/single_site.json"
multi_xml_file <- "../data/multi_site.xml"
multi_json_file <- "../data/multi_site.json"

out_single <- list(
  name = "Tropical_Forest",
  lat = -15.62,
  lng = -50.69,
  `S_CO2 [Mg CO2-eq ha-1 100 yrs-1]`  = 1210.868,
  `S_CH4 [Mg CO2-eq ha-1 100 yrs-1]`  = 31.4609,
  `S_N2O [Mg CO2-eq ha-1 100 yrs-1]`  = 19.0915,
  `F_CO2 [Mg CO2-eq ha-1 100 yrs-1]`  = 446.9381,
  `F_CH4 [Mg CO2-eq ha-1 100 yrs-1]`  = 7.6674,
  `F_N2O [Mg CO2-eq ha-1 100 yrs-1]`  = -74.3763,
  `D_CO2 [Mg CO2-eq ha-1 100 yrs-1]`  = 1657.806,
  `D_CH4 [Mg CO2-eq ha-1 100 yrs-1]`  = 39.1282,
  `D_N2O [Mg CO2-eq ha-1 100 yrs-1]`  = -55.2848,
  `swRFV [Mg CO2-eq ha-1 100 yrs-1]`  = NA,
  `latent [Mg CO2-eq ha-1 100 yrs-1]` = NA,
  `crv [Mg CO2-eq ha-1 100 yrs-1]`    = NA)

# out_multi <- list(
#   name = "Tropical_Forest",
#   lat = -15.62,
#   lng = -50.69,
#   `S_CO2 [Mg CO2-eq ha-1 100 yrs-1]`  = 1210.868,
#   `S_CH4 [Mg CO2-eq ha-1 100 yrs-1]`  = 31.4609,
#   `S_N2O [Mg CO2-eq ha-1 100 yrs-1]`  = 19.0915,
#   `F_CO2 [Mg CO2-eq ha-1 100 yrs-1]`  = 446.9381,
#   `F_CH4 [Mg CO2-eq ha-1 100 yrs-1]`  = 7.6674,
#   `F_N2O [Mg CO2-eq ha-1 100 yrs-1]`  = -74.3763,
#   `D_CO2 [Mg CO2-eq ha-1 100 yrs-1]`  = 1657.806,
#   `D_CH4 [Mg CO2-eq ha-1 100 yrs-1]`  = 39.1282,
#   `D_N2O [Mg CO2-eq ha-1 100 yrs-1]`  = -55.2848,
#   `swRFV [Mg CO2-eq ha-1 100 yrs-1]`  = NA,
#   `latent [Mg CO2-eq ha-1 100 yrs-1]` = NA,
#   `crv [Mg CO2-eq ha-1 100 yrs-1]`    = NA)

test_that("calc_ghgv works with single site json", {
  eco <- fromJSON(single_json_file)
  out_json <- calc_ghgv(toJSON(eco, auto_unbox = FALSE))
  
  #validate the json
  validate(out_json)
  
  #check actual results
  out <- fromJSON(out_json)
  expect_true(grepl("Tropical_Forest", names(out$results$site_1_data)))
  
  #test actual results match w/in error range
  lapply(setdiff(names(out$results$site_1_data$Tropical_Forest), c("name")), function(key) {
    if(is.na(out_single[[key]])) {
      expect_true(is.na(out$results$site_1_data$Tropical_Forest[[key]]))
    } else {
      expect_true((out$results$site_1_data$Tropical_Forest[[key]] - out_single[[key]])^2 < 1e-6)
    }
  })
})
  
test_that("calc_ghgv works with single site xml", {
  eco <- xmlParse(single_xml_file, validate = F)
  out_json <- calc_ghgv(eco, is_xml = TRUE)
  
  #validate the json
  validate(out_json)
  
  #check actual results
  out <- fromJSON(out_json)
  expect_true(grepl("Tropical_Forest", names(out$results$site_1_data)))
  
  #test actual results match w/in error range
  lapply(setdiff(names(out$results$site_1_data$Tropical_Forest), c("name")), function(key) {
    if(is.na(out_single[[key]])) {
      expect_true(is.na(out$results$site_1_data$Tropical_Forest[[key]]))
    } else {
      expect_true((out$results$site_1_data$Tropical_Forest[[key]] - out_single[[key]])^2 < 1e-6)
    }
  })
})

# test_that("calc_ghgv works with multi site json", {
#   eco <- fromJSON(multi_json_file)
#   out_json <- calc_ghgv(toJSON(eco, auto_unbox = FALSE))
#   
#   #validate the json
#   validate(out_json)
#   
#   #check actual results
#   out <- fromJSON(out_json)
#   expect_true(grepl("Tropical_Forest", names(out$results$site_1_data)))
#   
#   # test actual results match w/in error range
#   # We iterate through sites, ecosystems, and results
#   lapply(names(out$results), function(site) {
#     lapply(names(out$results[[site]]), function(eco) {
#       lapply(setdiff(names(out$results[[site]][[eco]]), c("name")), function(var) {
#         if(is.na(out_multi[[site]][[eco]][[var]])) {
#           expect_true(is.na(out$results[[site]][[eco]][[var]]))
#         } else {
#           expect_true((out$results[[site]][[eco]][[var]] - out_multi[[site]][[eco]][[var]])^2 < 1e-6)
#         }
#       })
#     })
#   })
# })
# 
# test_that("calc_ghgv works with multi site xml", {
#   eco <- xmlParse(multi_xml_file, validate=F)
#   out_json <- calc_ghgv(eco, is_xml = TRUE)
#   
#   #validate the json
#   validate(out_json)
#   
#   #check actual results
#   out <- fromJSON(out_json)
#   expect_true(grepl("Tropical_Forest", names(out$results$site_1_data)))
#   
#   #test actual results match w/in error range
#   lapply(setdiff(names(out$results$site_1_data$Tropical_Forest), c("name")), function(key) {
#     expect_true((out$results$site_1_data$Tropical_Forest[[key]] - out_test[[key]])^2 < 1e-6)
#   })
# })



