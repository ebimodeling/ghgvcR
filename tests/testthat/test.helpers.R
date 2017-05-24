library("ghgvcr")
context("test helper functions.")

#Tests are in alphabetical order by function they test.
test_that("decay() correctly calculates decay over time", {
  
})

test_that("kinetic_decay() correctly calculates kinetic decay over time", {
  
})

test_that("extract_ghg_params() correctly creates a matrix from ecosystem
           data.", {
  
})

test_that("extract_pool_params() correctly creates a matrix from ecosystem 
           data", {
             
})


test_that("json_to_df returns a data.frame and gracefully fails when fed non-JSON.", {
  json_str <- '{"site_1_data":{"Grass":{"name":["Grass"],"lat":[-15.62],"lng":[-50.69],"S_CO2 [Mg CO2-eq ha-1 100 yrs-1]":[6464.4463],"S_CH4 [Mg CO2-eq ha-1 100 yrs-1]":[1.1149],"S_N2O [Mg CO2-eq ha-1 100 yrs-1]":[1.216],"F_CO2 [Mg CO2-eq ha-1 100 yrs-1]":[0],"F_CH4 [Mg CO2-eq ha-1 100 yrs-1]":[5.0341],"F_N2O [Mg CO2-eq ha-1 100 yrs-1]":[-26.6803],"D_CO2 [Mg CO2-eq ha-1 100 yrs-1]":[6464.4463],"D_CH4 [Mg CO2-eq ha-1 100 yrs-1]":[6.1491],"D_N2O [Mg CO2-eq ha-1 100 yrs-1]":[-25.4644],"swRFV [Mg CO2-eq ha-1 100 yrs-1]":[0],"latent [Mg CO2-eq ha-1 100 yrs-1]":["NaN"],"crv [Mg CO2-eq ha-1 100 yrs-1]":["NaN"]}}}'
  valid_JSON <- toJSON(fromJSON(json_str))
  invalid_JSON <- substr(valid_JSON, 0, 100)
  bad_JSON <- toJSON(json_str)
  expect_true(class(json_to_df(valid_JSON)) == "data.frame")
  expect_error(json_to_df(invalid_JSON))
  expect_error(json_to_df(bad_JSON), 'JSON malformed: Not able to parse to a list.')
})

test_that("remap_range() returns correct values.", {
  #test data
  expect_equal(remap_range(45, -45, 45, 0, 90), 90)
  expect_equal(remap_range(0, -45, 45, 0, 90), 45)
  expect_equal(remap_range(-120, -180, 180, 0, 360), 60)
  expect_equal(remap_range(60, 0, 360, -180, 180), -120)
  expect_equal(remap_range(90, 0, 90, -45, 45), 45)
})


test_that("str2LogicalOrNumeric() correctly handles 'NA', 'NaN', '', and 'null'", {
  expect_true(is.na(str2LogicalOrNumeric("null")), 
              paste0(str2LogicalOrNumeric("null"), " is returned instead."))
  expect_true(is.na(str2LogicalOrNumeric("NA")), 
              paste0(str2LogicalOrNumeric("NA"), " is returned instead."))
  expect_true(is.nan(str2LogicalOrNumeric("NaN")), 
              paste0(str2LogicalOrNumeric("NaN"), " is returned instead."))
  expect_true(is.na(str2LogicalOrNumeric('')), 
              paste0(str2LogicalOrNumeric(''), " is returned instead."))
})

test_that("str2LogicalOrNumeric() correctly returns logicals", {
  expect_true(str2LogicalOrNumeric("TRUE"))
  expect_true(str2LogicalOrNumeric("True"))
  expect_true(str2LogicalOrNumeric("true"))
  expect_true(!str2LogicalOrNumeric("FALSE"))
  expect_true(!str2LogicalOrNumeric("False"))
  expect_true(!str2LogicalOrNumeric("false"))
})

test_that("str2LogicalOrNumeric() correctly returns numerics and strings", {
  expect_true(str2LogicalOrNumeric("0.01") == 0.01)
  expect_true(str2LogicalOrNumeric("0.01s") == "0.01s")
  expect_true(is.na(str2LogicalOrNumeric("$0.01")))
})

