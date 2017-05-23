library("ghgvcr")
context("test helper functions.")

#Tests are in alphabetical order by function they test.
test_that("decay() correctly calculates decay over time", {
  
})

test_that("extract_ghg_params() correctly creates a matrix from ecosystem
           data.", {
  
})

test_that("extract_pool_params() correctly creates a matrix from ecosystem 
           data", {
             
})

test_that("kinetic_decay() correctly calculates kinetic decay over time", {
  
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

test_that("man/str2LogicalOrNumeric() correctly resturns numerics and strings", {
  expect_true(str2LogicalOrNumeric("0.01") == 0.01)
  expect_true(str2LogicalOrNumeric("0.01s") == "0.01s")
  expect_true(is.na(str2LogicalOrNumeric("$0.01")))
})

