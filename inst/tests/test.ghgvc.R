test_that("run.ghgvc works for test config file",{
  config.xml <- system.file("config.xml", package = "ghgvcr")
  config.list <- xmlToList(xmlParse(config.xml))  
  x <- ghgvcr::ghgvc(options = config.list$options,
                     ecosystem_data = config.list$ecosystem_data)
  expect_true(class(x) == "character")
   expect_true(grepl("Tropical", x))
   expect_equal("[{\"name\":\"Tropical Forest\",\"S_CO2\":715.481930448906,\"S_CH4\":18.5897095138668,\"S_N2O\":11.2808552885403,\"F_CO2\":-264.088404311696,\"F_CH4\":-4.53053004775435,\"F_N2O\":43.9477537844218,\"D_CO2\":0,\"D_CH4\":0,\"D_N2O\":0},{\"name\":\"Misty Mountain Top\",\"S_CO2\":183.54445481929,\"S_CH4\":140.39500394662,\"S_N2O\":446.523494486041,\"F_CO2\":-264.088404311696,\"F_CH4\":-4.53053004775435,\"F_N2O\":43.9477537844218,\"D_CO2\":0,\"D_CH4\":0,\"D_N2O\":0}]", x)
})
