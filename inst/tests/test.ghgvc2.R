test_that("ghgvc2 works for multipft test config file",{
  config.xml <- system.file("multipft_config.xml", package = "ghgvcr")
  config.list <- xmlToList(xmlParse(config.xml))
  x <- ghgvc2(config.list)
  expect_true(class(x) == "character")
  expect_true(grepl("Tropical", x))
  expect_equal("{\"ecosystem_0_data\":[{\"name\":\"Tropical Forest\",\"S_CO2\":3077.19440798194,\"S_CH4\":8.71414987655346,\"S_N2O\":221.440495767305,\"F_CO2\":-777.589630035838,\"F_CH4\":198.153276609512,\"F_N2O\":9.21042004738499,\"D_CO2\":0,\"D_CH4\":0,\"D_N2O\":0},{\"name\":\"Misty Mountain Top\",\"S_CO2\":715.481930448906,\"S_CH4\":18.5897095138668,\"S_N2O\":11.2808552885403,\"F_CO2\":-264.088404311696,\"F_CH4\":-4.53053004775435,\"F_N2O\":43.9477537844218,\"D_CO2\":0,\"D_CH4\":0,\"D_N2O\":0}],\"ecosystem_1_data\":[{\"name\":\"Temperate Forest\",\"S_CO2\":4.56309041699992,\"S_CH4\":0.0587650836752113,\"S_N2O\":0.0181995344292526,\"F_CO2\":-463.068959358755,\"F_CH4\":-4.53053004775435,\"F_N2O\":29.7798338969974,\"D_CO2\":0,\"D_CH4\":0,\"D_N2O\":0},{\"name\":\"Misty Mountain Hop\",\"S_CO2\":271.104915549568,\"S_CH4\":2.34182957152679,\"S_N2O\":2.55419110417906,\"F_CO2\":-247.868281691267,\"F_CH4\":-3.7982437401118,\"F_N2O\":6.74989211681713,\"D_CO2\":0,\"D_CH4\":0,\"D_N2O\":0}]}", x)
})
