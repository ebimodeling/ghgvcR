library("ghgvcr")
context("testing biome functions.")

test_that("remap_range() returns correct values.", {
  #test data
  expect_equal(remap_range(45, -45, 45, 0, 90), 90)
  expect_equal(remap_range(0, -45, 45, 0, 90), 45)
  expect_equal(remap_range(-120, -180, 180, 0, 360), 60)
  expect_equal(remap_range(60, 0, 360, -180, 180), -120)
  expect_equal(remap_range(90, 0, 90, -45, 45), 45)
})

test_that("get_biome() correctly reads values from biome ncdf4 files.", {
  
  latitude <- 31
  longitude <- 121
  
  #paths
  netcdf_dir <- "/run/media/potterzot/zfire11/work/ebimodeling/netcdf/"
  #netcdf_dir <- "/opt/ghgvc/ghgvc/netcdf/"
  biome_defaults_file <- "inst/extdata/final_ecosystems.json"
  mapdata_dir <- "inst/extdata/"
  
  #Load the biome data for that location
  biome <- ghgvcr::get_biome(latitude, longitude, biome_defaults_file, 
                     netcdf_dir, mapdata_dir, 
                     write_data = FALSE)

  #tests of biome results
  expect_equal(typeof(biome), "list")
})

