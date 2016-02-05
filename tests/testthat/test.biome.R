library("ghgvcR")
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
  latitude <- 40.18
  longitude <- -89.82
  
  #paths
  netcdf_dir <- "/run/media/potterzot/zfire1/work/ebimodeling/netcdf/"
  named_ecosystems <- "/run/media/potterzot/zfire1/work/ebimodeling/public/data/final_ecosystems.json"
  
  #Load the biome data for that location
  biome <- get_biome(latitude, longitude, netcdf_dir, named_ecosystems, write_data = FALSE)

  #tests of biome results
  expect_equal(typeof(biome), "list")
})

