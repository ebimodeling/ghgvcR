library("ghgvcR")
context("testing input-output functions.")

#Tests are ordered by the name of the function they test.
test_that("")

gefs_url <- "http://thredds.ucar.edu/thredds/dodsC/grib/NCEP/GEFS/Global_1p0deg_Ensemble/members/GEFS_Global_1p0deg_Ensemble_20160129_1800.grib2"
con <- nc_open(gefs_url)
