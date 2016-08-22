library(ncdf4)

mapdir <- "/run/media/potterzot/zfire1/work/ebimodeling/netcdf/GCS/Maps"

kg_file <- "koppen_geiger.nc"
gez_file <- "gez_2010_wgs84.nc"
synmap_file <- "Hurtt_SYNMAP_Global_HD_2010.nc"

con1 <- nc_open(paste(mapdir, synmap_file, sep="/"))
b <- ncvar_get(con1, "biome_type", c(1,1))
nc_close(con1)

con1 <- nc_open(paste(mapdir, kg_file, sep="/"))
b <- ncvar_get(con1, "Band1", c(1,1))
nc_close(con1)

con1 <- nc_open(paste(mapdir, gez_file, sep="/"))
b <- ncvar_get(con1, "gez_name", c(1,1,1))
nc_close(con1)

###SYNMAP NUMERIC TO CATEGORY
synmap_codes <- data.frame(
  code = seq(0,47,1),
  name = c("Water",
           "Trees / Needle / Evergreen",
           "Trees / Needle / Deciduous",
           "Trees / Needle / Mixed",
           "Trees / Broad / Evergreen",
           "Trees / Broad / Deciduous",
           "Trees / Broad / Mixed",
           "Trees / Mixed / Evergreen",
           "Trees / Mixed / Deciduous",
           "Trees / Mixed / Mixed",
           "Trees & Shrubs / Needle / Evergreen",
           "Trees & Shrubs / Needle / Deciduous",
           "Trees & Shrubs / Needle / Mixed",
           "Trees & Shrubs / Broad / Evergreen",
           "Trees & Shrubs / Broad / Deciduous",
           "Trees & Shrubs / Broad / Mixed",
           "Trees & Shrubs / Mixed / Evergreen",
           "Trees & Shrubs / Mixed / Deciduous",
           "Trees & Shrubs / Mixed / Mixed",
           "Trees & Grasses / Needle / Evergreen",
           "Trees & Grasses / Needle / Deciduous",
           "Trees & Grasses / Needle / Mixed",
           "Trees & Grasses / Broad / Evergreen",
           "Trees & Grasses / Broad / Deciduous",
           "Trees & Grasses / Broad / Mixed",
           "Trees & Grasses / Mixed / Evergreen",
           "Trees & Grasses / Mixed / Deciduous",
           "Trees & Grasses / Mixed / Mixed",
           "Trees & Crops / Needle / Evergreen",
           "Trees & Crops / Needle / Deciduous",
           "Trees & Crops / Needle / Mixed",
           "Trees & Crops / Broad / Evergreen",
           "Trees & Crops / Broad / Deciduous",
           "Trees & Crops / Broad / Mixed",
           "Trees & Crops / Mixed / Evergreen",
           "Trees & Crops / Mixed / Deciduous",
           "Trees & Crops / Mixed / Mixed",
           "Shrubs",
           "Shrubs & Grasses",
           "Shrubs & Crops",
           "Shrubs & Barren",
           "Grasses",
           "Grasses & Crops", 
           "Grasses & Barren",
           "Crops",
           "Barren",
           "Urban",
           "Snow & Ice")
)


a <- list("x" = 1, "y" = 2)








