library(dplyr)
library(readr)
setwd("data-raw")

### FAO Biome Lookup Table
fao_biomes <- read_csv("fao_biomes.csv")
fao_biomes[is.na(fao_biomes)] <- 0
devtools::use_data(fao_biomes, overwrite = TRUE)

### Map to Vegtype Lookup Table
map_vegtypes <- read_csv("map_vegtypes.csv")
map_vegtypes[is.na(map_vegtypes)] <- FALSE
map_vegtypes[map_vegtypes == 1] <- TRUE
devtools::use_data(map_vegtypes, overwrite = TRUE)

### Koppen to Biome Lookup Table
koppen_biomes <- read_csv("koppen_biomes.csv")
koppen_biomes[is.na(koppen_biomes)] <- FALSE
koppen_biomes[koppen_biomes == 1] <- TRUE
devtools::use_data(koppen_biomes, overwrite = TRUE)

### Biome Defaults
biome_defaults <- read_csv("biome_defaults.csv")
devtools::use_data(biome_defaults, overwrite = TRUE)


