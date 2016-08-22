library(dplyr)
library(readr)

### FAO Biome Lookup Table
fao_biomes <- read_csv("data-raw/fao_biomes.csv")
fao_biomes[is.na(fao_biomes)] <- 0
write_csv(fao_biomes, "inst/extdata/fao_biomes.csv")
#devtools::use_data(fao_biomes, overwrite = TRUE)

### Map to Vegtype Lookup Table
map_vegtypes <- read_csv("data-raw/map_vegtypes.csv")
map_vegtypes[is.na(map_vegtypes)] <- 0
write_csv(map_vegtypes, "inst/extdata/map_vegtypes.csv")
#devtools::use_data(map_vegtypes, overwrite = TRUE)

### Koppen to Biome Lookup Table
koppen_biomes <- read_csv("data-raw/koppen_biomes.csv")
koppen_biomes[is.na(koppen_biomes)] <- 0
write_csv(koppen_biomes, "inst/extdata/koppen_biomes.csv")
#devtools::use_data(koppen_biomes, overwrite = TRUE)

### Biome Defaults
biome_defaults <- read_csv("data-raw/biome_defaults.csv")
write_csv(biome_defaults, "inst/extdata/biome_defaults.csv")
#devtools::use_data(biome_defaults, overwrite = TRUE)


