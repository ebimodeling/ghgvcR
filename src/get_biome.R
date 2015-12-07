#!/usr/bin/env Rscript
library(ghgvcr)

#script args
args   <- commandArgs(trailingOnly = TRUE)
rundir <- args[1]
outdir <- args[2]
ncfile <- args[3]
latitude <- as.numeric(args[4])
longitude <- as.numeric(args[5])

#Get biome data
biome <- ghgvcr::get_biome(ncfile, latitude, longitude)
print(biome)

#Write to file as json
biome_write(biome, outdir, format = c("json"))


