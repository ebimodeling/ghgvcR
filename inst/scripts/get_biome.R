#!/usr/bin/env Rscript
library(ghgvcr)

#script args
script_args   <- commandArgs(trailingOnly = TRUE)
if (length(script_args) < 5 | length(script_args) > 6) {
  stop("Incorrect number of arguments.\n
        USAGE: 'get_biome.R <latitude> <longitude> <netcdf_dir> <named_ecosystems> <output_dir> [output_filename] [output_format] [write_data]'\n
        latitude:         The latitude to load data for.
        longitude:        The longitude to load data for.
        netcdf_dir:       Full path to the netcdf biome data.
        named_ecosystems: Full path to name-indexed ecosystems json file.
        output_dir:       Full path to write the biome data to.
        write_data:       OPTIONAL Boolean whether to write data to a file.\n")
}



#write data is true by default.
write_data <- TRUE
if (length(script_args) == 6) {
  write_data <- as.logical(script_args[6])
}

#Get the biome data.
get_biome(script_args[1], 
          script_args[2], 
          script_args[3], 
          script_args[4], 
          script_args[5], 
          write_data = write_data)


