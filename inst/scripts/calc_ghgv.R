#!/usr/bin/env Rscript
library(ghgvcr)

#script args
args   <- commandArgs(trailingOnly = TRUE)
in_dir <- args[1]
out_dir <- args[2]

#load configuration
config_file <- file.path(in_dir, "multisite_config.xml")
config <- XML::xmlToList(xmlParse(config_file))

### Run the GHG calculator V2
res <- ghgvc(config)

### Write the results to a file
write_json(res, out_dir, filename="ghgvc", format = c("json", "csv"))

### Create a plot of the results
#p <- ghgcv_plot(x, outdir, config$options$T_E)






