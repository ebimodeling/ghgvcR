#!/usr/bin/env Rscript
library(ghgvcr)

#script args
args   <- commandArgs(trailingOnly = TRUE)
rundir <- args[1]
outdir <- args[2]

#load configuration
config.xml <- file.path(rundir, "multisite_config.xml")
config.list <- xmlToList(xmlParse(config.xml))

###For each site, load ecosystem data
#subtract options
sites <- names(config.list)[-which(names(config.list) %in% "options")]

### Run the GHG calculator V2
x <- ghgvc(config.list)

### Write the results to a file
ghgcv_write(x, outdir, format = c("json", "csv"))

### Create a plot of the results
p <- ghgcv_plot(x, outdir, config.list$options$T_E)






