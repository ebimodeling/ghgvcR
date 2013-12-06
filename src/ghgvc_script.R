#!/usr/bin/Rscript
library(ghgvcr)

## TODO pass inputdir and outputdir as arguments
inputdir <- "/home/ubuntu/ghgvcR/inst"
outputdir <- "/home/ubuntu/ghgvcR/inst/extdata"

config.xml <- file.path(inputdir, "multisite_config.xml")
config.list <- xmlToList(xmlParse(config.xml))

x <- ghgvc2(config.list)

writeLines(x, file.path(outputdir, "output.json"))
write.csv(as.data.frame(fromJSON(x)), file.path(outputdir, "output.csv"))
