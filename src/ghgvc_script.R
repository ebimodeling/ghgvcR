#!/usr/bin/Rscript
library(ghgvcr)
config.xml <- system.file("multisite_config.xml", package = "ghgvcr")


config.list <- xmlToList(xmlParse(config.xml))

x <- ghgvc2(config.list)

writeLines(x, "inst/extdata/output.json")
write.csv(as.data.frame(fromJSON(x)), "inst/extdata/output.csv")
