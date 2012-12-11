#!/usr/bin/Rscript
library(ghgvcR)
config.xml <- system.file("config.xml", package = "ghgvcR")


config.list <- xmlToList(xmlParse(config.xml))
options <- config.list$options

ecosystem_data <- config.list$ecosystem_data

x <- ghgvc(options, ecosystem_data)

writeLines(x, "inst/extdata/output.json")
write.csv(as.data.frame(fromJSON(x)), "inst/extdata/output.csv")
