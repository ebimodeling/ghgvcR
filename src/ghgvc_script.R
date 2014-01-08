#!/usr/bin/env Rscript

print(.libPaths())

library(ghgvcr)


args   <- commandArgs(trailingOnly = TRUE)
rundir <- args[1]
outdir <- args[2]


config.xml <- file.path(rundir, "multisite_config.xml")
config.list <- xmlToList(xmlParse(config.xml))

x <- ghgvc2(config.list)

writeLines(x, file.path(outdir, "output.json"))

outlist <- fromJSON(x)
outdf <- list()
for(site in names(outlist)){
    for(pft in seq(length(outlist[[site]]))){
        print(site)
        tmplist <- outlist$site_2_data[[pft]]
        tmplist <- lapply(tmplist, function(x) ifelse(is.list(x), NA, x))
        tmpdf <- data.frame(site = site, tmplist)
        outdf <- rbind(outdf, tmpdf)
    }
}
outdf$site <- gsub("site_", "", gsub("_data", "", outdf$site))
    
write.csv(outdf, file.path(outdir, "output.csv"))
