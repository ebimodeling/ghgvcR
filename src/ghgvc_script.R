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

## Cleaning up output for downloading
outdf$site <- gsub("site_", "", gsub("_data", "", outdf$site))

Location <- as.numeric(gsub("_data", "", gsub("site_", "", outdf$site)))
outdf$site <- NULL
outdf <- cbind(Location, outdf)
outdf <- outdf[order(outdf$Location),]

colnames(outdf)[colnames(outdf) == "name"] <- "Biome"

colnames(outdf) <- gsub("D_", "GHGV_", colnames(outdf))

outdf[outdf == 0] <- NA

write.csv(outdf, file.path(outdir, "output.csv"))

## Plotting
