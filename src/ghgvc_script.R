#!/usr/bin/Rscript
library(ghgvcr)

## TODO pass inputdir and outputdir as arguments
homedir <- Sys.getenv("HOME")
inputdir <- file.path(homedir, "ghgvcR/inst")
outputdir <- file.path(homedir, "ghgvcR/inst/extdata")


config.xml <- file.path(inputdir, "multisite_config.xml")
config.list <- xmlToList(xmlParse(config.xml))

x <- ghgvc2(config.list)

writeLines(x, file.path(outputdir, "output.json"))

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
    
write.csv(outdf, file.path(outputdir, "output.csv"))
