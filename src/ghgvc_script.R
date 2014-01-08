#!/usr/bin/env Rscript

library(ghgvcr)


args   <- commandArgs(trailingOnly = TRUE)
rundir <- args[1]
outdir <- args[2]


config.xml <- file.path(rundir, "multisite_config.xml")
config.list <- xmlToList(xmlParse(config.xml))

x <- ghgvc2(config.list)

writeLines(x, file.path(outdir, "output.json"))

outlist <- fromJSON(x)
outdf <- tmpdf <- list()
for(site in names(outlist)){
    for(pft in seq(length(outlist[[site]]))){
        tmplist <- outlist$site_2_data[[pft]]
        tmplist <- lapply(tmplist, function(x) ifelse(is.list(x), NA, x))
        tmplist <- lapply(tmplist, function(x) ifelse(x == 0, NA, x))
        tmplist[tmplist == "NaN"] <- NA
        Location <- as.numeric(gsub("_data", "", gsub("site_", "", site)))
        ## Temporary hack needs to be fixed
        ## https://github.com/ebimodeling/ghgvcR/issues/3
        ## https://github.com/ebimodeling/ghgvc/issues/19
        tmpdf <- data.frame(Location = Location, tmplist)

        tmp <- as.numeric(tmpdf[!colnames(tmpdf) %in% c("Location", "name")])
        tmpdf[!colnames(tmpdf) %in% c("Location", "name")] <- round(tmp, digits = 1)
        
        outdf <- rbind(outdf, tmpdf)
    }
}

## Cleaning up output for downloading

colnames(outdf)[colnames(outdf) == "name"] <- "Biome"
colnames(outdf) <- gsub("D_", "GHGV_", colnames(outdf))

outdf <- outdf[order(outdf$Location),]
write.csv(outdf, file.path(outdir, "output.csv"))

## Plotting
initial_storage <- outdf[,grepl("S_", colnames(outdf))]
ongoing_exchange <- outdf[,grepl("F_", colnames(outdf))]
Total_GHGV <- outdf[,grepl("GHGV_", colnames(outdf))]
Biophysical <- data.frame(Rnet = outdf$swRFV, LE = outdf$latent)
CRV <- outdf$crv

## T_E is number of years considered
T_E <- config.list$options$T_E
xlabels <- paste0("CO_2 Emission Equivalents (Mg CO_2-eq ha-1 ",  T_E, " yrs-1)")

plotdata <- data.frame(outdf$Biome,
                       Storage = rowSums(initial_storage),
                       Ongoing_Exchange = rowSums(ongoing_exchange),
                       Rnet = outdf$swRFV,
                       LE = outdf$latent,
                       CRV_BGC = )

ggplot() + geom_bar(data = st.bars, aes(x = values, y = values))

## Dummy plot for now
file.copy(system.file("extdata/output.svg", package = "ghgvcr"), file.path(outdir, "output.svg"))
