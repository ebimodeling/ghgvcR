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
        tmplist <- outlist[[site]][[pft]]
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
outdf$GHGV <- outdf$GHGV_CO2 + outdf$GHGV_CH4 + outdf$GHGV_N2O

outdf <- outdf[order(outdf$Location),]
write.csv(outdf, file.path(outdir, "output.csv"), row.names = FALSE)

## Plotting
initial_storage <- outdf[,grepl("S_", colnames(outdf))]
ongoing_exchange <- outdf[,grepl("F_", colnames(outdf))]
Total_GHGV <- outdf[,grepl("GHGV_", colnames(outdf))]
Biophysical <- data.frame(Rnet = outdf$swRFV, LE = outdf$latent)
CRV <- outdf$crv


library(ggplot2)
library(gridExtra)

library(Hmisc)

plotdata <- data.frame(Biome = capitalize(paste(gsub("_", " ", gsub("BR", "Brazil", outdf$Biome)), "Site", outdf$Location)),
                       Storage = rowSums(initial_storage, na.rm = TRUE),
                       Ongoing_Exchange = rowSums(ongoing_exchange, na.rm = TRUE),
                       Rnet = outdf$swRFV,
                       LE = outdf$latent,
                       CRV_BGC = outdf$crv)
plotdata[is.na(plotdata)] <- 0

nolabels <- theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
baseplot <- ggplot(data = plotdata) + theme_minimal() + coord_flip() + nolabels


## T_E is number of years considered
YEARS <- config.list$options$T_E
xlabels <- as.expression(bquote(paste("CO"[2], " Emission Equivalents (Mg CO"[2],"-eq ha"^{-1}, " ", .(YEARS)," yrs"^{-1},")")))

ylabels <- baseplot + geom_text(aes(x = plotdata$Biome, y = 1, label = gsub(" Site", "\nSite", plotdata$Biome)), 
                                hjust = 1) + ylim(0,1) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

storage.plot <- baseplot + 
  geom_bar(data = plotdata, aes(x = Biome, y = Storage), fill = "DarkGreen", width = 0.25, stat = "identity") +  ggtitle("Biogeochemical")
ongoing.plot <- baseplot +
  geom_bar(data = plotdata, aes(x = Biome, y = Ongoing_Exchange), fill = "DarkBlue", width = 0.25, stat = "identity") + ggtitle("Biophysical")
crv.plot <- baseplot +
  geom_bar(data = plotdata, aes(x = Biome, y = CRV_BGC), fill = "Grey", width = 0.25, stat = "identity") + ggtitle("Climate Regulating Value")


newplot <- grid.arrange(arrangeGrob(ylabels, storage.plot, ongoing.plot, crv.plot, ncol = 4,
                         #main = "",
                         sub = textGrob(xlabels, hjust = 0.4)))
                         #sub = annotate("text", x = 2, y = 0.3, parse = T, label = xlabels)))





