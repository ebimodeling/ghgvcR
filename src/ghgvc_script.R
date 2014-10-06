#!/usr/bin/env Rscript

library(ghgvcr)


args   <- commandArgs(trailingOnly = TRUE)
rundir <- args[1]
outdir <- args[2]


config.xml <- file.path(rundir, "multisite_config.xml")
config.list <- xmlToList(xmlParse(config.xml))


#### Hack to put in some defaults for biophysical variables
biophys.defaults <- structure(
  list(ecosystem = structure(
    c(7L, 4L, 5L, 3L, 6L, 1L, 2L),
    .Label = c("BR_soy", "BR_sugarcane", "miscanthus", "soybean", 
               "Spring Wheat", "switchgrass", "US_corn"), class = "factor"), 
  Rnet = c(0.01, 0.06, 0.02, 0.02, 0.05, 0.06, 0.17), 
  latent = c(0.25, 0.26, 0.13, 0.24, 0.25, 0.1, 0.33)), 
  .Names = c("ecosystem", "sw_radiative_forcing", "latent"), 
  class = "data.frame", row.names = c(NA, -7L))
#config.list[[site]][[ecosystem.idx]]$name <- "US_corn"
sites <- names(config.list)[-which(names(config.list)%in% "options")]
for (site in sites){
  for(ecosystem.idx in which(names(config.list[[site]]) %in% "pft")){
    ecosystem <- config.list[[site]][[ecosystem.idx]]$name
    eidx <- biophys.defaults$ecosystem == ecosystem
    if (any(eidx)){
#      if(is.null(config.list[[site]][[ecosystem.idx]]$sw_radiative_forcing) | 
#config.list[[site]][[ecosystem.idx]]$sw_radiative_forcing %in% c(NA, 0)){
        config.list[[site]][[ecosystem.idx]]$sw_radiative_forcing <- biophys.defaults$sw_radiative_forcing[eidx]
#      }
#      if(is.null(config.list[[site]][[ecosystem.idx]]$latent)) | 
#           config.list[[site]][[ecosystem.idx]]$latent %in% c(NA, 0)){
        config.list[[site]][[ecosystem.idx]]$latent <- biophys.defaults$latent[eidx]
#      }
      
    }
  }
} 
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
outdf$swRFV <- - outdf$swRFV



write.csv(outdf, file.path(outdir, "output.csv"), row.names = FALSE)

## Plotting
initial_storage <- outdf[,grepl("S_", colnames(outdf))]
ongoing_exchange <- outdf[,grepl("F_", colnames(outdf))]
Total_GHGV <- outdf[,grepl("GHGV_", colnames(outdf))]
Biophysical <- data.frame(Rnet = outdf$swRFV, LE = outdf$latent)
CRV <- outdf$crv


crop.defaults <- structure(list(ecosystem = structure(c(7L, 4L, 5L, 3L, 6L, 1L, 
                                       2L), .Label = c("BR_soy", "BR_sugarcane", "miscanthus", "soybean", 
                                                       "Spring Wheat", "switchgrass", "US_corn"), class = "factor"), 
               sw_radiative_forcing = c(3.84, 34.17, 10.47, 9.72, 24.88, 
                                        30.12, 90.67), latent = c(136.27, 141.19, 70.59, 132.53, 
                                                                  137.34, 55.43, 179.2)), .Names = c("name", "swRFV", 
                                                                                                     "latent"), class = "data.frame", row.names = c(NA, -7L))

for(name in outdf$name){
  if(name %in% crop.defaults$name){
    outdf$swRFV[outdf$name == name] <- crop.defaults[crop.defaults$name == name, "swRFV"]
    outdf$latent[outdf$name == name] <- crop.defaults[crop.defaults$name == name, "latent"]
  }
  
}
library(ggplot2)
library(gridExtra)
library(scales)
library(Hmisc)

plotdata <- data.frame(Biome = capitalize(paste(gsub("_", " ", gsub("BR", "Brazil", outdf$Biome)), "Site", outdf$Location)),
                       Storage = rowSums(initial_storage, na.rm = TRUE),
                       Ongoing_Exchange = rowSums(ongoing_exchange, na.rm = TRUE),
                       Rnet = outdf$swRFV,
                       LE = outdf$latent)
plotdata$CRV_BGC <- plotdata$Storage + plotdata$Ongoing_Exchange
plotdata$CRV_BIOPHYS <- plotdata$Rnet + plotdata$LE
plotdata$CRV_NET <- plotdata$CRV_BGC + plotdata$CRV_BIOPHYS
plotdata[is.na(plotdata)] <- 0

plotdata$CRV_BGC[plotdata$CRV_NET == 0] <- 0
plotdata$CRV_BIOPHYS[plotdata$CRV_NET == 0] <- 0
plotdata$CRV_NET[plotdata$CRV_NET == 0] <- NA

nolabels <- theme(axis.title = element_blank(), axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank(), legend.position = "top")
baseplot <- ggplot(data = plotdata) + theme_minimal() + coord_flip() + nolabels 


## T_E is number of years considered
YEARS <- config.list$options$T_E
xlabels <- as.expression(bquote(paste("CO"[2], " Emission Equivalents (Mg CO"[2],"-eq ha"^{-1}, " ", .(YEARS)," yrs"^{-1},")")))

biome <- data.frame(order = 1:(nrow(plotdata)+1), Biome = c("", as.character(plotdata$Biome)))

longdata <- melt(plotdata, id.var = "Biome")
longdata$label <- gsub(" Site", "\nSite", longdata$Biome)


ghgvc.subplot <- function(vars, data = longdata){
  d <- subset(longdata, variable %in% vars)
  pos <- d$value > 0
  posplot <- if(any(pos)) {
    geom_bar(data = d[pos,], 
             aes(x = Biome, y = value, fill = variable),  
             width = 0.25, stat = "identity")  
  } else {
    NULL
  }
  negplot <- if(any(!pos)) {
    geom_bar(data = d[!pos,], 
             aes(x = Biome, y = value, fill = variable),  
             width = 0.25, stat = "identity")  
  } else {
    NULL
  }
  return(baseplot + posplot + negplot)
}

bgc.plot <- ghgvc.subplot(c("Storage", "Ongoing_Exchange"), data = longdata) +
  scale_fill_manual(values= brewer_pal(pal = "Greens")(6)[c(4,6)], labels = c("Storage", "Ongoing Exchange")) + labs(fill = "") +
  ggtitle("Biogeochemical") + theme(axis.text.y = element_text(size = 12, hjust = 1))

biophys.plot <- ghgvc.subplot(c("LE", "Rnet"), data = longdata) + 
  scale_fill_manual(values = brewer_pal(pal = "Blues")(6)[c(4,6)], labels = c(expression("LE", "R"["net"]))) + labs(fill = "") + 
  ggtitle("Biophysical")

crv.plot <-  ghgvc.subplot(c("CRV_BGC", "CRV_BIOPHYS"), data = longdata) +
  scale_fill_manual(values= c(brewer_pal(pal = "Greens")(6)[5], brewer_pal(pal = "Blues")(6)[5]), labels = c("Biogeochemical", "Biophysical")) + labs(fill = "") +
  geom_point(data = subset(longdata, variable == "CRV_NET"), aes(x = Biome, y = value)) +
  ggtitle("Climate Regulating Value")

svg(filename=file.path(outdir, "output.svg"), width = 10, height = 1 + nrow(plotdata))

grid.arrange(bgc.plot, biophys.plot, crv.plot, ncol = 3, widths = c(2,1,1),
             sub = textGrob(xlabels, hjust = 0.2))
dev.off()

svgfixcmd <- paste("sed -i 's/symbol id/symbol overflow=\"visible\" id/g'", file.path(outdir, "output.svg"))
system(svgfixcmd)
#sub = annotate("text", x = 2, y = 0.3, parse = T, label = xlabels)))




