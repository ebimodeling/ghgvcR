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

########
#Can remove if biophys defaults no longer necessary. All this block does is
#Add biophys sw_radiative_forcing and latent defaults to the site ecosystems.
# 
# #### Hack to put in some defaults for biophysical variables
# biophys.defaults <- structure(
#   list(ecosystem = structure(
#     c(7L, 4L, 5L, 3L, 6L, 1L, 2L),
#     .Label = c("BR_soy", "BR_sugarcane", "miscanthus", "soybean", 
#                "Spring Wheat", "switchgrass", "US_corn"), class = "factor"), 
#     Rnet = c(0.01, 0.06, 0.02, 0.02, 0.05, 0.06, 0.17), 
#     latent = c(0.25, 0.26, 0.13, 0.24, 0.25, 0.1, 0.33)), 
#   .Names = c("ecosystem", "sw_radiative_forcing", "latent"), 
#   class = "data.frame", row.names = c(NA, -7L))
#
#for (site in sites){
#  for(ecosystem.idx in which(names(config.list[[site]]) %in% "pft")){
#    ecosystem_name <- config.list[[site]][[ecosystem.idx]]$name
#    
#   eidx <- biophys.defaults$ecosystem == ecosystem
#   if (any(eidx)){
##      if(!exists(config.list[[site]][[ecosystem.idx]]$sw_radiative_forcing) | 
##           config.list[[site]][[ecosystem.idx]]$sw_radiative_forcing %in% c(NA, 0)){
#        config.list[[site]][[ecosystem.idx]]$sw_radiative_forcing <- biophys.defaults$sw_radiative_forcing[eidx]
##     }
##     if(!exists(config.list[[site]][[ecosystem.idx]]$latent) | 
##          config.list[[site]][[ecosystem.idx]]$latent %in% c(NA, 0)){
#        config.list[[site]][[ecosystem.idx]]$latent <- biophys.defaults$latent[eidx]
##     }
#      
#    }
#  }
#} 

### Run the GHG calculator V2
x <- ghgvc2(config.list)

### Write the results to a file
ghgcv_write(x, outdir, format = c("json", "csv"))


### Create a plot of the results
p = ghgcv_plot(x, outdir, config.list$options$T_E)



mean(1:4)

y
  #sub = annotate("text", x = 2, y = 0.3, parse = T, label = xlabels)))

################
# Can remove if crop defaults no longer needed. Just replaces swRFV and 
# latent with defaults. 
# # Hack to use crop defaults for demo
# crop.defaults <- structure(list(
#   ecosystem = structure(
#     c(7L, 4L, 5L, 3L, 6L, 1L, 2L), 
#     .Label = c("BR_soy", "BR_sugarcane", "miscanthus", "soybean", 
#                "Spring Wheat", "switchgrass", "US_corn"), 
#     class = "factor"), 
#   sw_radiative_forcing = c(3.84, 34.17, 10.47, 9.72, 24.88, 30.12, 90.67), 
#   latent = c(136.27, 141.19, 70.59, 132.53, 137.34, 55.43, 179.2)), 
#   .Names = c("name", "swRFV", "latent"), 
#   class = "data.frame", 
#   row.names = c(NA, -7L)
#   )
# 
# for(name in outdf$name){
#   if(name %in% crop.defaults$name){
#     outdf$swRFV[outdf$name == name] <- crop.defaults[crop.defaults$name == name, "swRFV"]
#     outdf$latent[outdf$name == name] <- crop.defaults[crop.defaults$name == name, "latent"]*2
#   }
#   
# }




