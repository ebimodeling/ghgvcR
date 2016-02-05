## ------------------------------------------------------------------------
library(ghgvcr)

#load example config data
config_file <- system.file("config_examples/multisite_config.xml", package = "ghgvcr")
config <- XML::xmlToList(XML::xmlParse(config_file))  

#Calculate 
res <- ghgvc(config)

#Write the data to a file:
#outdir <- "./"
#write_json(res, outdir, filename = "ghgvc_example", format="json")

## ---- fig.show='hold'----------------------------------------------------
df <- as.data.frame(res$site_1_data)
#p <- ghgvc_plot(df, save = FALSE)
#p


## ----eval=FALSE----------------------------------------------------------
#  #NOT RUN
#  library(ghgvcr)
#  
#  #Example location
#  latitude <- 40.18
#  longitude <- -89.82
#  
#  #paths
#  netcdf_dir <- "<data directory>"
#  named_ecosystems <- "<named ecosystems file>"
#  
#  #Load the biome data for that location
#  biome <- get_biome(latitude, longitude, netcdf_dir, named_ecosystems, write_data = FALSE)
#  head(biome)
#  
#  #Calculate GHGV
#  #res <- ghgvc(biome)

