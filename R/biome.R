#' Get biome data from a netCDF file.
#' 
#' @export
#' @import ncdf
#' @importFrom jsonlite toJSON 
#'
#' @param ncfile name of the netCDF file.
#' @param latitude the latitude of the biome.
#' @param longitude the longitude of the biome.
#' @return JSON of biome data. 
get_biome <- function(ncfile, latitude, longitude) {
  
  #get connection
  con <- open.ncdf(ncfile)
  
  #Get latitude and longitude indexes
  lats <- con$dim$latitude$vals
  lons <- con$dim$longitude$vals
  
  lat_idx <- remap_range(latitude, min(lats), max(lats), 1, length(lats))
  lng_idx <- remap_range(longitude, min(lons), max(lons), 1, length(lons))
  
  #Set up start and count vectors. Just a single value is retreived.
  #old netcdf files had 4 dimensions. New just has lat/lng
  start <- c(lng_idx, lat_idx)
  count <- c(1,1)
  
  #Variable is the last variable
  var_name <- names(con$var)[con$nvars]
  toJSON(get.var.ncdf(con, var_name, start, count), auto_unbox = TRUE)
}

#' Write Biome Data
#' 
#' importFrom jsonlite fromJSON
#' @export
#' 
#' @param df the biome data.
#' @param outdir the directory to write the data to.
#' @param filename (character) name of file to write.
#' @param format (character) file format to write.
#' @return TRUE if written with no errors.
biome_write <- function(df, outdir, filename="biome", format=c("json", "csv")) {
  formats <- match.arg(format, several.ok = TRUE)
  
  #JSON
  if ("json" %in% formats) {
    writeLines(as.character(df), file.path(outdir, paste0(filename, ".json")))
  }
  
  #CSV
  if ("csv" %in% formats) {
    outlist <- fromJSON(df)
    outdf <- tmpdf <- list()
    for(site in names(outlist)){
      for(pft in seq(length(outlist[[site]]))){
        tmplist <- outlist[[site]][[pft]]
        tmplist <- lapply(tmplist, function(df) ifelse(is.list(df), NA, df))
        tmplist <- lapply(tmplist, function(df) ifelse(df == 0, NA, df))
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
    write.csv(outdf, file.path(outdir, paste0(filename,".csv")), row.names = FALSE)
  }
}





#' Remap latitude and longitude ranges.
#' 
#' @export
#' 
#' @param input the value to remap.
#' @param input_min minimum input value.
#' @param input_max maximum input value.
#' @param output_min minimum output value.
#' @param output_max maximum output value.
remap_range <- function(input, input_min, input_max, output_min, output_max) {
  #Adapted from RUBY code  
  # def remap_range(input, in_low, in_high, out_low, out_high)
  #   # map onto [0,1] using input range
  #   frac = ( input - in_low ) / ( in_high - in_low )
  #   # map onto output range
  #   ( frac * ( out_high - out_low ) + out_low ).to_i.round()
  # end
  frac <- min(max((input - input_min) / (input_max - input_min), 0), 1)
  as.integer(round((frac * (output_max - output_min)) + output_min))
}


