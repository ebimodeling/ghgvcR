#' Get biome data from a netCDF file.
#' 
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @export
#' 
#' @param data_dir directory containing the data file.
#' @param ncfile name of the netCDF file.
#' @param latitude the latitude.
#' @param longitude the longitude.
#' @param variables the names of variables to select. If missing then all are 
#'   returned.
#' @return JSON of netcdf results. 
get_ncdf <- function(data_dir, ncfile, latitude, longitude, variables = "all") {
  
  ncfile <- paste0(data_dir, ncfile)
  if (file.exists(ncfile)) {
    #get connection
    con <- nc_open(ncfile)
  } 
  else stop(paste0("file ", ncfile, " doesn't exist."))
  
  #check dimension names to get lat/lon indices
  dim_names <- names(con$dim)
  #Get latitude and longitude indexes
  if("latitude" %in% dim_names) {
    lats <- con$dim$latitude$vals
    lons <- con$dim$longitude$vals
  }
  else if("x" %in% dim_names) { #x,y coords, x == lon, y == lat
    lats <- con$dim$y$vals
    lons <- con$dim$x$vals
  }
  else if("lat" %in% dim_names) {
    lats <- con$dim$lat$vals
    lons <- con$dim$lon$vals
  }
  else {
    stop(paste("Error: dimension names", 
                dim_names, 
                "not found in ghgvcr::get_ncdf()."))
  }
    
  #Variables are all variables unless given
  varnames <- names(con$var)
  if (missing(variables) | variables == "all") variables <- varnames
  if (sum(variables %in% varnames) != length(variables))
    
    stop(paste0("variable(s): c(", variables, 
                ") are not in variable(s): c(", paste(varnames, collapse=", "), 
                ") in file ", ncfile))
  
  inbounds = FALSE
  if (min(lats) <= latitude & latitude <= max(lats) & 
      min(lons) <= longitude & longitude <= max(lons)) {
    inbounds = TRUE
      
    lat_idx <- remap_range(latitude, lats[1], lats[length(lats)], 1, length(lats))
    lng_idx <- remap_range(longitude, lons[1], lons[length(lons)], 1, length(lons))
    
    #Set up start and count vectors. Just a single value is retreived.
    #old netcdf files had 4 dimensions. New just has lat/lng and a time
    
    #Hack to fix wrong ordering of FAO/GEZ data file.
    if("gez_abbrev" %in% variables) {
      start <- c(1, lng_idx, lat_idx)
      count <- c(5, 1,1)
    }
    else {
      start <- c(lng_idx, lat_idx)
      count <- c(1,1)
    }
  }
  
  d <- list()
  for (v in variables) {
    if (inbounds) {
      ndims_leftover <- con$var[[v]]$ndims - length(start)
      d[v] <- ncvar_get(con, v, 
                        c(start, rep(1,ndims_leftover)), 
                        c(count, rep(1,ndims_leftover)))
    } else {
      d[v] <- NA
    }
  }
  nc_close(con)
  d
}

#' Get netcdf variable names.
#' 
#' Wrapper to simplify checking variables in a netcdf file.
#' 
#' @importFrom ncdf4 nc_open nc_close
#' 
#' @param file_name file name (relative to working directory).
#' @return list of variable names. 
get_ncdf_vars <- function(file_name) {
  if (file.exists(file_name)) {
    con <- nc_open(file_name)
    vars <- names(con$var)
    nc_close(con)
    return(vars)
  }
  else stop(paste0("file ", file_name, " doesn't exist"))
}











