library(ncdf4)
library(raster)


# This script takes 3-D NetCDF file with time (month) as 3rd dimension and convert it to 2-D NetDCF file where montly values are averaged.
## When data are carbon pools the averall the average is taken,
## When data are carbon fluxes A yearly sum is calculated and the average of yearly sum is taken for the output

setwd('R:/Global Maps Data/MsTMIP CLM-4') 
getwd()


# Set vectors of montly variables we want to convert and if they are pools or fluxes (names should be same as variables as metions in name of .nc file)

Variables <- c("AbvGrndWood", "CarbPools", "NEE", "TotLivBiom", "TotSoilCarb")
Carbon <- c("Pools", "Pools", "Fluxes", "Pools", "Pools")
Units <- c("kg C m-2", "kg C m-2", "kg C m-2 s-1", "kg C m-2", "kg C m-2")
Names <- c("Yearly Above ground woody biomass", "Yearly Size of each carbon pool", "Yearly Net Ecosystem Exchange", "Yearly Total living biomass", "Yearly Total Soil Carbon")


# set 2 model names (RG1 and SG1)

Models <- c("RG1", "SG1")

# convert all files with a loop

for (m in Models){
  
  print(m)
  
  for (i in 1:length(Variables)){
    
    v <- Variables[i]
    c <- Carbon[i]
    u <- Units[i]
    n <- Names[i]
    
    print(v)
    
    input.name <- paste0("CLM4_", m, "_Monthly_", v, ".nc4")
    output.name <- paste0("CLM4_", m, "_", v, ".nc")
    
    
  
    # open input NetCDF as brick and set CRS
    
    r <- brick(input.name, varname = v )
    crs(r)<-projm
    
    # Average values over all month (carbon pools) or sum values aver months of each year and then average (carbon fluxes)
    
    if(c == "Pools") {
    r.avg <- stackApply(r, indices = rep(1, 1320), mean) # mean of all layers
    }
    if(c == "Fluxes"){
    r.avg <- stackApply(r, indices = rep(1:110, each = 12), sum) # sum per year 
    r.avg <- stackApply(r, indices = rep(1, 110), mean) # mean of all years
    }
    
    # save values
    
    r.avg_vals <- as.matrix(r.avg)
   
    #save them in a one layer raster and write it as NetDCF file
    
    r1 <- raster(r[[1]])
    r1 <- setValues(r1, r.avg_vals)
    
    writeRaster(r1, output.name, format = "CDF",  overwrite = TRUE, varname = v, longname = n, varunit = u , xname = "lon", yname = "lat", NAflag = -9999)
    
  
 }
}












