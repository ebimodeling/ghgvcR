#' Get biome data from netcdf and json files and return and/or write a config 
#' file.
#' 
#' @export
#' @importFrom jsonlite toJSON fromJSON
#'
#' @param latitude the selected latitude.
#' @param longitude the selected longitude.
#' @param netcdf_dir full path to the directory containing the netcdf data files.
#' @param named_ecosystems full path of the name-indexed ecosystem json file.
#' @param output_dir full path of the directory to write results to.
#' @param output_filename name of file to write (without extension).
#' @param output_format format to save data in.
#' @param write boolean whether to write the data.
#' @return JSON of biome data. 
get_biome <- function(latitude, 
                      longitude,
                      netcdf_dir, 
                      named_ecosystems, 
                      output_dir, 
                      output_filename = "biome",
                      output_format = c("json", "cvs"),
                      write_data = TRUE) {
  if (write_data== TRUE && missing(output_dir)) 
    stop("'output_dir' cannot be missing if write_data is TRUE.")
 
  output_format <- match.arg(output_format)
  
  #convert lat/lon to floats if they are strings
  if(typeof(latitude)=="character") latitude <- as.numeric(latitude)
  if(typeof(longitude)=="character") longitude <- as.numeric(longitude)
   
  #results are a list
  res <- list()
  
  #list of data sources
  data_list <- list(
    "saatchi_agb_num" = list(
      ncdir = "",
      ncfile = "saatchi.nc",
      variable = "agb_1km"
    ),
    "saatchi_bgb_num" = list(
      ncdir = "",
      ncfile = "saatchi.nc",
      variable = "bgb_1km"
    ),
    "nbcd_num" = list(
      ncdir = "",
      ncfile = "nbcd.nc",
      variable = "reprojx1"
    ),
    "soc_num" = list(
      ncdir = "",
      ncfile = "SoilCarbonDataS.nc",
      variable = "HWSDa_OC_Dens_Sub_5min.rst"
    ),
    "global_bare_latent_heat_flux_num" = list(
      ncdir = "GCS/PotVeg/Bare/",
      ncfile = "global_bare_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "global_bare_net_radiation_num" = list(
      ncdir = "GCS/PotVeg/Bare/",
      ncfile = "global_bare_rnet_10yr_avg.nc",
      variable = "rnet"
    ),
    "us_corn_latent_heat_flux_num" = list(
      ncdir = "GCS/Crops/US/Corn/",
      ncfile = "us_corn_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "us_corn_net_radiation_num" = list(
      ncdir = "GCS/Crops/US/Corn/",
      ncfile = "us_corn_rnet_10yr_avg.nc",
      variable = "netrad"
    ),
    "us_misc_latent_heat_flux_num" = list(
      ncdir = "GCS/Crops/US/MXG/",
      ncfile = "us_mxg_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "us_misc_net_radiation_num" = list(
      ncdir = "GCS/Crops/US/MXG/",
      ncfile = "us_mxg_rnet_10yr_avg.nc",
      variable = "netrad"
    ),
    "us_soy_latent_heat_flux_num" = list(
      ncdir = "GCS/Crops/US/Soybean/",
      ncfile = "us_soyb_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "us_switch_latent_heat_flux_num" = list(
      ncdir = "GCS/Crops/US/Switch/",
      ncfile = "us_switch_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "us_soybean_num" = list(
      ncdir = "GCS/Crops/US/Soybean/fractioncover/",
      ncfile = "fsoy_2.7_us.0.5deg.nc",
      variable = "fsoy"
    ),
    "us_corn_num" = list(
      ncdir = "GCS/Crops/US/Corn/fractioncover/",
      ncfile = "fcorn_2.7_us.0.5deg.nc",
      variable = "fcorn"
    ),
    "br_bare_sugc_net_radiation_num" = list(
      ncdir = "GCS/Crops/Brazil/Bare/",
      ncfile = "brazil_bare_sugc_rnet_10yr_avg.nc",
      variable = "rn"
    ),
    "br_bare_sugc_latent_heat_flux_num" = list(
      ncdir = "GCS/Crops/Brazil/Bare/",
      ncfile = "brazil_bare_sugc_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "br_sugc_latent_heat_flux_num" = list(
      ncdir = "GCS/Crops/Brazil/Sugarcane/",
      ncfile = "brazil_sugc_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "braz_fractional_soybean_num" = list(
      ncdir = "GCS/Crops/Brazil/Soybean/",
      ncfile = "brazil_soyb_fractional_10yr_avg.nc",
      variable = "brzsoyrast"
    ),
    "braz_soybean_num" = list( 
      #note there was an error in previous code that pointed this to sugarcane
      ncdir = "GCS/Crops/Brazil/Soybean/",
      ncfile = "brazil_soyb_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "braz_fractional_sugarcane_num" = list(
      ncdir = "GCS/Crops/Brazil/Sugarcane/",
      ncfile = "brazil_sugc_fractional_10yr_avg.nc",
      variable = "brzSGrast"
    ),
    "braz_sugarcane_num" = list(
      ncdir = "GCS/Crops/Brazil/Sugarcane/",
      ncfile = "brazil_sugc_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "global_biome_tundra_num" = list(
      ncdir = "GCS/biomes/",
      ncfile = "Tundra.nc",
      variable = "Tndra"
    ),
    "global_biome_savanna_num" = list(
      ncdir = "GCS/biomes/",
      ncfile = "TropicalSavanna.nc",
      variable = "TrpSvna"
    ),
    "global_biome_peat_num" = list(
      ncdir = "GCS/biomes/",
      ncfile = "TopicalForestAndPeatForest.nc",
      variable = "Trp_PtFrst"
    ),
    "global_biome_temperate_scrub_num" = list(
      ncdir = "GCS/biomes/",
      ncfile = "TemperateScrubAndWoodland.nc",
      variable = "TmpScrb_Wdlnd"
    ),
    "global_biome_temperate_grassland_num" = list(
      ncdir = "GCS/biomes/",
      ncfile = "TemperateGrassland.nc",
      variable = "TmpGrslnd"
    ),
    "global_biome_temperate_forest_num" = list(
      ncdir = "GCS/biomes/",
      ncfile = "TemperateForest.nc",
      variable = "TmprtFrst"
    ),
    "global_biome_boreal_num" = list(
      ncdir = "GCS/biomes/",
      ncfile = "NorthernPeatlandAndBorealForest.nc",
      variable = "NPt_BrlFrst"
    ),
    "global_biome_marsh_num" = list(
      ncdir = "GCS/biomes/",
      ncfile = "MarshAndSwampland.nc",
      variable = "Mrsh_Swmp"
    ),
    "global_biome_desert_num" = list(
      ncdir = "GCS/biomes/",
      ncfile = "Desert.nc",
      variable = "dsrt"
    ),
    "global_pasture_num" = list(
      ncdir = "GCS/",
      ncfile = "Pasture2000_5min.nc",
      variable = "farea"
    ),
    "global_cropland_num" = list(
      ncdir = "GCS/",
      ncfile = "Cropland2000_5min.nc",
      variable = "farea"
    ),
    "global_potVeg_latent_num" = list(
      ncdir = "GCS/PotVeg/PotentialVeg/",
      ncfile = "global_veg_latent_10yr_avg.nc",
      variable = "latent"
    ),
    "global_potVeg_rnet_num" = list(
      ncdir = "GCS/PotVeg/PotentialVeg/",
      ncfile = "global_veg_rnet_10yr_avg.nc",
      variable = "rnet"
    ),
    #Disabled per request in ruby code
    # "us_springwheat_num" = list(
    #   ncdir = "GCS/Crops/US/SpringWheat/fractioncover/",
    #   ncfile = "fswh_2.7_us.0.5deg.nc",
    #   variable = "fswh"
    # ),
    "biome_num" = list(
      ncdir = "",
      ncfile = "vegtype.nc",
      variable = "vegtype"
    )
  )
  
  #iterate through the list of data sources and 
  #load the data for the lat/lon pair.
  res <- lapply(data_list, function(x) { 
    get_ncdf(paste0(netcdf_dir, x$ncdir), x$ncfile, latitude, longitude, x$variable)[[x$variable]][[1]]
  })
  
  #specific calculations based on loaded data
  res$us_switch_latent_heat_flux_diff <- res$us_switch_latent_heat_flux_num - 
    res$global_bare_latent_heat_flux_num
  res$us_corn_latent_heat_flux_diff <- res$us_corn_latent_heat_flux_num  - 
    res$global_bare_latent_heat_flux_num
  res$us_misc_latent_heat_flux_diff <- res$us_misc_latent_heat_flux_num  - 
    res$global_bare_latent_heat_flux_num
  res$us_soy_latent_heat_flux_diff <- res$us_soy_latent_heat_flux_num - 
    res$global_bare_latent_heat_flux_num
  res$br_sugc_latent_heat_flux_diff <- res$br_sugc_latent_heat_flux_num  - 
    res$br_bare_sugc_latent_heat_flux_num
  res$br_sugc_net_radiation_diff <- res$br_sugc_net_radiation_num - 
    res$br_bare_sugc_net_radiation_num 
  res$us_corn_net_radiation_diff <- res$us_corn_net_radiation_num - 
    res$global_bare_net_radiation_num
  res$us_misc_net_radiation_diff <- res$us_misc_net_radiation_num - 
    res$global_bare_net_radiation_num
  res$us_soy_net_radiation_diff <- res$us_soy_net_radiation_num - 
    res$global_bare_net_radiation_num
  res$us_switch_net_radiation_diff <- res$us_switch_net_radiation_num - 
    res$global_bare_net_radiation_num
  
  #Get the named ecosystem data
  name_indexed_ecosystems = fromJSON(file(named_ecosystems))
  
  biome_data <- list(
    "native_eco" = list(),
    "agroecosystem_eco" = list()
  )
  
  ############ Here we set the additional logic threshold levels ############
  if(is.na(res$biome_num) || res$biome_num == "") {
    #do nothing
  }
  else if (res$biome_num <= 15) {
    ## Logic for vegtype ecosystems
    if (res$biome_num == 1) {
      # Per Kristas request
      # "tropical_peat_forest" only where SOC 30-100 cm > 75
      if (res$soc_num > 75) {
        biome_data$native_eco["tropical_peat_forest"] <- name_indexed_ecosystems["tropical peat forest"]
      } 
      biome_data$native_eco["tropical_forest"] <- name_indexed_ecosystems["tropical forest"]
    }
    else if (res$biome_num == 2) {
      biome_data$native_eco["tropical_forest"] <- name_indexed_ecosystems["tropical forest"]
      biome_data$native_eco["tropical_savanna"] <- name_indexed_ecosystems["tropical savanna"]
    }
    else if (3 <= res$biome_num & res$biome_num <= 5) {
      biome_data$native_eco["temperate_forest"] <- name_indexed_ecosystems["temperate forest"]
    }
    else if (6 <= res$biome_num & res$biome_num <= 7) {
      # Per Kristas request
      # "northern_peatland" only where SOC 30-100 cm > 75
      if (soc_num > 75) {
        biome_data$native_eco["northern_peatland"] <- name_indexed_ecosystems["northern peatland"]
      }
      biome_data$native_eco["boreal_forest"] <- name_indexed_ecosystems["boreal forest"]
    }
    else if (res$biome_num == 8) {
      if (latitude >= 50) {
        biome_data$native_eco["boreal_forest"] <- name_indexed_ecosystems["boreal forest"]
      }
      else {
        biome_data$native_eco["temperate_forest"] <- name_indexed_ecosystems["temperate forest"]
      }
    }
    else if (res$biome_num == 9) {
      if (abs(latitude) >= 50) { #TODO Check abs
        biome_data$native_eco["boreal_forest"] <- name_indexed_ecosystems["boreal forest"]
      }
      else if (23.26 < abs(latitude) && abs(latitude) <= 50) {
        biome_data$native_eco["temperate_grassland"] <- name_indexed_ecosystems["temperate grassland"]
        biome_data$native_eco["scrub/woodland"] <- name_indexed_ecosystems["temperate scrub/woodland"]
        biome_data$native_eco["temperate_forest"] <- name_indexed_ecosystems["temperate forest"]
      }
      else if (abs(latitude) <= 23.26) {
        biome_data$native_eco["tropical_savanna"] <- name_indexed_ecosystems["tropical savanna"]
      }
    }
    else if (res$biome_num == 10) {
      biome_data$native_eco["temperate_grassland"] <- name_indexed_ecosystems["temperate grassland"]
    }
    else if (res$biome_num == 11) {
      if (latitude <= 5) {
        biome_data$native_eco["scrub/woodland"] <- name_indexed_ecosystems["temperate scrub/woodland"]
      }
    }
    else if (res$biome_num == 12 | res$biome_num == 14) {
      biome_data$native_eco["desert"] <- name_indexed_ecosystems["desert"]
    }
    else if (res$biome_num == 13 | res$biome_num == 15) {
      biome_data$native_eco["tundra"] <- name_indexed_ecosystems["tundra"]
    }
  }
  
  #### Iterate through native ecosystems to apply logic
  for (eco in names(biome_data$native_eco)) {
    ## SOC Logic
    if (res$soc_num != 0 && !is.na(res$soc_num)) {
      biome_data$native_eco[[eco]]$OM_SOM$s002 <- (res$soc_num * 1.72)  # 0.30 x (soc 0-30 + soc 30-100).
    }
    ## Saatchi Logic
    if (res$saatchi_bgb_num != 0 && !is.na(res$saatchi_bgb_num)) {
      biome_data$native_eco[[eco]]$OM_root$s001 <- res$saatchi_bgb_num
    } 
    if (res$saatchi_agb_num != 0 && !is.na(res$saatchi_agb_num)) {
      biome_data$native_eco[[eco]]$OM_ag$s001 <- res$saatchi_agb_num
    }
    ## Radiative and latent
    if (eco %in% c("temperate_pasture", 
                       "temperate_cropland", 
                       "tropical_pasture", 
                       "tropical_cropland")) {
      biome_data$native_eco[[eco]]$sw_radiative_forcing <- list("s000" = 0)
      biome_data$native_eco[[eco]]$latent <- ("s000" = 0)
    }
    else {
      biome_data$native_eco[[eco]]$sw_radiative_forcing <- list(
        "s000" = (res$global_potVeg_rnet_num - res$global_bare_net_radiation_num) /
          51007200000*1000000000
        )
      biome_data$native_eco[[eco]]$latent <- list(
        "s000" = (res$global_potVeg_latent_num - res$global_bare_latent_heat_flux_num) / 
          51007200000*1000000000
        )
    }
  }
  
  # Will we have a saatchi match without a vegtype?
  # Which ecosystems does the saatchi data get places into? ... 
  # is it just every ecosystem that comes up?
  
  ###   AGROECOSYSTEMS: tropical pasture, temperate pasture, tropical cropland, 
  ###   temperate cropland, wetland rice
  #if (!is.na(res$us_springwheat_num)) {
  #  # disabled per kristas request
  #  biome_data$agroecosystem_eco["springwheat"] = name_indexed_ecosystems["switchgrass"]
  #}
  
  if (!is.na(res$global_pasture_num) & res$global_pasture_num > 0.01 & res$global_pasture_num < 1.0) {
    if (abs(latitude) < 23.26) {
      biome_data$agroecosystem_eco["tropical_pasture"] = name_indexed_ecosystems["tropical pasture"]
    }
    else {
      biome_data$agroecosystem_eco["temperate_pasture"] = name_indexed_ecosystems["temperate pasture"]
    }
  }
  if (!is.na(res$global_cropland_num) & res$global_cropland_num > 0.01 & res$global_cropland_num < 1.0) {
    if (abs(latitude) < 23.26) {
      biome_data$agroecosystem_eco["tropical_cropland"] = name_indexed_ecosystems["tropical cropland"]
    }
    else {
      biome_data$agroecosystem_eco["temperate_cropland"] = name_indexed_ecosystems["temperate cropland"]
    }
  }
  
  ### Custom additions
  custom <- list("s000" = 0, "User defined" = "custom")
  
  if (!is.na(res$us_corn_num) && res$us_corn_num > 0.01) {
    biome_data$agroecosystem_eco["US_corn"] <- name_indexed_ecosystems["US corn"]
    biome_data$agroecosystem_eco[["US_corn"]]$latent <- custom 
    biome_data$agroecosystem_eco[["US_corn"]]$sw_radiative_forcing <- custom 
  }
  if (!is.na(res$us_soybean_num) && res$us_soybean_num > 0.01) {
    biome_data$agroecosystem_eco["soybean"] <- name_indexed_ecosystems["US soy"]
    biome_data$agroecosystem_eco[["soybean"]]$latent <- custom 
    biome_data$agroecosystem_eco[["soybean"]]$sw_radiative_forcing <- custom 
  }
  if (!is.na(res$braz_sugarcane_num) & 
      res$braz_sugarcane_num > 0.01 & 
      res$braz_sugarcane_num < 110.0) {
    biome_data$agroecosystem_eco["BR_sugarcane"] <- name_indexed_ecosystems["BR sugarcane"]
    biome_data$agroecosystem_eco[["BR_sugarcane"]]$latent <- custom 
    biome_data$agroecosystem_eco[["BR_sugarcane"]]$sw_radiative_forcing <- custom 
  }
  if (res$braz_fractional_soybean_num == 1 & 
      !is.na(res$br_sugc_latent_heat_flux_diff)) {
    biome_data$agroecosystem_eco["BR_soy"] <- name_indexed_ecosystems["BR soy"]
    biome_data$agroecosystem_eco[["BR_soy"]]$latent <- custom 
    biome_data$agroecosystem_eco[["BR_soy"]]$sw_radiative_forcing <- custom 
  }
  if (res$braz_fractional_sugarcane_num == 1 & 
      !is.na(res$br_sugc_latent_heat_flux_diff)) {
    biome_data$agroecosystem_eco["BR_sugarcane"] <- name_indexed_ecosystems["BR sugarcane"]
    biome_data$agroecosystem_eco[["BR_sugarcane"]]$latent <- custom 
    biome_data$agroecosystem_eco[["BR_sugarcane"]]$sw_radiative_forcing <- custom 
  }
  if (res$braz_fractional_sugarcane_num == 1 & 
      !is.na(res$br_sugc_latent_heat_flux_diff)) {
    biome_data$agroecosystem_eco["BR_sugarcane"] <- name_indexed_ecosystems["BR sugarcane"]
    biome_data$agroecosystem_eco[["BR_sugarcane"]]$latent <- custom 
    biome_data$agroecosystem_eco[["BR_sugarcane"]]$sw_radiative_forcing <- custom 
  }
  if (!is.na(res$us_misc_latent_heat_flux_diff) == 1) {
    biome_data$agroecosystem_eco["miscanthus"] <- name_indexed_ecosystems["BR sugarcane"]
    biome_data$agroecosystem_eco[["miscanthus"]]$latent <- custom 
    biome_data$agroecosystem_eco[["miscanthus"]]$sw_radiative_forcing <- custom 
    biome_data$biofuel_eco["miscanthus"] <- name_indexed_ecosystems["BR sugarcane"]
    biome_data$biofuel_eco[["miscanthus"]]$latent <- custom 
    biome_data$biofuel_eco[["miscanthus"]]$sw_radiative_forcing <- custom 
  } 
  if (!is.na(res$us_switch_latent_heat_flux_diff) == 1) {
    biome_data$agroecosystem_eco["switchgrass"] <- name_indexed_ecosystems["BR sugarcane"]
    biome_data$agroecosystem_eco[["switchgrass"]]$latent <- custom 
    biome_data$agroecosystem_eco[["switchgrass"]]$sw_radiative_forcing <- custom 
    biome_data$biofuel_eco["switchgrass"] <- name_indexed_ecosystems["BR sugarcane"]
    biome_data$biofuel_eco[["switchgrass"]]$latent <- custom 
    biome_data$biofuel_eco[["switchgrass"]]$sw_radiative_forcing <- custom 
  } 
  
  #write the data to a file if specified
  if (write_data == TRUE) { 
    write_ghgv(toJSON(biome_data), 
               output_dir, 
               output_filename, 
               format = output_format)
  }

  return(biome_data)
}



