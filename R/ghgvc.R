##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois and Authors.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## 
## Citation: Kristina J. Teinputeira and Evan H. Delucia 2011. The greenhouse gas value of ecosystems. Global Change Biology. 17(1):425–438 doi: 10.1111/j.1365-2486.2010.02220.x
##-------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------#
#' Greenhouse Gas Value Calculator
#'
#' R function implementation of GHGV_calculator_web.m
#' @title GHGVC
#' 
#' @importFrom jsonlite toJSON base64_enc
#' @export
#' 
#' @param eco_json (json string) A JSON list of configuration settings and data parameters. 
#' @param output_filename (character) name of file to write.
#' @param output_format (character) format of output file, either json or csv.
#' @param write_output (logical) indicating whether or not to write the results.
#' @param plot_filename (character) name of file to write.
#' @param plot_format (character) format of output file, either json or csv.
#' @param write_plot (logical) indicating whether or not to write the results.
#'   the results.
#' @return List of GHGVC results for each location specified in \code{eco_json}.
#' @author Chris Schauer, David LeBauer, Nicholas Potter
calc_ghgv <- function(eco_json,
                      output_filename = "ghgv.csv",
                      save_output = FALSE,
                      plot_filename = "ghgv_plot.png",
                      plot_units = c("co2", "mi"),
                      save_plots = FALSE
                      ) {
  
  #set options from parameters
  plot_units <- match.arg(plot_units, several.ok = TRUE)
  
  #convert from json to list
  eco_params <- fromJSON(eco_json)
   
  #get config information
  options <- eco_params$options
  options <- lapply(eco_params$options, str2LogicalOrNumeric)
  
  ### 
  #Constants for use in calculation
  include_anth <- if(is.null(options$includeANTH)) 1 else options$includeANTH 
  include_bio <- if(is.null(options$includeBIO)) 1 else options$includeBIO 
  num_years_analysis <- options$T_A
  num_years_emissions <- options$T_E 
  annual_discount_rate <- options$r
  ta <- 1:num_years_analysis
  te <- 0:num_years_emissions
  w <- 1 / (1 + annual_discount_rate)**(1:num_years_analysis)
  
  ghg_radiative_efficiency <- 10^9 * 
    c(1.4*10^-5, 3.7*10^-4*4/3, 3.03*10^-3)
  
  names(ghg_radiative_efficiency) <- c("co2", "ch4", "n2o")
  A <- 1.78e8 #TODO rename

  ### 
  #Parameters dependent on options values
  includes = c(options$storage, options$flux, options$disturbance) * c(1, -1, -1)
  names(includes) <- c("storage", "flux", "disturbance")
  
  #GHG to include
  ghg_flags <- c(options$co2, options$ch4, options$n2o) #GHG flags to include
  
  #Discount rate over analysis years
  
  #Emissions time for peat
  t_peat <- rep(1,num_years_emissions+1)
  if (num_years_emissions > 50) t_peat[51:num_years_emissions] <- 0
  
  #see p_x function for description of decay kinetics (currently from IPCC 2007)
  decay_p <- kinetic_decay(ta)
  
  #calculate radiative forcing from C pulse for later comparison
  I_Cpulse <- 1000 * 1/44 #1 Mg CO2 pulse at time 0--> units of kmol
  C_Cpulse <- matrix(0, nrow = num_years_analysis, ncol = 1)
  C_Cpulse[1] <- I_Cpulse / A
  C_Cpulse[2:num_years_analysis] <- I_Cpulse / A * decay_p[1:num_years_analysis-1]
  cRF_Cpulse <- cumsum((C_Cpulse * w) %*% ghg_radiative_efficiency[1])
  
  ###
  #Sites for analysis 
  sites <- names(eco_params)[-which(names(eco_params) %in% "options")]
  
  out <- list()
  
  for (site in sites) {
    site_params <- eco_params[[site]]
    
    #site lat/lon for later
    site_lat <- as.numeric(site_params$lat[[1]])
    site_lng <- as.numeric(site_params$lng[[1]])
    
    if("pft" %in% names(site_params)) { 
      #fix issue of blank site (user selects a site but doesn't click a biome)
      #this way the calculator doesn't try to include results for an empty site.
      out[[site]] <- list()
    }
    
    # loop through "pft"
    for(ecosystem in which(names(site_params) %in% "pft")){
      ecosystem_data <- lapply(site_params[[ecosystem]], str2LogicalOrNumeric)
      
      #There should be no "MAP" values, but replace with 0 if so.
      ecosystem_data[ecosystem_data == "MAP"] <- 0
      
      ###Ecosystem data
      termite <- ecosystem_data[['termite']] / 100
      
      #Biophysical
      sw_radiative_forcing <- ecosystem_data$sw_radiative_forcing
      latent_cooling <- ecosystem_data$latent
      biophysical_net <- ecosystem_data$biophysical_net
      
      sw_radiative_flux <- rep(biophysical_net, num_years_analysis)
      
      #Get parameter matrices
      pool_params <- extract_pool_params(ecosystem_data)
      ghg_params <- extract_ghg_params(ecosystem_data, include_anth)
      
      #Decay kinetics. 
      #decomposition decay function
      decay_decomp = decay(pool_params['decomp',], te)
      decay_decomp['peat',] = pool_params['decomp','peat'] * te
      
      if (pool_params['storage', 'peat'] > 0) {
        decay_decomp['peat',] <- (exp(-pool_params['decomp', 'peat'] * (te - 50)) - 
                                    exp(-pool_params['decomp', 'peat'] * (te - 50 + 1))) *
          (t_peat - 1) * -1
      }
      
      #distrubed decomposition decay function
      decay_disturb_decomp = decay(pool_params['disturb_decomp',], te)
      
      #Transition from Aggrading to 'Mature':
      age_transition <- ecosystem_data[['age_transition']]
      
      #Other disturbance parameters
      disturb_rate <- ecosystem_data[['rd']]
      disturb_years <- min(num_years_emissions, max(ecosystem_data[['tR']], 0))
      
      #CALCULATE GHGV
      ###
      #Components of Inputs (Ix), Eq. 4:
      #Sx, Eq. 5:
      storage <- rbind(
        as.vector(crossprod(pool_params['storage',], pool_params['combust',])) * 
          ghg_params['combust',],
        t(t(pool_params['storage',] * (1 - pool_params['combust',]) * 
             ghg_params[5:8,]) %*% decay_decomp[,1:num_years_emissions])
      )
      
      #Flux, Fx
      flux <- rbind(0,
                    matrix(ghg_params['flux',], 
                           nrow = num_years_emissions, 
                           ncol = 3, 
                           byrow=TRUE))
      if (age_transition > 0 & age_transition < num_years_emissions - 1) {
        flux[(age_transition + 2):(num_years_emissions + 1),] <- ghg_params['new_flux']
      }
      
      #Disturbance, Dx, Eq. 6:
      #First the flux disturbance
      flux_disturb <- c(1:disturb_years, 
                        rep(disturb_years, 
                            num_years_emissions - 1)) *
        t(ghg_params['FR',] - t(flux))
      
      #storage disturbance
      storage_disturb = t(ghg_params['combust',] %*% t(
        sum(pool_params['disturb_storage',] * 
            pool_params['disturb_combust',], na.rm = TRUE) + 
        cumsum(c(0, 
          colSums(decay_disturb_decomp[,1:num_years_emissions] * 
            pool_params['disturb_storage',] * 
            (1 - pool_params['disturb_combust',]), na.rm=TRUE)))))
      
      disturbance <- disturb_rate * (storage_disturb + flux_disturb)
      disturbance[1,] <- 0
      
      #Input is a 3D array containing year, ghg, and 
      #storage, flux, and disturbance
      #cat(length(storage * options$storage))
      #cat(length(-flux * options$flux))
      #cat(length(-disturbance * options$disturbance))
      input <- array(
        cbind(
          storage * options$storage,
          -flux * options$flux,
          -disturbance * options$disturbance), 
        dim=c(num_years_emissions+1, 3, 3))
      #,
      #  dimnames = list("year", "ghg", "input"))
      #clearing <- array(
      #  cbind(
#           
#         ),
#         dim=c(num_years_emissions+1, 3, 3),
#         dimnames = list("year", "ghg", "input"))
#         
#       )
      #TODO: turn this into apply statements
      temp <- matrix(0, nrow = num_years_analysis, ncol = 9)
      clearing <- array(dim=c(num_years_analysis, 3, 3))
      for (j in 1:num_years_analysis) {
        for (k in 1:min(num_years_emissions+1, j)) {
          if (j - k == 0)
            temp[k,] <- as.vector(input[k,,]) / A
          else
            temp[k,] <- as.vector(input[k,,]) / A * decay_p[(j-k),]
        }
        clearing[j,,] <- colSums(temp)
      }
      
      #Radiative flux and final calculations
      res <- as.vector(apply(clearing, MARGIN=c(2,3), sum) * ghg_radiative_efficiency) / cRF_Cpulse[num_years_emissions]
      out_names <- c("S_CO2", "S_CH4", "S_N2O", 
                      "F_CO2", "F_CH4", "F_N2O", 
                      "D_CO2", "D_CH4", "D_N2O")
      names(res) <- out_names
     
      #Why these aren't calculated above we don't know...
      res[7] <- res["S_CO2"] + res["F_CO2"]
      res[8] <- res["S_CH4"] + res["F_CH4"]
      res[9] <- res["S_N2O"] + res["F_N2O"]
       
      storage_group = sum(res[1:3])
      flux_group = sum(res[4:6])

      # determine the scale between the input and output value for sw radiative forcing
      swRFV <- cumsum(sw_radiative_flux * w)
      swRFV_C <- swRFV / cRF_Cpulse
      swRFV_scale_factor = swRFV_C[num_years_emissions] / sw_radiative_forcing
      
      # scale latent proportional to above
      instance_output_latent = swRFV_scale_factor * latent_cooling
      
      #   CRV= (S_CO2+S_CH4+S_N2O) + (F_CO2+F_CH4+F_N2O)-swRFV+latent
      climate_regulating_value = storage_group + 
        flux_group - 
        swRFV_C[num_years_emissions] + 
        instance_output_latent
      
      listResult <- list(
        name   = ecosystem_data$name,
        lat    = site_lat,
        lng    = site_lng,
        S_CO2  = res[1],
        S_CH4  = res[2],
        S_N2O  = res[3],
        F_CO2  = res[4],
        F_CH4  = res[5],
        F_N2O  = res[6],
        D_CO2  = res[7],
        D_CH4  = res[8],
        D_N2O  = res[9],
        swRFV  = swRFV_C[num_years_emissions],
        latent = instance_output_latent,
        crv    = climate_regulating_value)
      
      #Replace names with units
      units <- paste("[Mg CO2-eq ha-1", num_years_analysis, "yrs-1]")
      unit_names <- c("S_CO2", "S_CH4", "S_N2O", 
                      "F_CO2", "F_CH4", "F_N2O",
                      "D_CO2", "D_CH4", "D_N2O",
                      "swRFV", "latent", "crv")
      names(listResult) <- c("name", "lat", "lng", 
                             lapply(unit_names, function(x) { paste(x, units) }))
        
      #Output
      out[[site]][[listResult$name]] <- listResult
    }
  }
  
  #get the outputs
  out_json <- toJSON(out) 
  #write the data to a file if specified
  if(save_output == TRUE) {
    x <- sapply(output_formats, 
           function(x){
             write_output(out_json, paste0(output_filename, ".", x))
           })
  }
 
  #create the plots
  plots <- list() 
  for(units in plot_units) {
    plt <- plot_ghgv(json2DF(out_json), years = num_years_analysis, units = units)
    if(save_plots == TRUE) {
      sapply(plot_formats, 
             function(x){
               write_plot(plt, paste0(plot_filename, ".", x))
             })
    }
    tmpfile <- tempfile(fileext = ".svg")
    write_plot(plt, tmpfile)
    plots[units] <- base64_enc(readBin(tmpfile, 
                                       what = "raw"))
    #, 
    #                                   size = file.info(tmpfile)[1, "size"]))
  }

  res <- list(
    results = out_json, 
    plots = plots
  )
  return(toJSON(res))
}
