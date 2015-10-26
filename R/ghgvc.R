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
#' @importFrom jsonlite fromJSON toJSON
#' 
#' @export
#' 
#' @param config A list of configuration settings and data parameters. 
#' @return List of GHGVC results for each location specified in \code{config}.
#' @author Chris Schauer, David LeBauer
ghgvc <- function(config){
  
  ### 
  #Constants for use in calculation  
  includeANTH <- 1 #include anthro
  A <- 1.78e8 #TODO rename
  ghg_radiative_efficiency <- 10^9 * 
    c(1.4*10^-5, 3.7*10^-4*4/3, 3.03*10^-3)    
  
  names(ghg_radiative_efficiency) <- c("co2", "ch4", "n2o")
  
  #get config information
  options <- config$options
  options <- lapply(config$options, str2LogicalOrNumeric)
  
  ### 
  #Parameters dependent on options values
  includes = c(options$storage, options$flux, options$disturbance) * c(1, -1, -1)
  names(includes) <- c("storage", "flux", "disturbance")
  
  #GHG to include
  ghg_flags <- c(options$co2, options$ch4, options$n2o) #GHG flags to include
  
  #Years of analysis
  num_years_analysis <- options$T_A
  ta <- 1:num_years_analysis
  
  num_years_emissions <- options$T_E 
  te <- 0:num_years_emissions
  
  #Discount rate over analysis years
  annual_discount_rate <- options$r
  w <- 1 / (1 + annual_discount_rate)**(1:num_years_analysis)
  
  #Emissions time for peat
  t_peat <- rep(1,num_years_emissions+1)
  if (num_years_emissions > 50) t_peat[51:num_years_emissions] <- 0
  
  #see p_x function for description of decay kinetics (currently from IPCC 2007)
  decay_p <- calc_time_decay(ta)
  
  #calculate radiative forcing from C pulse for later comparison
  I_Cpulse <- 1000 * 1/44 #1 Mg CO2 pulse at time 0--> units of kmol
  C_Cpulse <- matrix(0, nrow = num_years_analysis, ncol = 1)
  C_Cpulse[1] <- I_Cpulse / A
  C_Cpulse[2:num_years_analysis] <- I_Cpulse / A * p_x[1:num_years_analysis-1]
  cRF_Cpulse <- cumsum((C_Cpulse * w) %*% ghg_radiative_efficiency[1])
  
  ###
  #Sites for analysis 
  sites <- names(config)[-which(names(config) %in% "options")]
  out <- list()
  for (site in sites) {
    site_config <- config[[site]]
    out[[site]] <- list()
    
    # loop through "pft"
    for(ecosystem in which(names(site_config) %in% "pft")){
      ecosystem_data <- lapply(site_config[[ecosystem]], str2LogicalOrNumeric)
      ###Ecosystem data
      termite <- ecosystem_data[['termite']] / 100
      #Biophysical
      sw_radiative_forcing <- ecosystem_data$sw_radiative_forcing
      Qle <- ecosystem_data$latent
      Qh <- ecosystem_data$sensible
      biophysical_net <- Qh + Qle
      radiative_flux_sw <- rep(biophysical_net, num_years_analysis)

      
      #Get parameter matrices
      pool_params <- extract_pool_params(ecosystem_data)
      ghg_params <- extract_ghg_params(ecosystem_data, includeANTH)
      
      #Decay kinetics. 
      #decomposition decay function
      decay_decomp = decay_func(pool_params['decomp',], te)
      decay_decomp['peat',] = pool_params['decomp','peat'] * te
      
      if (pool_params['storage', 'peat'] > 0) {
        decay_decomp['peat',] <- (exp(-pool_params['decomp', 'peat'] * (te - 50)) - 
                                    exp(-pool_params['decomp', 'peat'] * (te - 50 + 1))) *
          (t_peat - 1) * -1
      }
      
      #distrubed decomposition decay function
      decay_disturb_decomp = decay_func(pool_params['disturb_decomp',], te)
      
      #Transition from Aggrading to 'Mature':
      age_transition <- ecosystem_data[['age_transition']]
      
      #Other disturbance parameters
      disturb_rate <- ecosystem_data[['rd']]
      disturb_years <- ecosystem_data[['tR']]
      
      #CALCULATE GHGV
      ###
      #Components of Inputs (Ix), Eq. 4:
      #Sx, Eq. 5:
      storage <- rbind(
        crossprod(pool_params['storage',], pool_params['combust',]) * 
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
                            num_years_emissions - disturb_years+1)) *
        t(ghg_params['FR',] - t(flux))
      
      #storage disturbance
      storage_disturb = t(ghg_params['combust',] %*% t(
        sum(pool_params['disturb_storage',] * 
            pool_params['disturb_combust',], na.rm = TRUE) + 
        cumsum(c(0, 
          colSums(decay_disturb_decomp[,1:num_years_emissions] * 
            pool_params['disturb_storage',] * 
            (1 - pool_params['disturb_combust',]), na.rm=TRUE)))))
      
      disturbance <- disturb_rate * (storage_disturb - flux_disturb)
      disturbance[1,] <- 0
      
      #Input is a 3D array containing year, ghg, and 
      #storage, flux, and disturbance
      input <- array(
        cbind(
          storage * options$storage,
          -flux * options$flux,
          -disturbance * options$disturbance), 
        dim=c(num_years_emissions+1, 3, 3),
        dimnames = c("year", "ghg", "input"))
      
      #clearing <- array(
      #  cbind(
#           
#         ),
#         dim=c(num_years_emissions+1, 3, 3),
#         dimnames = c("year", "ghg", "input"))
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
            temp[k,] <- as.vector(input[k,,]) / A * p_x[(j-k),]
        }
        clearing[j,,] <- colSums(temp)
      }
      
      #Radiative flux and final calculations
      res <- as.vector(apply(clearing, MARGIN=c(2,3), sum) * ghg_radiative_efficiency) / cRF_Cpulse[100]
      names(res) <- c("S_CO2", "S_CH4", "S_N2O", 
                      "F_CO2", "F_CH4", "F_N2O", 
                      "D_CO2", "D_CH4", "D_N2O")
      
      storage_group = sum(res[1:3])
      flux_group = sum(res[4:6])

      # determine the scale between the input and output value for sw radiative forcing
      radiative_forcing_sw <- cumsum(radiative_flux_sw * w)
      swRFV_C <- swRFV / cRF_Cpulse
      swRFV_scale_factor = swRFV_C[num_years_emissions] / sw_radiative_forcing
      
      # scale latent proportional to above
      instance_output_latent = swRFV_scale_factor * latent_cooling
      
      #   CRV= (S_CO2+S_CH4+S_N2O) + (F_CO2+F_CH4+F_N2O)-swRFV+latent
      climate_regulating_value = storage_group + flux_group - swRFV_C[num_years_emissions] + instance_output_latent
      
      listResult <- list(
        name   = ecosystem_data$name,
        S_CO2  = res[1],
        S_CH4  = res[2],
        S_N2O  = res[3],
        F_CO2  = res[4],
        F_CH4  = res[5],
        F_N2O  = res[6],
        D_CO2  = res[7],
        D_CH4  = res[8],
        D_N2O  = res[9],
        swRFV  = swRFV_C[num_years_emissions])
        #latent = instance_output_latent,
        #crv    = climate_regulating_value)
        
      jsonResults <- jsonlite::toJSON(listResult)
      print(jsonResults) 
      original.output <- "[{\"name\":\"Tropical Forest\",\"S_CO2\":715.481930448906,\"S_CH4\":18.5897095138668,\"S_N2O\":11.2808552885403,\"F_CO2\":264.088404311696,\"F_CH4\":4.53053004775435,\"F_N2O\":-43.9477537844218,\"D_CO2\":0,\"D_CH4\":0,\"D_N2O\":0,\"swRFV\":382.964452488271},{\"name\":\"Misty Mountain Top\",\"S_CO2\":183.54445481929,\"S_CH4\":140.39500394662,\"S_N2O\":446.523494486041,\"F_CO2\":264.088404311696,\"F_CH4\":4.53053004775435,\"F_N2O\":-43.9477537844218,\"D_CO2\":0,\"D_CH4\":0,\"D_N2O\":0,\"swRFV\":382.964452488271}]"
      
      
      #Output
      out[[site]][[listResult$name]] <- listResult
    }
  }
  return(out)
}
# 
#       #calculate full radiative forcing & apply weighting
#       radiative_forcing_ghg <- apply(clearing, MARGIN=3, function(x) { 
#         x %*% ghg_radiative_efficiency
#       })
#       radiative_forcing_sw <- radiative_flux_sw * w
#       radiative_forcing <- radiative_forcing_ghg + 
#         matrix(radiative_forcing_sw, nrow = num_years_analysis, ncol = 3)
# 
#       #calculate total GHGV (including all components)
#       GHGV <- apply(radiative_forcing_ghg, MARGIN = 2, cumsum)
#       swRFV <- cumsum(radiative_forcing_sw)
#       cRF_Cpulse <- apply(RF_Cpulse, MARGIN = 2, cumsum)
#         
#       RFV <- GHGV + swRFV
#         
#       GHGV_C <- GHGV / cRF_Cpulse
#       swRFV_C <- swRFV / cRF_Cpulse
#       RFV_C <- RFV / cRF_Cpulse
#       
#       GHGV_matrix[1:num_years_analysis,i] <- GHGV
#       GHGV_C_matrix[1:num_years_analysis,i] <- GHGV_C
#       
#       swRFV_matrix[1:num_years_analysis,i] <- swRFV
#       swRFV_C_matrix[1:num_years_analysis,i] <- swRFV_C
#       
#       RFV_matrix[1:num_years_analysis,i] <- RFV
#       RFV_C_matrix[1:num_years_analysis,i] <- RFV_C
#       
#       #separate reporting for each GHG
#       radiative_flux_w <- NaN * radiative_flux
#       GHGVx <- NaN * radiative_flux
#       GHGVx_C <- NaN * radiative_flux
#       
#       for (kk in 1:3) {
#         radiative_flux_w[1:num_year_analysis,kk] <- (radiative_flux[,kk] * t(w))
#         for (j in 1:num_year_analysis) {
#           GHGVx[j,kk] <- sum(radiative_flux_w[1:j,kk])
#         }
#         GHGVx_C[,kk] <- t((GHGVx[,kk] / cRF_Cpulse))
#       }
#       if (nm == 1) {
#         GHGmatrix[13:15, i] <- GHGVx_C[num_year_analysis,]
#         GHGmatrix[16, i] <- sum(GHGmatrix[13:15,i])
#       }
#       else if (nm == 2) {
#         GHGmatrix[1:3, i] <- GHGVx_C[num_year_analysis,]
#         GHGmatrix[4, i] <- sum(GHGmatrix[1:3,i])
#       }
#       else if (nm == 3) {
#         GHGmatrix[5:7, i] <- GHGVx_C[num_year_analysis,]
#         GHGmatrix[8, i] <- sum(GHGmatrix[5:7,i])
#       }
#       else if (nm == 4) {
#         GHGmatrix[9:11, i] <- GHGVx_C[num_year_analysis,]
#         GHGmatrix[12, i] <- sum(GHGmatrix[9:11,i])
#       }
#     }
#     
# 
#     
# 
#             
#       
#       
#             
      
#       clearing <- rbind(
#         input / A *
#         input[num_years_emissions+1,] 
#         matrix(rep(apply(p_x, MARGIN = 2, cumsum), 3), ncol=9)
#         input * (1 + cumsum(p_x))
#         
#         c1 = input[1,] / A
#         c2,1 = input[1,] / A * p_x[1]
#         c2,2 = input[2,] / A
#         c3,1 = input[1,] / A * p_x[2]
#         c3,2 = input[2,] / A * p_x[1]
#         c3,3 = input[3,] / A
#         c4,1 = input[1,] / A * p_x[3]
#         c4,2 = input[2,] / A * p_x[2]
#         c4,3 = input[3,] / A * p_x[1]
#         c4,4 = input[4,] / A
#         
#         A * input[1:4] * rbind(p_x[1:3,], rep(1,3))
#         A * cumsum(input * )
#         
#         
#         ci = input[i] + sum(input[1:(i-1),] * p)
#         
#         
#         
#         c21 = input[1,] / A * p_x[1,]
#         c22 = input[2,] / A
#         c31
#         c32
#         c33
#         c52 = input[1] / A + sum(input[2:,] / A * cumsum(p_x))
#         for j in 1:100
#           for k in c(1:51, rep(51,49))
#             
        
        
        
        
        
        
        
      





