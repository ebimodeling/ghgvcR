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
  ghg_radiative_efficiency <- 10^9 * data.frame(
    co2 = c(1.4*10^-5),     
    ch4 = c(3.7*10^-4*4/3),  
    n2o = c(3.03*10^-3))    
  #get config information
  options <- config$options
  options <- lapply(config$options, str2LogicalOrNumeric)
  
  ### 
  #Parameters dependent on options values
  includes = c(options$storage, options$flux, options$disturbance) * c(1, -1, -1)
  names(includes) <- c("storage", "flux", "disturbance")
  
  num_years_analysis <- options$T_A
  ta <- 1:num_years_analysis
  
  num_years_emissions <- options$T_E 
  te <- 0:num_years_emissions
  
  #Emissions time for peat
  t_peat <- rep(1,num_years_emissions+1)
  if (num_years_emissions > 50) t_peat[51:num_years_emissions] <- 0
  
  annual_discount_rate <- options$r
  ghg_flags <- c(options$co2, options$ch4, options$n2o) #GHG flags to include
  
  ###
  #Sites for analysis 
  sites <- names(config)[-which(names(config) %in% "options")]
  out <- list()
  for (site in sites) {
    site_config <- config[[site]]
    out[site] <- list()
    
    # loop through "pft"
    for(ecosystem in which(names(site.config) %in% "pft")){
      ecosystem_data <- lapply(site_config[[ecosystem]], str2LogicalOrNumeric)
      ###Ecosystem data
      termite <- ecosystem_data[['termite']] / 100
      #Biophysical
      sw_radiative_forcing <- ecosystem_data$sw_radiative_forcing
      Qle <- ecosystem_data$latent
      Qh <- ecosystem_data$sensible
      biophysical_net <- Qh + Qle
      
      #Get parameter matrices
      pool_params <- extract_pool_params(ecosystem_data)
      ghg_params <- extract_ghg_params(ecosystem_data)
      
      #Decay kinetics. 
      #see p_x function for description of decay kinetics (currently from IPCC 2007)
      decay_p <- calc_time_decay(ta)
      
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
        t((pool_params['storage',] * (1 - pool_params['combust',])) %*% 
            decay_decomp) %*% ghg_params['combust',]
      )
      
      #Flux, Fx
      flux <- rbind(0,
                    matrix(ghg_params['flux',], 
                           nrow = num_years_emissions+1, 
                           ncol = 3, 
                           byrow=TRUE))
      if (age_transition > 0 & age_transition < num_years_emissions - 1) {
        flux[(age_transition + 2):(num_years_emissions + 1),] <- ghg_params['new_flux']
      }
      
      #Disturbance, Dx, Eq. 6:
      #First the flux disturbance
      flux_disturb <- c(1:(disturb_years), 
                        rep(disturb_years, 
                            num_years_emissions - disturb_years+2)) *
        t(ghg_params['FR',] - t(flux))
      
      #storage disturbance
      storage_disturb = TODO
      disturbance <- disturb_rate * (storage_disturb + flux_disturb)
      disturbance[1,] <- 0
      
      for (nm in 1:4) {
        #calculate I_x (eq. 4)
        if (nm == 1)
          input <- storage * options$storage - 
            flux * options$flux -
            disturbance * options$disturbance
        else if (nm == 2)
          input <- storage * options$storage
        else if (nm == 3)
          input <- -flux * options$flux
        else if (nm == 4)
          input <- -disturbance * options$flux
        
        
        for (g in which(ghg_flags == TRUE)) {
          #calculate D_x
          
          tempA <- array(0, dim = c(3, num_years_emissions+1, num_years_emissions+1))
          tempA <- array(apply(fdecayD, 1, function(x) { c(x,x,x) }), dim=c(num_years_emissions+1,num_years_emissions+1,3))
          (sum(S_pD * (1-fcD)) * Ec) * fdecayD
          som_disturbance = rowSums(tempA)
          
          for (j in 1:num_years_emissions+1) { #col
            #1:23-23 * 
            flux_disturbance[j,] <- min(j,disturb_years) * (FR - t(flux))
            for (jk in 1:j) { 
              if (jk == j)
                tempA[jk] <- sum(S_pD * fcD) * Ec
              else if (jk < j)
                tempA[jk] <- t((S_pD * (1-fcD)) * Ec) %*% fdecayD[,jk]
            }
            som_disturbance[j,] <- sum(tempA)
          }
          
          
          w = matrix(0, num_year_analysis, 1)
          temp = matrix(0, 1, num_year_analysis)
          
          for (j in 1:num_year_analysis) {
            for (k in 1:min(num_years_emissions+1, th[j])) {
              if (j - k == 0)
                temp[k] <- input[k,] / A
              else
                temp[k] <- input[k,] / A * p_x[(j-k),]
            }
            clearing[j,] <- sum(temp)
            w[j] <- 1 / (1 + annual_discount_rate)^j
          }
          
          radiative_flux <- ghg_radiative_efficiency * clearing
        }
        
        RFsw[1:num_years_emissions] <- biophysical_net
        
        #calculate full radiative forcing & apply weighting
        radiative_forcing_GHG <- rowSums(radiative_flux) * w
        radiative_forcing_SW <- rowSums(RFsw) * w
        radiative_forcing <- RF_GHG + RF_SW
        
        #calculate radiative frocing from C pulse for comparison
        I_Cpulse <- 1000 * 1/44 #1 Mg CO2 pulse at time 0--> units of kmol
        C_Cpulse <- NaN*clearing[,1]
        C_Cpulse[1] <- I_Cpulse / A
        C_Cpulse[2:num_year_analysis] <- I_Cpulse / A * p_x[1:num_year_analysis-1]
        RF_Cpulse <- (C_Cpulse * ghg_radiative_efficiency[1]) * w
        
        #calculate GHGV--for inclusion of all components (nm=1)
        if (nm == 1) {
          GHGV <- NaN * RF_GHG
          swRFV <- NaN * RF_SW
          cRF_Cpulse <- NaN * RF_Cpulse
          for (j in 1:num_year_analysis) {
            GHGV[j] <- sum(RF_GHG[1:j,])
            swRFV[j] <- sum(RF_SW[1:j,])
            cRF_Cpulse[j] <- sum(RF_Cpulse[1:j])
          }
          
          RFV <- GHGV + swRFV
          
          GHGV_C <- GHGV / cRF_Cpulse
          swRFV_C <- swRFV / cRF_Cpulse
          RFV_C <- RFV / cRF_Cpulse
          
          GHGV_matrix[1:num_year_analysis,i] <- GHGV
          GHGV_C_matrix[1:num_year_analysis,i] <- GHGV_C
          
          swRFV_matrix[1:num_year_analysis,i] <- swRFV
          swRFV_C_matrix[1:num_year_analysis,i] <- swRFV_C
          
          RFV_matrix[1:num_year_analysis,i] <- RFV
          RFV_C_matrix[1:num_year_analysis,i] <- RFV_C
        }
        #separate reporting for each GHG
        radiative_flux_w <- NaN * radiative_flux
        GHGVx <- NaN * radiative_flux
        GHGVx_C <- NaN * radiative_flux
        
        for (kk in 1:3) {
          radiative_flux_w[1:num_year_analysis,kk] <- (radiative_flux[,kk] * t(w))
          for (j in 1:num_year_analysis) {
            GHGVx[j,kk] <- sum(radiative_flux_w[1:j,kk])
          }
          GHGVx_C[,kk] <- t((GHGVx[,kk] / cRF_Cpulse))
        }
        if (nm == 1) {
          GHGmatrix[13:15, i] <- GHGVx_C[num_year_analysis,]
          GHGmatrix[16, i] <- sum(GHGmatrix[13:15,i])
        }
        else if (nm == 2) {
          GHGmatrix[1:3, i] <- GHGVx_C[num_year_analysis,]
          GHGmatrix[4, i] <- sum(GHGmatrix[1:3,i])
        }
        else if (nm == 3) {
          GHGmatrix[5:7, i] <- GHGVx_C[num_year_analysis,]
          GHGmatrix[8, i] <- sum(GHGmatrix[5:7,i])
        }
        else if (nm == 4) {
          GHGmatrix[9:11, i] <- GHGVx_C[num_year_analysis,]
          GHGmatrix[12, i] <- sum(GHGmatrix[9:11,i])
        }
      }
      
      # determine the scale between the input and output value for sw radiative forcing
      swRFV_scale_factor = swRFV_C_matrix[num_years_emissions,i] / sw_radiative_forcing
      # scale latent proportional to above
      instance_output_latent = swRFV_scale_factor * latent_cooling
      
      
      storage_group = ( GHGmatrix[1,i] + GHGmatrix[2,i] + GHGmatrix[3,i] )
      flux_group = ( GHGmatrix[5,i] + GHGmatrix[6,i] + GHGmatrix[7,i] )
      
      #   CRV= (S_CO2+S_CH4+S_N2O) + (F_CO2+F_CH4+F_N2O)-swRFV+latent
      climate_regulating_value = storage_group + flux_group - swRFV_C_matrix[num_years_emissions,i] + instance_output_latent
      
      listResult <- list( name = ecosystem[['name']], 
                          S_CO2 = GHGmatrix[1,i],
                          S_CH4 = GHGmatrix[2,i],
                          S_N2O = GHGmatrix[3,i],
                          F_CO2 = GHGmatrix[5,i],
                          F_CH4 = GHGmatrix[6,i],
                          F_N2O = GHGmatrix[7,i],
                          D_CO2 = (GHGmatrix[1,i] + GHGmatrix[5,i]),
                          D_CH4 = (GHGmatrix[2,i] + GHGmatrix[6,i]),
                          D_N2O = (GHGmatrix[3,i] + GHGmatrix[7,i]),
                          swRFV =  swRFV_C_matrix[num_years_emissions,i], 
                          latent = instance_output_latent, 
                          crv = climate_regulating_value )
      
      jsonResults <- jsonlite::toJSON(listResult)
      
      #Output
      out[site][ecosystem_data$name] <- fromJSON(jsonResults)[[1]]
    }
  }    
}





