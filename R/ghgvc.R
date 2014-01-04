#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois and Authors.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
# 
# Citation: Kristina J. Teixeira and Evan H. Delucia 2011. The greenhouse gas value of ecosystems. Global Change Biology. 17(1):425–438 doi: 10.1111/j.1365-2486.2010.02220.x
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Greenhouse Gas Value Calculator
##'
##' R function implementation of GHGV_calculator_web.m
##' @title GHGVC
##' @param options 
##' @param ecosystem_data 
##' @export
##' @return GHGVC result
##' @author Chris Schauer, David LeBauer
ghgvc <- function(options, ecosystem_data){
  options <- lapply(options, get.nums)
  
  includeANTH <- 1

  includeS <- as.logical(options[['storage']])
  includeF <- as.logical(options[['flux']])
  includeD <- as.logical(options[['disturbance']])

  if (as.logical(options[['co2']]) && as.logical(options[['ch4']]) && as.logical(options[['n2o']])) {
    gf <- 1
    gl <- 3
  } else {
    if (as.logical(options[['co2']]))
      gf <- 1
    else if (as.logical(options[['ch4']]))
      gf <- 2
    else
      gf <- 3

    gl <- gf
  }

  A <- 1.78e8
  ax <- c(1.4*10^-5*10^9, 3.7*10^-4*10^9*4/3, 3.03*10^-3*10^9) #radiative efficiency of GHGs (nW/m2/ppb; IPCC 2007)
                                        #see p_x function for description of decay kinetics (currently from IPCC 2007)

  nruns <- length(grep("pft", names(ecosystem_data)))
  p_matrix = matrix(0, 52, nruns)
  s_names <- vector('list', nruns)

  T_A <- as.numeric(options[['T_A']])
  T_E <- as.numeric(options[['T_E']])
  annual_discount_rate <- as.numeric(options[['r']])

  GHGV_matrix = NaN * matrix(1, T_A, nruns)
  GHGV_C_matrix = NaN * matrix(1, T_A, nruns)
  swRFV_matrix = NaN * matrix(1, T_A, nruns)
  swRFV_C_matrix = NaN * matrix(1, T_A, nruns)
  RFV_matrix = NaN * matrix(1, T_A, nruns)
  RFV_C_matrix = NaN * matrix(1, T_A, nruns)
  GHGmatrix = NaN * matrix(1, 16, nruns)
  jsonResults = matrix("", 1, nruns)

  for (i in 1:nruns) {
    ecosystem <- lapply(ecosystem_data[[i]], get.nums)

    t_E <- seq(0, T_E, len=T_E+1)
    t_A <- seq(1, T_A, len=T_A)

    #Biophysical
    sw_radiative_forcing <- ecosystem[['sw_radiative_forcing']]
    latent_cooling <- ecosystem[['latent']]
    sensible_heat <- ecosystem[['sensible']]
    biophysical_net <- latent_cooling + sensible_heat

                                        #STORAGE
    aboveground_storage <- ecosystem[['OM_ag']]
    root_storage <- ecosystem[['OM_root']]
    wood_storage <- ecosystem[['OM_wood']]
    litter_storage <- ecosystem[['OM_litter']]
    peat_storage <- ecosystem[['OM_peat']]
    S_SOM <- ecosystem[['OM_SOM']]
    
    S_p <- c(aboveground_storage + wood_storage + litter_storage, root_storage, peat_storage, S_SOM)

                                        #BURN CHARACTERISTICS
    fc <- c(ecosystem[['fc_ag_wood_litter']], ecosystem[['fc_root']], ecosystem[['fc_peat']], ecosystem[['fc_SOM']])

                                        #print(fc)

    Ec <- c(ecosystem[['Ec_CO2']], ecosystem[['Ec_CH4']], ecosystem[['Ec_N2O']]) #proportional release of GHG

                                        #DECOMPOSITION
    k <- c(ecosystem[['k_ag_wood_litter']], ecosystem[['k_root']], ecosystem[['k_peat']], ecosystem[['k_SOM']])

    t_peat <- 0*t_E+1
    if (T_E > 50) {
      t_peat[51:T_E] <- 0
    }

    fdecay <- matrix(c(exp(-k[1]*t_E)-exp(-k[1]*(t_E+1)), exp(-k[2]*t_E)-exp(-k[2]*(t_E+1)), k[3]*t_peat, exp(-k[4]*t_E)-exp(-k[4]*(t_E+1))), nrow=4, byrow=TRUE)
    if (peat_storage > 0) {
      fdecay[4,] <- (exp(-k[4]*(t_E-50))-exp(-k[4]*(t_E-50+1)))*(t_peat-1)*-1
                                        #print(fdecay[4,])
    }

    termite <- ecosystem[['termite']] / 100

    Ed_CO2 <- c(ecosystem[['Ed_CO2_ag_wood_litter']], ecosystem[['Ed_CO2_root']], ecosystem[['Ed_CO2_peat']], ecosystem[['Ed_CO2_litter']])
    Ed_CH4 <- c(ecosystem[['Ed_CH4_ag_wood_litter']], ecosystem[['Ed_CH4_root']], ecosystem[['Ed_CH4_peat']], ecosystem[['Ed_CH4_litter']])
    Ed_N2O <- c(ecosystem[['Ed_N2O_ag_wood_litter']], ecosystem[['Ed_N2O_root']], ecosystem[['Ed_N2O_peat']], ecosystem[['Ed_N2O_litter']])
    
    Ed <- matrix(c(Ed_CO2, Ed_CH4, Ed_N2O), nrow=4)

                                        #print(Ed)

                                        #FLUX

    if (includeANTH == 0)
      F <- c(ecosystem[['F_CO2']] + ecosystem[['F_anth']], ecosystem[['F_CH4']], ecosystem[['F_N2O']])
    else if (includeANTH == 1)
      F <- c(ecosystem[['F_CO2']] + ecosystem[['F_anth']], ecosystem[['F_CH4']], ecosystem[['F_N2O']])

                                        #DISTURBANCE
    rD <- ecosystem[['rd']]
    tR <- ecosystem[['tR']]
    FR <- c(ecosystem[['FR_CO2']], ecosystem[['FR_CH4']], ecosystem[['FR_N2O']])

                                        #Disturbance biomass pools:
    S_pD <- c(aboveground_storage + wood_storage + litter_storage, root_storage, peat_storage)

    fcD <- c(ecosystem[['dfc_ag_wood_litter']], ecosystem[['dfc_root']], ecosystem[['dfc_peat']])

    kD <- c(ecosystem[['dk_ag_wood_litter']], ecosystem[['dk_root']], ecosystem[['dk_peat']])

    fdecayD <- matrix(c(exp(-kD[1]*t_E) - exp(-kD[1]*(t_E+1)), exp(-kD[2]*t_E) - exp(-kD[2]*(t_E+1)), exp(-kD[3]*t_E) - exp(-kD[3]*(t_E+1))),nrow=3,byrow=TRUE)

                                        #Transition from Aggrading to 'Mature':
    age_transition <- ecosystem[['age_transition']]
    F_new <- c(ecosystem[['new_F_CO2']], ecosystem[['new_F_CH4']], ecosystem[['new_F_N2O']])

                                        #CALCULATE GHGV
    Sx <- NaN * matrix(1, T_E + 1, 3)
    Fx <- NaN * matrix(1, T_E + 1, 3)
    Dx <- NaN * matrix(1, T_E + 1, 3)
    SDx <- NaN * matrix(1, T_E + 1, 3)
    FDx <- NaN * matrix(1, T_E + 1, 3)
    Ix <- NaN * matrix(1, T_E + 1, 3)
    Cx <- NaN * matrix(1, T_A, 3)
    RFx <- matrix(0, T_A, 3)
    RFsw <- NaN * matrix(1, T_A, 1)

    p_x = t(matrix(c(.217+.259*exp(-t_A / 172.9)+.338*exp(-t_A / 18.51)+.186*exp(-t_A / 1.186), exp(-t_A / 12), exp(-t_A / 114)),nrow=3,byrow=TRUE))

    for (nm in 1:4) {
      for (g in gf:gl) {

                                        #calculate S_x (eq. 5)
	Sx[1,g] <- sum(S_p * fc * Ec[g])
	Sx[2:(T_E+1),g] <- t((S_p * (1 - fc)) * Ed[,g]) %*% fdecay[,1:T_E]

                                        #calculate F_x
	Fx[,g] <- F[g]
	Fx[1,g] <- 0
	if (age_transition + 1 < T_E && age_transition > 0) {
	  Fx[(age_transition + 2):(T_E + 1),g] <- F_new[g]
	}

                                        #calculate D_x
	tempA <- matrix(0, 1, T_E+1)
	for (j in 1:T_E+1) {
	  FDx[j,g] <- min(j,tR) * (FR[g] - Fx[j,g])
	  for (jk in 1:j) {
	    if (jk == j)
              tempA[jk] <- sum(S_pD * fcD * Ec[g])
	    else if (jk < j)
              tempA[jk] <- t((S_pD * (1-fcD)) * Ec[g]) %*% fdecayD[,jk]
	  }

	  SDx[j,g] <- sum(tempA)
	}

	Dx[,g] <- rD * (SDx[,g] + FDx[,g])
	Dx[1,g] <- 0

                                        #calculate I_x (eq. 4)
	if (nm == 1)
	  Ix[1:(T_E+1),g] <- Sx[,g] * includeS-Fx[,g] * includeF - Dx[,g] * includeD
	else if (nm == 2)
	  Ix[1:(T_E+1),g] <- Sx[,g] * includeS
	else if (nm == 3)
	  Ix[1:(T_E+1),g] <- -Fx[,g] * includeF
 	else if (nm == 4)
	  Ix[1:(T_E+1),g] <- -Dx[,g] * includeD

	w = matrix(0, T_A, 1)
	temp = matrix(0, 1, T_A)

	for (j in 1:T_A) {
	  for (k in 1:min(T_E+1, t_A[j])) {
	    if (j - k == 0)
              temp[k] <- Ix[k,g] / A
	    else
              temp[k] <- Ix[k,g] / A * p_x[(j-k),g]
	  }
	  Cx[j,g] <- sum(temp)
	  w[j] <- 1 / (1 + annual_discount_rate)^j
	}

	RFx[,g] <- ax[g] * Cx[,g]
      }

      RFsw[1:T_E] <- biophysical_net

                                        #calculate full radiative forcing & apply weighting
      RF_GHG <- rowSums(RFx) * w
      RF_SW <- rowSums(RFsw) * w
      RF <- RF_GHG + RF_SW

                                        #calculate radiative frocing from C pulse for comparison
      I_Cpulse <- 1000 * 1/44 #1 Mg CO2 pulse at time 0--> units of kmol
      C_Cpulse <- NaN*Cx[,1]
      C_Cpulse[1] <- I_Cpulse / A
      C_Cpulse[2:T_A] <- I_Cpulse / A * p_x[1:T_A-1]
      RF_Cpulse <- (C_Cpulse * ax[1]) * w

                                        #calculate GHGV--for inclusion of all components (nm=1)
      if (nm == 1) {
	        GHGV <- NaN * RF_GHG
	        swRFV <- NaN * RF_SW
	        cRF_Cpulse <- NaN * RF_Cpulse
	        for (j in 1:T_A) {
	          GHGV[j] <- sum(RF_GHG[1:j,])
	          swRFV[j] <- sum(RF_SW[1:j,])
	          cRF_Cpulse[j] <- sum(RF_Cpulse[1:j])
	        }

	        RFV <- GHGV + swRFV

	        GHGV_C <- GHGV / cRF_Cpulse
	        swRFV_C <- swRFV / cRF_Cpulse
	        RFV_C <- RFV / cRF_Cpulse

	        GHGV_matrix[1:T_A,i] <- GHGV
	        GHGV_C_matrix[1:T_A,i] <- GHGV_C

	        swRFV_matrix[1:T_A,i] <- swRFV
	        swRFV_C_matrix[1:T_A,i] <- swRFV_C

	        RFV_matrix[1:T_A,i] <- RFV
	        RFV_C_matrix[1:T_A,i] <- RFV_C

      }

                                        #separate reporting for each GHG
      RFx_w <- NaN * RFx
      GHGVx <- NaN * RFx
      GHGVx_C <- NaN * RFx

      for (kk in 1:3) {
	        RFx_w[1:T_A,kk] <- (RFx[,kk] * t(w))
	        for (j in 1:T_A) {
	          GHGVx[j,kk] <- sum(RFx_w[1:j,kk])
	        }
	        GHGVx_C[,kk] <- t((GHGVx[,kk] / cRF_Cpulse))
      }

      if (nm == 1) {
	        GHGmatrix[13:15, i] <- GHGVx_C[T_A,]
	        GHGmatrix[16, i] <- sum(GHGmatrix[13:15,i])
      }
      else if (nm == 2) {
        	GHGmatrix[1:3, i] <- GHGVx_C[T_A,]
          GHGmatrix[4, i] <- sum(GHGmatrix[1:3,i])
      }
      else if (nm == 3) {
        	GHGmatrix[5:7, i] <- GHGVx_C[T_A,]
          GHGmatrix[8, i] <- sum(GHGmatrix[5:7,i])
      }
      else if (nm == 4) {
   	      GHGmatrix[9:11, i] <- GHGVx_C[T_A,]
        	GHGmatrix[12, i] <- sum(GHGmatrix[9:11,i])
      }
    }
    
    # determine the scale between the input and output value for sw radiative forcing
    swRFV_scale_factor = swRFV_C_matrix[T_E,i] / sw_radiative_forcing
    # scale latent proportional to above
    instance_output_latent = swRFV_scale_factor * latent_cooling
    
    storage_group = ( GHGmatrix[1,i] + GHGmatrix[2,i] + GHGmatrix[3,i] )
    flux_group = ( GHGmatrix[5,i] + GHGmatrix[6,i] + GHGmatrix[7,i] )
    
    #   CRV= (S_CO2+S_CH4+S_N2O) + (F_CO2+F_CH4+F_N2O)-swRFV+latent
    climate_regulating_value = storage_group + flux_group - swRFV_C_matrix[T_E,i] + instance_output_latent
    
    listResult <- list( name = ecosystem[['name']], 
                        S_CO2 = GHGmatrix[1,i], S_CH4 = GHGmatrix[2,i], S_N2O = GHGmatrix[3,i],
                        F_CO2 = GHGmatrix[5,i], F_CH4 = GHGmatrix[6,i], F_N2O = GHGmatrix[7,i],
                        D_CO2 = (GHGmatrix[1,i] + GHGmatrix[5,i]), D_CH4 = (GHGmatrix[2,i]+GHGmatrix[6,i]), D_N2O = (GHGmatrix[3,i] = GHGmatrix[7,i]),
					              swRFV = swRFV_C_matrix[T_E,i], latent = instance_output_latent, crv = climate_regulating_value )

    jsonResults[i] <- toJSON(listResult)
  }

  jsonResult = "["

  for (i in 1:length(jsonResults)) {
    jsonResult <- paste(jsonResult,jsonResults[i],sep="")
    if (i < length(jsonResults))
      jsonResult <- paste(jsonResult,",",sep="")
  }

  jsonResult <- paste(jsonResult,"]",sep="")
  print(jsonResult)
  return(jsonResult)
}

##' Greenhouse Gas Value Calculator v2
##'
##' R function implementation of GHGV_calculator_web.m; works for n ecosystems
##' @title GHGVC2
##' @param config.list 
##' @export
##' @return GHGVC2 result
##' @author David LeBauer
ghgvc2 <- function(config.list){
  options <- config.list$options
  ecosystems <- names(config.list)[-which(names(config.list)%in% "options")]
  out <- list()
  for (ecosystem in ecosystems){
    out[[ecosystem]] <- fromJSON(ghgvc(options, config.list[[ecosystem]]))
  }
  
  return(toJSON(out))
}
