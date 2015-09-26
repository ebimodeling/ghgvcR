#' Extract GHG parameters into a matrix.
#' 
#' @param ecosystem a named list of ecosystem traits.
#' @return a matrix of greenhouse gas parameters.
extract_ghg_params <- function(ecosystem) {
  m = matrix(nrow = 8, ncol = 3)
  colnames(m) <- c("CO2", "CH4", "N20")
  rownames(m) <- c("combust", "flux", "FR", "new_flux",
                   "decomp_ag", "decomp_root", "decomp_peat", "decomp_litter")
  
  #Combustion
  m['combust',] <- unlist(ecosystem[c('Ec_CO2', 'Ec_CH4', 'Ec_N2O')]) #proportional release of GHG
  
  #Flux
  m['flux',] <- unlist(ecosystem[c('F_CO2', 'F_CH4', 'F_N2O')])
  m['flux','CO2'] = m['flux','CO2'] + ecosystem[['F_anth']]
  
  #TODO: ??
  m['FR',] <- unlist(ecosystem[c('FR_CO2', 'FR_CH4', 'FR_N2O')])
  
  #New Flux
  m['new_flux',] <- unlist(ecosystem[c('new_F_CO2', 'new_F_CH4', 'new_F_N2O')])
  
  #Above ground decomp
  m['decomp_ag',] <- unlist(ecosystem[c('Ed_CO2_ag_wood_litter', 
                              'Ed_CH4_ag_wood_litter',
                              'Ed_N2O_ag_wood_litter')])
  
  #Root decomp                             
  m['decomp_root',] <- unlist(ecosystem[c('Ed_CO2_root',  
                              'Ed_CH4_root',
                               'Ed_N2O_root')])
  
  #Peat decomp
  m['decomp_peat',] <- unlist(ecosystem[c('Ed_CO2_peat', 
                              'Ed_CH4_peat',
                              'Ed_N2O_peat')])
 
  #Litter decomp 
  m['decomp_litter',] <- unlist(ecosystem[c('Ed_CO2_litter',
                              'Ed_CH4_litter',
                              'Ed_N2O_litter')])
  
  m
}

#' Extract pool parameters into a matrix.
#' 
#' @export
#' 
#' @param ecosystem a list of ecosystem traits.
#' @return a matrix.
extract_pool_params <- function(ecosystem) {
  m <- matrix(nrow = 6, ncol = 4)
  colnames(m) <- c("above_ground", "root", "peat", "som") 
  rownames(m) <- c("storage", "combust", "decomp", 
                   "disturb_storage", "disturb_combust", "disturb_decomp")
  
  #STORAGE, formerly S_p
  # [1] is above ground mass
  m['storage',] <- c(sum(unlist(ecosystem[c('OM_ag', 'OM_wood', 'OM_litter')])),
            ecosystem$OM_root,
            ecosystem$OM_peat,
            ecosystem$OM_SOM)
  
  #Combustion fraction, formerly fc
  m['combust',] <- unlist(ecosystem[c('fc_ag_wood_litter', 
                              'fc_root', 
                              'fc_peat', 
                              'fc_SOM')])
  
  #Decomposition of non-disturbed biomass (k)
  m['decomp',] <- unlist(ecosystem[c('k_ag_wood_litter', 'k_root', 'k_peat', 'k_SOM')])
  
  #Disturbed storage
  m['disturb_storage',] <- m[1,]
  m['disturb_storage','som'] <- NA
  
  #disturbed combusted biomass
  m['disturb_combust',] <- c(unlist(ecosystem[c('dfc_ag_wood_litter', 'dfc_root', 'dfc_peat')]), NA)
 
  #disturbed decay of non-combusted biomass
  m['disturb_decomp',] <- c(unlist(ecosystem[c('dk_ag_wood_litter', 'dk_root', 'dk_peat')]), NA)
  
  m
}

#' Decay Kinetics.
#' 
#' @param time_vector a vector of time moments.
#' @return a matrix of decay kinetics for each time moment.
calc_time_decay <- function(time_vector) {
  t(matrix(c(.217 +
             .259 * exp(-time_vector / 172.9) +
             .338 * exp(-time_vector / 18.51) +
             .186 * exp(-time_vector / 1.186), 
             exp(-time_vector / 12), 
             exp(-time_vector / 114)),
           nrow = 3, byrow = TRUE))
}

#' Decay function for rates over time.
#' 
#' @export
#' 
#' @param r a vector of decay rates.
#' @param t a sequential vector of time steps.
#' @return a matrix of decay fluxes for each time moment. 
decay_func <- function(r, t) {
  exp(-pool_params['decomp',] %o% t) - 
    exp(-pool_params['decomp',] %o% (t+1))
}
