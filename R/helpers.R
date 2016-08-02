#' Decay function for rates over time.
#' 
#' @param r a vector of decay rates.
#' @param t a sequential vector of time steps.
#' @return a matrix of decay fluxes for each time moment. 
decay <- function(r, t) {
  exp(-r %o% t) - 
    exp(-r %o% (t+1))
}


#' Extract GHG parameters into a matrix.
#'  
#' @param ecosystem a named list of ecosystem traits.
#' @param inclAnth boolean whether to include Anthro flux
#' @return a matrix of greenhouse gas parameters.
extract_ghg_params <- function(ecosystem, includeANTH = TRUE) {
  m = matrix(nrow = 8, ncol = 3)
  colnames(m) <- c("CO2", "CH4", "N20")
  rownames(m) <- c("combust", "flux", "FR", "new_flux",
                   "decomp_ag", "decomp_root", "decomp_peat", "decomp_litter")
  
  #Combustion
  m['combust',] <- unlist(ecosystem[c('Ec_CO2', 'Ec_CH4', 'Ec_N2O')]) #proportional release of GHG
  
  #Flux
  m['flux',] <- unlist(ecosystem[c('F_CO2', 'F_CH4', 'F_N2O')])
  if (includeANTH) m['flux','CO2'] = m['flux','CO2'] + unlist(ecosystem[['F_anth']])
  
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


#' Convert GHGVC json results to data.frame.
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @param res a json object containing results from \code{ghgvc()}.
#' @return a data frame of ghgvc results.
json2DF <- function(json) {
  print(json)
  #convert to list of data.frames
  d <- lapply(fromJSON(json), function(r) { data.frame(do.call(rbind.data.frame, r)) })
  ncols <- length(names(d[[1]]))
  
  #out data frame
  outdf <- data.frame(matrix(vector(), 
                             ncol=ncols + 1, 
                             dimnames = list(c(), c("Location", names(d[[1]]))))
  )
  
  #iterate through sites and append to data.frame
  for(i in 1:length(d)) {
    r1 <- d[[i]]
    r1$Location <- length(d) + 1 - i
    r1[r1 == "NaN"] <- NA
    outdf <- rbind(outdf, r1[c(ncols+1, 1:ncols)])
  }
  
  #round the results
  outdf[!colnames(outdf) %in% c("Location", "name")] <- round(outdf[!colnames(outdf) %in% c("Location", "name")], digits = 1)
  
  #fix column names
  colnames(outdf)[colnames(outdf) == "name"] <- "Biome"
  colnames(outdf) <- gsub("D_", "GHGV_", colnames(outdf))
  
  #total ghgv
  outdf$GHGV <- outdf$GHGV_CO2 + outdf$GHGV_CH4 + outdf$GHGV_N2O
  
  #invert swRFV
  outdf$swRFV <- - outdf$swRFV
  
  return(outdf)
}


#' Decay Kinetics.
#' 
#' @param time_vector a vector of time moments.
#' @return a matrix of decay kinetics for each time moment.
kinetic_decay <- function(time_vector) {
  t(matrix(c(.217 +
             .259 * exp(-time_vector / 172.9) +
             .338 * exp(-time_vector / 18.51) +
             .186 * exp(-time_vector / 1.186), 
             exp(-time_vector / 12), 
             exp(-time_vector / 114)),
           nrow = 3, byrow = TRUE))
}


#' Remap latitude and longitude ranges.
#' 
#' @param input the value to remap.
#' @param input_min minimum input value.
#' @param input_max maximum input value.
#' @param output_min minimum output value.
#' @param output_max maximum output value.
remap_range <- function(input, input_min, input_max, output_min, output_max) {
  #Adapted from RUBY code:  
  # def remap_range(input, in_low, in_high, out_low, out_high)
  #   # map onto [0,1] using input range
  #   frac = ( input - in_low ) / ( in_high - in_low )
  #   # map onto output range
  #   ( frac * ( out_high - out_low ) + out_low ).to_i.round()
  # end
  frac <- min(max((input - input_min) / (input_max - input_min), 0), 1)
  as.integer(round((frac * (output_max - output_min)) + output_min))
}


#' Convert string to logical or numeric.
#'
#' @param x (character) a string
#' @return a logical or numeric value
#' @examples \dontrun{
#'   str2LogicalOrNumeric("TRUE") # -> TRUE
#'   str2LogicalOrNumeric("5") # -> 5
#'   str2LogicalOrNumeric("NaN") # -> "NaN"
#' }
str2LogicalOrNumeric <- function(string) {
  if (grepl("TRUE|FALSE", string)) x <- as.logical(string)
  else if (!grepl("[a-zA-Z]", string)) x <- as.numeric(string)
  else x <- string
  x
}







