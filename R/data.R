#' Biome Defaults.
#'
#' Contains the default values for biomes, with rows containing variable names
#' and values, and columns containing biome codes. Selecting a specific column
#' provides the default calculator values for ghgvcR.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{carat}{weight of the diamond, in carats}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"biome_defaults"

#' FAO Biomes. 
#' 
#' Provides the grassland and pasture biome codes for a given FAO code.
#' 
#' @format A data frame with 15 rows and 3 variables:
#' \describe{
#'   \item{FAO}{FAO vegetation code.}
#'   \item{GX}{Grassland biome code.}
#'   \item{APX}{Pasture biome code.}
#' }
#' @source \url{?}
"fao_biomes"

#' Koppen-Geiger Climate Zones and Biome Codes.
#' 
#' Data set of Koppen-Geiger climate zones and the biome codes that are 
#' associated with different vegetation types. For looking up the biome code
#' when provided with a climate zone and a vegetation type.
#' 
#' @format A data frame with 31 rows and 13 columns.
#' \describe{
#'   \item{Zone}{Climate zone.}
#'   \item{...}{Vegetation types.}
#' }
"koppen_biomes"

#' Vegetation types by map value.
#' 
#' Boolean indicators of vegetation types available an each map value.
#' 
#' @format a data frame with 118 rows and 15 columns
#' \describe{
#'   \item{Map}{Map from which the value was drawn.}
#'   \item{value}{Map value.}
#'   \item{Category}{Vegetation type category}
#'   \item{...}{Vegetation types (int).}
#' }
"map_vegtypes"