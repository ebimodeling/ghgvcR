##' Get numbers
##' 
##' Get numbers (e.g., from string or other format)
##' @title Get Nums
##' @param x 
##' @return GHGVC output 
##' @export
##' @author David LeBauer
get.nums <- function(x){
  options(warn=FALSE)
  if(is.na(as.numeric(x))){
    if(is.na(as.logical(x))){
      x <-  as.character(x)
    } else {
      x <-    as.logical(x)
    }
  } else {
    x <-  as.numeric(x)
  }
  print(x)
  return(x)
  options(warn = TRUE)
}        

#' Convert string to logical or numeric
#' 
#' @export
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



