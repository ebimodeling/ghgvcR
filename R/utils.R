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
