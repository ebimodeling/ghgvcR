#' Write Greenhouse Gas value calculator output to file.
#' 
#' @export
#' 
#' @param df data.frame of output from \code{ghgvc()}.
#' @param outdir the directory to write the data to.
#' @param filename (character) name of file to write.
#' @param format (character) file format to write.
#' @return TRUE if written with no errors.
ghgvc_write <- function(df, outdir, filename="output", format=c("json", "csv")) {
  formats <- match.arg(format, several.ok = TRUE)
  
  #JSON
  if ("json" %in% formats) {
    writeLines(df, file.path(outdir, paste0(filename, ".json")))
  }
  
  #CSV
  if ("csv" %in% formats) {
    outlist <- fromJSON(df)
    outdf <- tmpdf <- list()
    for(site in names(outlist)){
      for(pft in seq(length(outlist[[site]]))){
        tmplist <- outlist[[site]][[pft]]
        tmplist <- lapply(tmplist, function(df) ifelse(is.list(df), NA, df))
        tmplist <- lapply(tmplist, function(df) ifelse(df == 0, NA, df))
        tmplist[tmplist == "NaN"] <- NA
        Location <- as.numeric(gsub("_data", "", gsub("site_", "", site)))
        ## Temporary hack needs to be fixed
        ## https://github.com/ebimodeling/ghgvcR/issues/3
        ## https://github.com/ebimodeling/ghgvc/issues/19
        tmpdf <- data.frame(Location = Location, tmplist)
        
        tmp <- as.numeric(tmpdf[!colnames(tmpdf) %in% c("Location", "name")])
        tmpdf[!colnames(tmpdf) %in% c("Location", "name")] <- round(tmp, digits = 1)
        
        outdf <- rbind(outdf, tmpdf)
      }
    }
    
    ## Cleaning up output for downloading
    colnames(outdf)[colnames(outdf) == "name"] <- "Biome"
    colnames(outdf) <- gsub("D_", "GHGV_", colnames(outdf))
    outdf$GHGV <- outdf$GHGV_CO2 + outdf$GHGV_CH4 + outdf$GHGV_N2O
    
    outdf <- outdf[order(outdf$Location),]
    outdf$swRFV <- - outdf$swRFV
    write.csv(outdf, file.path(outdir, paste0(filename,".csv")), row.names = FALSE)
  }
}

