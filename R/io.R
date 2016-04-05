#' Writes a json string to a json or csv file.
#' 
#' importFrom jsonlite toJSON fromJSON validate
#' @export
#' 
#' @param json a json object of the ghgv output.
#' @param output_dir the directory to write the data to.
#' @param filename (character) name of file to write.
#' @param format (character) file format to write.
#' @return TRUE if written with no errors.
write_ghgv <- function(json, output_dir, filename="ghgv", format=c("json", "csv")) {
  formats <- match.arg(format, several.ok = TRUE)
  # tryCatch(validate(json),
  #          error = function(e) {
  #            print(json)
  #            stop("json must be a valid json string (jsonlite::validate() must be TRUE.")
  #          })
  
  #JSON
  if ("json" %in% formats) {
    writeLines(json, file.path(output_dir, paste0(filename, ".json")))
  }
  
  #CSV
  if ("csv" %in% formats) {
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
      r1$Location <- i
      r1[r1 == "NaN"] <- NA
      outdf <- rbind(outdf, d1[c(ncols+1, 1:ncols)])
    }
    
    #round the results
    outdf[!colnames(outdf) %in% c("Location", "name")] <- round(outdf[!colnames(outdf) %in% c("Location", "name")], digits = 1)
    
    #fix column names
    colnames(outdf)[colnames(outdf) == "name"] <- "Biome"
    colnames(outdf) <- gsub("D_", "GHGV_", colnames(outdf))
    
    #total ghgv
    outdf$GHGV <- outdf$GHGV_CO2 + outdf$GHGV_CH4 + outdf$GHGV_N2O
    
    #write the data
    outdf$swRFV <- - outdf$swRFV
    write.csv(outdf, file.path(output_dir, paste0(filename,".csv")), row.names = FALSE)
  }
}



