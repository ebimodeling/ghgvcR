#' Writes a json string to a json or csv file.
#' 
#' @param json a json object of the ghgv output.
#' @param output_dir the directory to write the data to.
#' @param filename (character) name of file to write.
#' @param format (character) file format to write.
#' @return TRUE if written with no errors.
write_ghgv <- function(json, output_dir, filename="ghgv", format=c("json", "csv")) {
  formats <- match.arg(format, several.ok = TRUE)
  
  #JSON
  if ("json" %in% formats) {
    writeLines(json, file.path(output_dir, paste0(filename, ".json")))
  }
  
  #CSV
  if ("csv" %in% formats) {
    outdf <- json2DF(json)
    outdf[outdf == 0] <- NA
    write.csv(outdf, file.path(output_dir, paste0(filename,".csv")), row.names = FALSE)
  }
}



