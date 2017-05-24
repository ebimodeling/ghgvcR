#' Writes a json string to a json or csv file.
#' 
#' @param json a json object of the ghgv output.
#' @param output_filename (character) the filename to output, extension included.
#' @return TRUE if written with no errors.
write_output <- function(json, output_filename) {
  format <- unlist(strsplit(output_filename, "[.]"))[2]
  
  if(format == "csv") {
    outdf <- json_to_df(json)
    outdf[outdf == 0] <- NA
    write.csv(outdf, file.path(output_filename), row.names = FALSE)
  }
  else if(format == "json") {
    writeLines(json, file.path(output_filename))
  } 
}

#' Writes a json string to a json or csv file.
#' 
#' @import grid
#' @import gridExtra
#'
#' @param plt a ggplot2 object.
#' @param plot_filename (character) the name of the plot file to save, extension included.
#' @return TRUE if written with no errors.
write_plot <- function(plt, plot_filename) {
  nrows <- nrow(plt)
  format <- unlist(strsplit(plot_filename, "[.]"))[2]
  
  if(format == "png") {
    png(filename=file.path(plot_filename), width = 10, height = 2.5 + 1.5*nrows)
    grid.arrange(plt)
    dev.off()
  } else if(format == "svg") {
    svg(filename=file.path(plot_filename), width = 10, height = 2.5 + 1.5*nrows)
    grid.arrange(plt)
    dev.off()
    
    #Fix to remove overflow
    svgfixcmd <- paste("sed -i 's/symbol id/symbol overflow=\"visible\" id/g'", 
                        file.path(plot_filename))
    system(svgfixcmd)
  } 
}