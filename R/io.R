#' Writes a json string to a json or csv file.
#' 
#' @param json a json object of the ghgv output.
#' @param output_filename (character) the filename to output, without extension.
#' @param format (character) file format to write.
#' @return TRUE if written with no errors.
write_output <- function(json, output_filename, format=c("json", "csv")) {
  formats <- match.arg(format, several.ok = TRUE)
  
  #JSON
  if ("json" %in% formats) {
    writeLines(json, file.path(paste0(output_filename, ".json")))
  }
  
  #CSV
  if ("csv" %in% formats) {
    outdf <- json2DF(json)
    outdf[outdf == 0] <- NA
    write.csv(outdf, file.path(paste0(output_filename,".csv")), row.names = FALSE)
  }
}

#' Writes a json string to a json or csv file.
#' 
#' @import grid
#' @import gridExtra
#'
#' @param plt a ggplot2 object.
#' @param plot_filename (character) the name of the plot file to save, without extension.
#' @param plot_format (character) the type, either svg or png, of the image to save
#' @param format (character) file format to write.
#' @return TRUE if written with no errors.
write_plot <- function(plt, plot_filename, format=c("svg", "png")) {
  formats <- match.arg(format, several.ok = TRUE)
  
  for(format in formats) {
    write_svg_plot(plt, plot_filename)
  }
  #Fix to remove overflow
  svgfixcmd <- paste("sed -i 's/symbol id/symbol overflow=\"visible\" id/g'", 
                     file.path(paste0(plot_filename, ".svg")))
  system(svgfixcmd)
}

#' Writes a ggplot2 object to an svg file.
#' 
#' @param plt a ggplot2 object.
#' @param plot_filename (character) the filename to output, without extension.
#' @return TRUE if written with no errors.
write_svg_plot <- function(plt, plot_filename) {
  nrows <- 3
  svg(filename=file.path(paste0(plot_filename, ".svg")), width = 10, height = 2.5 + 1.5*nrows)
  grid.arrange(plt)
  dev.off()
  return(TRUE)
} 

#' Writes a ggplot2 object to a png file.
#' 
#' @param plt a ggplot2 object.
#' @param plot_filename (character) the filename to output, without extension.
#' @return TRUE if written with no errors.
write_svg_plot <- function(plt, plot_filename) {
  png(filename=file.path(paste0(plot_filename, ".png")), width = 10, height = 2.5 + 1.5*nrows)
  grid.arrange(plt)
  dev.off()
} 
