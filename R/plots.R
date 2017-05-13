#' Plot Greenhouse Gas values.
#' 
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import scales
#' @importFrom Hmisc capitalize
#' @importFrom tidyr gather
#' @export
#' 
#' @param df a data.frame of output from \code{ghgvc()}.
#' @param output_dir the directory to save the plot image.
#' @param years number of years considered in the analysis.
#' @param save boolean to save plot as an image.
#' @param savefile name of svg file to save.
#' @return a ggplot2 plot object.
plot_ghgv <- function(df, output_dir, 
                      years = 50,
                      save = TRUE, 
                      savefile = "output.svg") {
  
  if (missing(output_dir) && save == TRUE) warning("output_dir must be specified if save is TRUE.")
  
  ### Format biome name
  # first remove underscores and concatenate
  Biome <- capitalize(paste(gsub("_", " ", df$Biome), "Site", df$Location))
  # convert BR to Brazil
  Biome <- gsub("BR", "Brazil", Biome)
  # convert broadleaf conifer forest
  Biome <- gsub("Mixed Broadleaf Conifer Forest", "Mixed Forest", Biome)
    
  #Format data for plotting
  plotdata <- data.frame(
    Biome = Biome,
    Location = df$Location,
    Order = order(df$Location, vegtype_order(df$Biome)),
    Storage = rowSums(df[,grepl("S_", colnames(df))], na.rm = TRUE),
    Ongoing_Exchange = rowSums(df[,grepl("F_", colnames(df))], na.rm = TRUE),
    Rnet = df$swRFV,
    LE = df$latent)
  
  #Create sums
  plotdata$CRV_BGC <- plotdata$Storage + plotdata$Ongoing_Exchange
  plotdata$BGC_NET <- plotdata$Storage + plotdata$Ongoing_Exchange
  
  plotdata$CRV_BIOPHYS <- plotdata$Rnet + plotdata$LE
  plotdata$BIOPHYS_NET <- -abs(plotdata$Rnet) + abs(plotdata$LE)
  
  plotdata$CRV_NET <- plotdata$BGC_NET + plotdata$BIOPHYS_NET
  
  #Replace NA/0 with 0
  plotdata[is.na(plotdata)] <- 0
  plotdata$CRV_BGC[plotdata$CRV_NET == 0] <- 0
  plotdata$CRV_BIOPHYS[plotdata$CRV_NET == 0] <- 0
  plotdata$CRV_NET[plotdata$CRV_NET == 0] <- NA
  
  #Don't plot CRV_NET if BIOPHYS is 0, and 
  #Don't plot CRV_BGC if Ongoing_Exchange is 0
  plotdata$CRV_NET[plotdata$CRV_BIOPHYS == 0] <- NA
  plotdata$CRV_BGC[plotdata$Ongoing_Exchange == 0] <- NA
  
  ## Build data for subplots
  longdata <- gather(plotdata, variable, value, -Biome, -Location, -Order)
  longdata <- longdata[with(longdata, order(Order, Biome)), ]
  longdata$label <- gsub(" Site", "\nSite", longdata$Biome)
  longdata$Biome <- with(longdata, factor(Biome, levels = unique(Biome), ordered = TRUE))
  
  
  print(longdata)
  print(levels(longdata$Biome))
  
  #baseplot for use in grid plots
  baseplot <- ggplot(data = plotdata) + 
    geom_hline(aes(yintercept = 0)) +      #horizontal line
    coord_flip() + 
    theme_minimal() +                      #set ggplot2 theme
    theme(axis.title = element_blank(),    
          axis.text.y = element_blank(),   
          axis.ticks.y = element_blank(),  
          legend.position = "top",       
          panel.grid.major = element_line("black", size = 0.14)
    )
  
  #BGC
  bgc_plot <- ghgvc_subplot(c("Ongoing_Exchange", "Storage"), 
                            data = longdata, 
                            baseplot = baseplot)
  bgc_plot <- bgc_plot + 

    scale_fill_manual(values= brewer_pal(pal = "Greens")(6)[c(4,6)], 
                      labels = c("Ongoing Exchange", "Storage")) + 
    labs(fill = "") + 
    ggtitle("Biogeochemical") + 
    theme(axis.text.y = element_text(size = 12, hjust = 1)) 
  bgc_plot 
  
  #BIOPHYS
  biophys_plot <- ghgvc_subplot(c("LE", "Rnet"), 
                                data = longdata,
                                baseplot = baseplot) 
  biophys_plot <- biophys_plot + 
    scale_fill_manual(values = brewer_pal(pal = "Blues")(6)[c(4,6)], 
                      labels = c(expression("Latent Heat Flux", "Net Radiation"))) + 
    labs(fill = "") + 
    ggtitle("Biophysical")
  
  biophys_plot <- biophys_plot +
    geom_point(data = subset(longdata, variable == "CRV_NET"), 
               aes(x = Biome, y = value))
  
  #CRV
  crv_plot <-  ghgvc_subplot(c("BGC_NET", "BIOPHYS_NET"), 
                             data = longdata,
                             baseplot = baseplot) 
  crv_plot <- crv_plot +
    scale_fill_manual(values= c(brewer_pal(pal = "Greens")(6)[5], 
                                brewer_pal(pal = "Blues")(6)[5]), 
                      labels = c("Biogeochemical", "Biophysical")) + 
    labs(fill = "") +
    ggtitle("Climate Regulating Value")
  
  
  #Add the net Biogeochemical point
  bgc_plot <- bgc_plot +
    geom_point(data = subset(longdata, variable == "BGC_NET"), 
               aes(x = Biome, y = value))
  
  #Add the net Biophysical point
  biophys_plot <- biophys_plot +
    geom_point(data = subset(longdata, variable == "BIOPHYS_NET"), 
               aes(x = Biome, y = value))
  
  
  #Add the net CRV point only if there is biophysical
  crv_plot <- crv_plot +
    geom_point(data = subset(longdata, variable == "CRV_NET"), 
               aes(x = Biome, y = value))
  
  #Create the x label
  xlabels <- as.expression(bquote(paste("CO"[2], " Emission Equivalents (Mg CO"[2],"-eq ha"^{-1}, " ", .(years)," yrs"^{-1},")")))
  
  final_plot <- arrangeGrob(bgc_plot, 
                            biophys_plot, 
                            crv_plot, 
                            ncol = 3, 
                            widths = c(2,1,1),
                            sub = textGrob(xlabels, 
                                           x = unit(1, "npc"), 
                                           y = unit(.9, "npc"),
                                           just = c("centre", "bottom")))
  if(save) {
    #Create the SVG Plot image
    svg(filename=file.path(output_dir, savefile), width = 10, height = 2.5 + 1.5*nrow(plotdata))
    # final_plot <- grid.arrange(bgc_plot, 
    #                            biophys_plot, 
    #                            crv_plot, 
    #                            ncol = 3, 
    #                            widths = c(2,1,1),
    #                            textGrob(xlabels, just = c("centre", "bottom")))
    grid.arrange(final_plot)
    dev.off()
    
    #Fix to remove overflow
    svgfixcmd <- paste("sed -i 's/symbol id/symbol overflow=\"visible\" id/g'", 
                       file.path(output_dir, savefile))
    system(svgfixcmd)
    #sub = annotate("text", x = 2, y = 0.3, parse = T, label = xlabels)))
  }
  final_plot
}


#' Plot ghgcv subplot.
#' 
#' Creates a ggplot2 object using long-form data from \code{ghgcv_plot()} and
#' a set of variables to plot over.
#'
#' @import ggplot2 
#' 
#' @param vars a list of variables to plot.
#' @param longdata long form data.frame of data from \code{ghgcv()}.
#' @param baseplot a ggplot2 object as the base plot.
#' @return a ggplot2 object.
ghgvc_subplot <- function(vars, data, baseplot) {
  #subset the data just including vars
  d <- subset(data, variable %in% vars)
  d$variable <- factor(d$variable, levels = vars)

  d$Biome <- reorder(d$Biome, rev(d$Order))
  
  if(all(vars == c("LE", "Rnet"))){
    d[d$variable == "LE",]$value <- abs(d[d$variable == "LE", ]$value )
    d[d$variable == "Rnet",]$value <- -abs(d[d$variable == "Rnet", ]$value )
  }

  # plot <- geom_bar(data = d,
  #                  aes(x = Biome, y = value, fill = variable),
  #                  width = 0.25, stat = "identity")
  # return(baseplot + plot)
  
  # #Positive Plot
  # pos <- d$value > 0
  # pos[is.na(pos)] <- FALSE #fix cases with NA
  
  # posplot <- if(any(pos)) {
  posplot <-geom_bar(data = d, #[pos,],
             aes(x = Biome, y = value, fill = variable),
             width = 0.25, stat = "identity")
  # } else {
  #   NULL
  # }
  
  # #Negative Plot
  # negplot <- if(any(!pos)) {
  #   geom_bar(data = d[!pos,],
  #            aes(x = Biome, y = value, fill = variable),
  #            width = 0.25, stat = "identity")
  # } else {
  #   NULL
  # }
  
  #return all plots as one object
  return(baseplot + posplot) # + negplot
}





