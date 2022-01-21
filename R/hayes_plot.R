#' Plot interactions generated using Process Hayes.
#' 
#' @import ggplot2
#' @importFrom dplyr pull
#' @importFrom jtools theme_apa
#' 
#' @description A function to visualize the interactions provided
#' by a the Process Hayes macro. Only works with hayesData objects,
#' which are the data class produced by `read_hayes_plot_data`.
#' 
#' @param hayes_data An object of class HayesData, containing the relevant information extracted from
#' the Process Hayes output
#' @param title Title of the plot
#' @param xlab X axis label
#' @param ylab Y axis label
#' @param legend_title Title above the legend
#' @param SD whether the labels of the legend should be replaced with +/-1 SD
#' @param colorblind Whether colorblind-safe colors should be used. If true, uses colors from the colorblind_1 palette in this package.
#' @param APA Whether black and white colors should be used.
#' 
#' @export

hayes_plot_model_1 <- function(hayes_data, title = NULL, xlab = NULL, ylab = NULL, legend_title = NULL, SD = F, colorblind = F, APA = F){
  if (class(hayes_data) != "HayesData") stop("Object must be of type HayesData.")
  if (hayes_data$model_description["Model",] != 1) stop("Can only visualize model 1 right now.")
  
  x_name <- hayes_data$model_description["X",]
  y_name <- hayes_data$model_description["Y",]
  w_name <- hayes_data$model_description["W",]
  
  x_vals <- pull(hayes_data$plot_data, x_name)
  y_vals <- pull(hayes_data$plot_data, y_name)
  w_vals <- factor(pull(hayes_data$plot_data, w_name))
  
  if (APA){
    if(colorblind) message("Cannot do APA and colorblind. Only using APA.")
    palette <- c("#363636", "black", "#999DA0")
  } else if (colorblind) {
    palette <- c(palettes$colorblind_1[4], palettes$colorblind_1[2], palettes$colorblind_1[1])
  } else {
    palette <- c("#F21A00", "#EBCC2A", "#3B9AB2")
  }
    
  plot <- ggplot(data  = hayes_data$plot_data, 
         aes(x = x_vals, 
             y = y_vals,
             color = w_vals)) +
    geom_line(aes(linetype = w_vals)) +
    scale_linetype_manual(values = c("dashed", "solid", "twodash"), guide = "none") +
    scale_color_manual(values = palette, guide = "none") + 
    geom_point(aes(fill = w_vals), size = 4, color = "black", pch = 21)
  
  if(SD){
    plot <- plot + scale_fill_manual(values = palette, labels = c("-1SD", "Mean", "+1SD"))
  } else {
    plot <- plot + scale_fill_manual(values = palette, labels = round(as.numeric(levels(w_vals)), 3))
  }
  
  if(!is.null(title)) plot <- plot + ggtitle(title)
  
  if(!is.null(xlab)) plot <- plot + xlab(xlab) else plot <- plot + xlab("")
  if(!is.null(ylab)) plot <- plot + ylab(ylab) else plot <- plot + ylab("")
  
  if(!is.null(legend_title)){
    plot <- plot + 
      labs(fill = legend_title) + 
      theme_apa(legend.use.title = T)
  } else plot <- plot + theme_apa()
    
  print(plot)
}
