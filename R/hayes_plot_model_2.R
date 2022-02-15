#' Plot interactions generated using Process Hayes Model 2.
#' 
#' @import ggplot2
#' @importFrom dplyr pull
#' @importFrom jtools theme_apa
#' @importFrom ggpubr ggarrange
#' @importFrom ggpubr annotate_figure
#' @importFrom ggpubr text_grob
#' 
#' @description A function to visualize the interactions provided
#' by a the Process Hayes macro. Only works with hayesData objects,
#' which are the data class produced by `read_hayes_plot_data`.
#' 
#' @param hayes_data An object of class HayesData, containing the relevant information extracted from
#' the Process Hayes output
#' @param swap_moderators Whether Z and W should be swapped. Will plot Z as second y axis and W as legend by default. 
#' @param align_horizontal Whether the plots should be aligned horizontally or vertically. Defaults to false, which corresponds to vertical alignment, which is the way it is implemented in SPSS.
#' @param round_z_vals Number of digits for second y axis labels. Defaults to 3.
#' @param first_y_hjust Horizontal alignment of first y axis label. It's somewhat of a guessing game to get this right, but I haven't found a better way than this.
#' @param second_y_hjust Horizontal alignment of second y axis label. It's a bit difficult to get this right as well.
#' @param title Title of the plot
#' @param xlab X axis label
#' @param ylab_left First Y axis label
#' @param ylab_right Second Y axis label
#' @param legend_title Title above the legend
#' @param SD whether the labels of the legend should be replaced with +/-1 SD
#' @param colorblind Whether colorblind-safe colors should be used. If true, uses colors from the colorblind_1 palette in this package.
#' @param APA Whether black and white colors should be used.
#' 
#' @returns A GGPlot object.
#' 
#' @export

hayes_plot_model_2 <- function(hayes_data, swap_moderators = F, align_horizontal = F, round_z_vals = 3, first_y_hjust = 1, second_y_hjust = 1, title = NULL, xlab = NULL, ylab_left = NULL, ylab_right = NULL, legend_title = NULL, SD = F, colorblind = F, APA = F){  
  
  x_name <- hayes_data$model_description["X",]
  y_name <- hayes_data$model_description["Y",]
  
  if(swap_moderators) rownames(hayes_data$model_description)[4:5] <- c("Z", "W")
  
  w_name <- hayes_data$model_description["W",]
  z_name <- hayes_data$model_description["Z",]
  
  if(is.null(legend_title)) legend_title <- w_name
  if(is.null(xlab)) xlab <- x_name
  if(is.null(ylab_left)) ylab_left <- y_name
  if(is.null(ylab_right)) ylab_right = z_name  
  
  hayes_data$plot_data$list_factor <- factor(pull(hayes_data$plot_data, z_name)) 
  
  plotlist <- vector(mode = "list")
  
  ylim <- c(min(pull(hayes_data$plot_data, y_name)), max(pull(hayes_data$plot_data, y_name)))
  
  for (level in rev(levels(hayes_data$plot_data$list_factor))){ # Reverse for largest on top, lowest on bottom
    temp_data <- hayes_data
    temp_data$plot_data <- filter(hayes_data$plot_data, list_factor == level)
    temp_data$model_description["Model",] <- "1"
    plotlist[[level]] = hayes_plot_model_1(temp_data, 
                                           title, 
                                           xlab = "", 
                                           ylab = "", # Do this using annotate figure to have unified y label 
                                           legend_title, 
                                           SD, 
                                           colorblind, 
                                           APA) + 
        scale_y_continuous(limits = ylim,
                           sec.axis = dup_axis(breaks = NULL, 
                                               name = round(as.numeric(level), round_z_vals))) + 
        theme(axis.title.y.right = element_text(margin = margin(l = 10)),
              legend.margin = margin(l = 40, 
                                     b = 30),
              plot.margin = margin(l = -10, t = 10))
  }
  
  if(align_horizontal) plot <- ggarrange(plotlist = plotlist, nrow = 1, legend = "right", common.legend = T, align = "hv")
  else plot <- ggarrange(plotlist = plotlist, ncol = 1, legend = "right", common.legend = T, align = "hv")
  
  plot <- annotate_figure(plot, 
                          right = text_grob(ylab_right, rot = -90, vjust = 13.5, hjust = second_y_hjust, face = "bold", size = 13),
                          left = text_grob(ylab_left, rot = 90, hjust = first_y_hjust, face = "bold", size = 13),
                          bottom = text_grob(xlab, face = "bold", size = 13))
  
  return(plot)
}