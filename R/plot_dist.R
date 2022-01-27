#' Plot distribution of Likert-scale type item
#'
#' @import ggplot2
#' @importFrom jtools theme_apa
#' @importFrom stats median
#' @importFrom stats density
#'
#' @param var Variable to plot
#' @param data data.frame in which variable is contained
#' @param max_val Maximum value of the item being analyzed (optional). If not supplied, the x axis will scale automatically. If supplied,
#' the x axis will scale between 0 and max_val.
#' @param title Title for the plot, optional
#' @param colorblind Optional change of colors using the colorblind_1 palette from this package.
#'
#' @return Plot of response pattern for the item

plot_dist <- function(var, data, max_val = NULL, title = "", colorblind = F){

  palette <-  if(colorblind) c(palettes$colorblind_1[8], palettes$colorblind_1[4], palettes$colorblind_1[1]) else c("grey", "blue", "red")
  if(is.null(max_val)) max_val <- max(pull(data, var))
  
  max_density <- max(density(pull(data, var))$y)
  width <- 0.1 * max_density
 
  
  plot <- ggplot(data, aes(x = pull(data, var = var))) +
    geom_histogram(aes(y=..density..), binwidth = 0.5) +
    geom_density(alpha=.2, fill = palette[1], color = "#878787") +
    
    geom_boxplot(aes(y = 0 - 0.667 * width), position = "dodge", width = width) +
    
    geom_vline(aes(xintercept = mean(pull(data, var = var), na.rm = T), color = "Mean")) +
    geom_vline(aes(xintercept = median(pull(data, var = var), na.rm = T), color = "Median")) +

    scale_color_manual(values = c(palette[2], palette[3])) +
    scale_x_continuous(breaks = seq(1, max_val, by = 1), limits = c(1, max_val)) + # Step size of 1 on x axis
  
    xlab("") +
    ylab("") +
    ggtitle(title) +
    theme_apa()

  
  print(plot)
}
