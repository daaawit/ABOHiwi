#' Plot distribution of Likert-scale type item
#'
#' @import ggplot2
#' @importFrom jtools theme_apa
#' @importFrom stats median
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
  max_density <- max(as.numeric(prop.table(table(pull(data, var)))))

  plot <- ggplot(data, aes(x = pull(data, var = var))) +
    geom_histogram(aes(y=..density..), binwidth = 0.5) +
    geom_density(alpha=.2, fill = palette[1], color = "#878787") +

    geom_boxplot(aes(y = -0.5*max_density), position = "dodge", width = 0.5*max_density) +
    geom_vline(aes(xintercept = mean(pull(data, var = var), na.rm = T), color = "Mean")) +
    geom_vline(aes(xintercept = median(pull(data, var = var), na.rm = T), color = "Median")) +

    scale_color_manual(values = c(palette[2], palette[3])) +

    xlab("") +
    ylab("") +
    ggtitle(title) +
    theme_apa()

  if(!is.null(max_val)) plot <- plot + xlim(c(0,max_val))
  print(plot)
}
