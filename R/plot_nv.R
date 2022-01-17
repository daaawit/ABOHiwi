#' Plot distribution of Likert-scale type item
#' 
#' @import ggplot2
#' @importFrom jtools theme_apa
#' @importFrom stats median
#' 
#' @param var Variable to plot
#' @param dat data.frame in which variable is contained
#' @param title Title for the plot, optional
#' 
#' @return Plot of response pattern for the item

plot_dist <- function(var, dat, title = ""){
 plot <- ggplot(dat, aes(x = pull(dat, var = var))) +
      geom_histogram(aes(y=..density..), binwidth = 0.5) +
      geom_density(alpha=.2, fill = "red") +
      geom_vline(aes(xintercept = mean(pull(dat, var = var), na.rm = T)), color = "red") + # Median = Rote Linie
      geom_vline(aes(xintercept = median(pull(dat, var = var), na.rm = T)), color = "blue") + # MW = Blaue Linie
      xlab("") +
      ggtitle(title) +
      theme_apa()
 
 plot
}
