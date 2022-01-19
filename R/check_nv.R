#' Check normal distribution of Likert-scale type item
#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom nortest ad.test
#' @importFrom stats shapiro.test
#' @importFrom stats ks.test
#' @importFrom stats sd
#'
#' @description Check normal distribution of variables detecting whether there are
#' outliers present and plotting the empirical distribution.
#'
#' @param var Variable to check, supplied as a string
#' @param data Dataframe from which the variable stems
#' @param outliers Whether function should check for outliers. If true, will return outlier values.
#' @param ID Column by which outliers should be identified. Function will return a vector consisting of the IDs of the outliers.
#' @param exclude_outliers_in_plot Whether outliers should be removed prior to plotting the distribution of the variable.
#' @param plot Whether ND should be plotted
#' @param max_val Maximum value of the item being analyzed (optional). If not supplied, the x axis will scale automatically. If supplied,
#' the x axis will scale between 0 and max_val. Only used for plotting.
#' @param colorblind Optional change of colors in the plot. using the colorblind_1 palette from this package. Only used for plotting.
#'
#' @export

check_nv <- function(var, data, outliers = FALSE, ID = NULL, exclude_outliers_in_plot = FALSE, plot = TRUE, max_val = NULL, colorblind = F){

  var_vals <- as.numeric(pull(data, var = var))

  title <- var

  if(outliers){
    if(is.null(ID)){
      stop("No ID column supplied")
    } else {
      outs <- check_outliers(var_vals = var_vals, ID = ID, data = data)
      if (length(outs) != 0 && exclude_outliers_in_plot){
        data <- filter(data, !(data[[ID]] %in% outs))
        title <- sprintf("%s with outlier correction", title)
      }
    }
  }

  shap <- shapiro.test(var_vals)
  kgs <- ks.test(var_vals, "pnorm", mean = mean(var_vals), sd = sd(var_vals))
  ad <- ad.test(var_vals)

  print(sprintf("Shapiro-Wilk for %s: W = %.4f, p = %.4f", var, shap$statistic, shap$p.value))
  print(sprintf("Kolmogorov-Smirnov for %s: D = %.4f, p = %.4f", var, kgs$statistic, kgs$p.value))
  print(sprintf("Anderson-Darling for %s: D = %.4f, p = %.4f", var, ad$statistic, ad$p.value))

  if (plot) plot_dist(var, data, max_val = max_val, title = title, colorblind = colorblind)
  if (outliers) return(outs)
}
