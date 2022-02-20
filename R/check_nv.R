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
#' @param exclude_outliers Whether outliers should be removed prior to plotting the distribution of the variable.
#' @param plot Whether ND should be plotted
#' @param title Title. Used for plotting as well as for printing out test statistics. Defaults to variable name if not set.
#' @param ... Arguments to pass to `plot_dist` for plotting of distribution
#'
#' @export

check_nv <- function(var, data, outliers = FALSE, ID = NULL, exclude_outliers = FALSE, plot = TRUE, title = NULL, ...){

  var_vals <- as.numeric(pull(data, var = var))

  if(plot & is.null(title)) title <- var

  if(outliers){
    if(is.null(ID)){
      stop("No ID column supplied")
    } else {
      outs <- check_outliers(var_vals = pull(data, var = var), ID = ID, data = data)
      cat("\n")
      if (length(outs) != 0 && exclude_outliers){
        data <- filter(data, !(data[[ID]] %in% outs))
        title <- sprintf("%s with outlier correction", title)
      }
    }
  }

  shap <- shapiro.test(var_vals)
  kgs <- ks.test(var_vals, "pnorm", mean = mean(var_vals), sd = sd(var_vals))
  ad <- ad.test(var_vals)

  print(sprintf("Shapiro-Wilk for %s: W = %.4f, p = %.4f", ifelse(is.null(title), var, title), shap$statistic, shap$p.value))
  print(sprintf("Kolmogorov-Smirnov for %s: D = %.4f, p = %.4f", ifelse(is.null(title), var, title), kgs$statistic, kgs$p.value))
  print(sprintf("Anderson-Darling for %s: D = %.4f, p = %.4f", ifelse(is.null(title), var, title), ad$statistic, ad$p.value))

  if (plot){
    plot <- plot_dist(var, data, title = title, ...)
    plot
  }
  if (outliers) return(outs)
}


