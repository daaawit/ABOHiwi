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
#' @param dat Dataframe from which the variable stems
#' @param outliers Whether function should return the outliers
#' @param ID Column by which outliers should be identified
#' @param plot Whether ND should be plotted
#'
#' @export

check_nv <- function(var, dat, outliers = FALSE, ID = NULL, plot = T){
  if(outliers == T){
    if(is.null(ID)){
      stop("No ID column supplied")
    } else {
      outs <- check_outliers(var, ID, dat)
      if (length(outs) == 0){
        print("No outliers found")
        title <- sprintf("%s with no outliers found", toString(var))
      } else{
        print(paste("Found ", length(outs), " outliers"))
        print(outs)
        dat <- filter(dat, !(ID %in% outs))
        title <- sprintf("%s with outlier correction", toString(var))
      }
    }
  } else {
    title <- var
  }

  shap <- shapiro.test(pull(dat, var))
  kgs <- ks.test(pull(dat, var), "pnorm", mean = mean(pull(dat, var = var)), sd = sd(pull(dat, var = var)))
  ad <- ad.test(pull(dat, var))

  print(sprintf("Shapiro-Wilk for %s: W = %.4f, p = %.4f", var, shap$statistic, shap$p.value))
  print(sprintf("Kolmogorov-Smirnov for %s: D = %.4f, p = %.4f", var, kgs$statistic, kgs$p.value))
  print(sprintf("Anderson-Darling for %s: D = %.4f, p = %.4f", var, ad$statistic, ad$p.value))

  if (plot) plot_nv(var, dat, title)
}
