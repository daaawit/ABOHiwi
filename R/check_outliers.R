#' Check outliers for a Likert-scale type item
#'
#' @importFrom grDevices boxplot.stats
#' @importFrom dplyr filter
#'
#' @param var_vals Variable to plot
#' @param ID Unique identifier column from dataframe, which can be used to identify outlier values
#' @param data data.frame in which variable is contained
#'
#' @return vector of outliers, if they exist, otherwise nothing is returned

check_outliers <- function(var_vals, ID, data){

  outs <- pull(filter(data, var_vals %in% boxplot.stats(var_vals)$out), ID)

  if(length(outs != 0)){
    print(sprintf("Outliers: %s", paste(outs, collapse = " ")))
    return(as.vector(outs))
  } else {
    print("No outliers removed")
  }
}
