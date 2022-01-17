#' Check outliers for a Likert-scale type item
#' 
#' @importFrom grDevices boxplot.stats
#' @importFrom dplyr filter
#' 
#' @param var Variable to plot
#' @param ID Unique identifier column from dataframe, which can be used to identify outlier values
#' @param dat data.frame in which variable is contained
#' 
#' @return vector of outliers, if they exist, otherwise nothing is returned

check_outliers <- function(var, ID, dat){
  
  outs <- pull(filter(dat, pull(dat, var) %in% boxplot.stats(select(dat, var))$out), ID)
  if(length(outs != 0)){
    print(sprintf("Found outlier values: %s", paste(outs, collapse = " ")))
    return(outs)
  } else {
    print("No outliers removed")
  }
}
