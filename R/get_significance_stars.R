#' This function takes in a vector of p values
#' and returns a vector of stars corresponding to
#' the significance level of the p values

get_significance_stars <- function(p_vals){
  n <- length(p_vals)
  
  stars <- rep("", times = n)
  
  for (i in 1:n){
    if(p_vals[i] < .001){
      stars[i] <- "***"
    } else if (p_vals[i] < .01) {
      stars[i] <- "**"
    } else if (p_vals[i] < .05) {
      stars[i] <- "*"
    } 
  }
  
  return(stars)
}

