#' Generate a vector of significance stars for a
#' set of p values.
#'
#' @param p_vals A vector of p values
#' @param sign_levels A list consisting of the significance levels as names and
#' significance signs as values. Names must be provided as strings and can have
#' a leading zero (but don't have to), i.e. can be in form of e.g. ".001" or "0.001".
#' Significance signs must also be provided as strings.
#' @return A vector of significance signs corresponding to the p values

get_significance_stars <- function(p_vals, sign_levels = list(".001" = "***", ".01" = "**", ".05" = "*")){
  n <- length(p_vals)

  sign_levels <- sign_levels[sort(names(sign_levels))] # Reordering significance levels in ascending order

  stars <- rep("", times = n)

  for (i in 1:n){
    for(level in names(sign_levels)){
      if (p_vals[i] < as.numeric(level)){
        stars[i] <- sign_levels[[level]]
        break
      }
    }
  }
  return(stars)
}
