#' Plot correlations as a heatmap with significance stars and confidence intervals
#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr relocate
#' @importFrom reshape2 melt
#' @importFrom psychometric CIr
#' @importFrom stats cor
#' @importFrom stats pt
#' 
#' @description Plots correlations between multiple variables as a heatmap. This function fulfills a similar need to psych::cor.plot()
#' but allows for different values on the x and y axis. It also simplifies a lot of stuff that is annoying about the function in the
#' psych package.
#' 
#' @param data The dataframe from which the data will be sampled
#' @param x Independent variables, provided as a vector containing variable names as strings, e.g. c("iv1", "iv2", "iv3").
#' @param y Dependent variables, optional. Also provided as a vector. If no y values are specified, the function uses the x values for both
#' the x and y axis.
#' @param use Which values to use for the correlation. Can be any option for the `use` command of the `stats::cor` function, i.e. "everything", 
#' "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs". Defaults to "complete.obs".
#' @param method Which correlation method to use. Can be any option for the `method` command of the `stats::cor` function, i.e. "pearson"
#' "kendall", or "spearman". Defaults to "pearson".
#' @param digits How many digits should be shown in the plot. Defaults to 3.
#' @param digit_size How big the digits within each tile should be. Defaults to 4, but will probably need to be adjusted depending
#' on the number of variables
#' @param significance Whether significance of correlation should be computed. Significance will be included both in the table as well as
#' in the plot. Right now, computation of significance is done using the t-distribution, but not accounting for multiple comparisons.
#' Will be implemented in the future as an optional parameter.
#' @param sign_levels A list consisting of the significance levels as names and significance signs (e.g. "***") as values. Names must be provided
#' as strings and can have a leading zero (but don't have to), i.e. can be in form of e.g. ".001" or "0.001". Significance signs must
#' also be provided as strings. Defaults to .001 = three stars, .01 = two stars, .05 = one star
#' @param CI Whether the CI for each correlation should be reported in the table. Defaults to TRUE.
#' @param conf_level Confidence level used for the CI. Defaults to 0.95.
#' @param plot_CI Whether the CI should be included below the correlation within the plot.
#' @param colorblind Whether the colors in the plot should be modified to a colorblind palette. Defaults to FALSE. Uses colors from the palette
#' colorblind_1 in this package if TRUE.
#' @param title Title that should be added to the plot. If not specified, title will be omitted.
#' @param label_rotation How many degrees the label on the x axis (corresponds to the values specified as 'y' if y != NULL) should be rotated. 
#' @param return_plot Whether plot should be returned as an object. If FALSE, plot will be printed
#' but not saved. If TRUE, function will return a list containing the correlation table and the ggplot data as elements.
#' 
#' @return Returns the correlation table and plots it as a heatmap. If return_plot is TRUE, also returns the ggplot object as a list element
#' together with the correlation table. Can be called using print(object$cor_plot)
#'
#' @export



cor_plot <- function(data, x, y = NULL, use = "complete.obs", method = "pearson", digits = 3, digit_size = 4,
                     significance = T, sign_levels = list(".001" = "***", ".01" = "**", ".05" = "*", ".1" = "."),
                     CI = T, conf_level = 0.95, plot_CI = T,
                     colorblind = F, title = NULL, label_rotation = 0, return_plot = F){
  
  n <- nrow(data)
  
  # I sometimes provide subsets of data frames as indeces, but this function only needs colnames. So if
  # something other than colnames are supplied, convert them first
  if(!is.vector(x)) x <- colnames(x)
  if(!is.null(y) & !is.vector(y)) y <- colnames(y)

  cor_data <- select(data, x, y)
  cor_plot_data <- melt(cor(cor_data, use = use, method = method))
  
  if(!is.null(y)){
    cor_plot_data <- filter(cor_plot_data, Var1 %in% y & Var2 %in% x)
  } else {
    cor_plot_data$value <- ifelse(cor_plot_data$Var1 == cor_plot_data$Var2, 0, cor_plot_data$value)
  }
  
  # Put IVs first
  cor_plot_data <- relocate(cor_plot_data, Var2)
  cor_plot_data$Var2 <- factor(cor_plot_data$Var2, ordered = T)

  if(CI){
    # Relies on psychometric package
    CIs <- data.frame(t(sapply(cor_plot_data$value, function(x) psychometric::CIr(x,n, level = conf_level))))
    colnames(CIs) <- c("lower", "higher")

    cor_plot_data <- cbind(cor_plot_data, CIs)
  }

  if(significance){
    # Compute the t values for the correlation by hand and get the corresponding p-values
    df <- n - 2
    t_val <- (cor_plot_data$value * sqrt(df)/sqrt(1 - cor_plot_data$value))

    cor_plot_data$p_val <- 1 - pt(abs(t_val), df = df) # Abs to make it one-directional
    cor_plot_data$sign_level <- get_significance_stars(cor_plot_data$p_val, sign_levels = sign_levels)
  }

  # Color palette
  if(colorblind) palette <- c(palettes$colorblind_1[1], palettes$colorblind_1[4]) else palette <- c("blue", "red")

  plot <- ggplot(cor_plot_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() + 
    scale_x_discrete(position = "top") + 
    scale_y_discrete(limits = rev(levels(cor_plot_data$Var2))) + 
    scale_fill_gradient2(low = palette[1], high = palette[2], midpoint = 0, limit = c(-1,1), name = "r") + 
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = label_rotation, vjust = 0.5, hjust = 1),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      legend.title.align = 0.19, # Center legend across bar
    ) 


  # Labels for the plot
  labels <- round(cor_plot_data$value, digits)
  if (significance) labels <- paste(labels, cor_plot_data$sign_level)
  if (plot_CI){
    if(!CI){
      message("CI is set to false, cannot add it to plot")
    } else {
      CI_label <- sprintf("\n[%.2f, %.2f]", cor_plot_data$lower, cor_plot_data$higher)
      labels <- paste(labels, CI_label)
    }
  }


  plot <- plot +
    geom_text(aes(Var1, Var2, label = labels), color = "black", size = digit_size)
  
  if(!is.null(title)) plot <- plot + 
    ggtitle(title) + 
    theme(plot.title = element_text(hjust = 0.5, margin = margin(0,0,10,0))) # Centering + Bottom margin

  print(plot)
  
  # Rename before exporting
  if(!is.null(y)) colnames(cor_plot_data)[1:3] <- c("IV", "DV", "r") else colnames(cor_plot_data)[1:3] <- c("Var1", "Var2", "r")

  if(return_plot) return(list(cor_data = cor_plot_data, cor_plot = plot)) else return(cor_plot_data)
}
