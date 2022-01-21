#' Read Hayes data as .txt file into R
#' 
#' @import stringr
#' @importFrom utils read.delim
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' 
#' @description The process Hayes Macro is quite nice for a lot of different analysis types. However,
#' the R implementation is not quite there yet in terms of usability, whereas the plots you can generate
#' with SPSS are straight up not pretty. This function thus reads in the SPSS output of a Process Hayes 
#' model (provided as a .txt file), finds the plot data and converts it into a R dataframe. It also stores additional
#' parameters of interest.
#' 
#' To extract the data from the Hayes output, this function uses the text in specific lines of the Hayes module. 
#' It might be possible that these change between versions. The current version fo this function works with Hayes
#' version 4.0.
#' 
#' There will be a similar method for R methods called read_hayes_plot_data_R sometime.
#' 
#' @param path Path of the .txt file with the SPSS output script.
#' 
#' @returns A list of class HayesData consisting of the plot_data for visualizing the interaction as well as the various significance 
#' measures from the Hayes SPSS output as well as the model description.
#' 
#' @export

read_hayes_plot_data_spss <- function(path){
  data <- read.delim(path, sep = "\n")
  
  ##### Model description #####
  model_description_rows <- which(data[colnames(data)] == "************************************************************************** ")
  model_description <- data[(model_description_rows[1]) : (model_description_rows[2] - 1), ]
  model_description <- gsub("\\s+", "", str_trim(model_description)) # Replace all spaces between variables with one ";"
  model_description <- read.csv(text = model_description, sep = ":", header = T)
  colnames(model_description) <- c("Var")
  model_description
  
  ##### Model summary #####
  model_summary_row_index <- which(data[colnames(data)] == "Model Summary ")
  model_summary <- data[(model_summary_row_index+1):(model_summary_row_index+2),]
  model_summary <- gsub("\\s+", ";", str_trim(model_summary)) # Replace all spaces between variables with one ";"
  model_summary <- read.csv(text = model_summary, sep = ";")
  
  ##### Plot Data #####
  
  plot_data_start_index <- which(data[colnames(data)] == "DATA LIST FREE/ ") + 1
  plot_data_end_index <- which(data[colnames(data)] == "END DATA. ") - 1
  plot_data <- data[plot_data_start_index:plot_data_end_index, ]
  plot_data <- gsub("\\s+", ";", str_trim(plot_data)) 
  plot_data <- gsub(",", ".", plot_data) # Replace , with . for conversion to numeric
  plot_data <- read.csv(text = plot_data, sep = ";") # Convert to df 
  plot_data <- plot_data[-c(1),] # Remove BEGIN   DATA.
  plot_data[,4] <- NULL # Remove last column, consisting of "."
  plot_data <- data.frame(sapply(plot_data, function(x) as.numeric(x)))
  
  ##### Significance data ##### 
  
  data <- read.delim("C:/Users/David/Desktop/hayes.txt", sep = "\n")
  signif_start_index <- which(data[colnames(data)] == "Model ") + 1
  signif_end_index <- which(data[colnames(data)] == "Product terms key: ") - 2
  signif_data <- data[signif_start_index:signif_end_index,]
  signif_data <-  gsub("\\s+", ";", str_trim(signif_data)) # Remove whitespaces
  signif_data[1] <- paste("Var", signif_data[1], sep = ";") # Variables have no label in Hayes output. Add label "var"
  signif_data <- read.csv(text = signif_data, sep = ";") # Convert to dataframe
  
  interaction_start_index <- which(data[colnames(data)] == "Test(s) of highest order unconditional interaction(s): ")+1
  interaction_end_index <- which(data[colnames(data)] == "---------- ")-1
  interaction_data <- data[interaction_start_index:interaction_end_index,]
  interaction_data <-  gsub("\\s+", ";", str_trim(interaction_data)) # Remove whitespaces
  interaction_data[1] <- paste("Var", interaction_data[1], sep = ";") # Variables have no label in Hayes output. Add label "var"
  interaction_data <- read.csv(text = interaction_data, sep = ";") # Convert to dataframe
  interaction_data
  
  hayes_data <- list(plot_data = plot_data, 
                     model_description = model_description, 
                     model_summary = model_summary, 
                     signif_data = signif_data, 
                     interaction_data = interaction_data)
  class(hayes_data) <- "HayesData"
  
  return(hayes_data)
}
