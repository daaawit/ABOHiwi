% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_outliers.R
\name{check_outliers}
\alias{check_outliers}
\title{Check outliers for a Likert-scale type item}
\usage{
check_outliers(var_vals, ID, data)
}
\arguments{
\item{var_vals}{Variable to plot}

\item{ID}{Unique identifier column from dataframe, which can be used to identify outlier values}

\item{data}{data.frame in which variable is contained}
}
\value{
vector of outliers, if they exist, otherwise nothing is returned
}
\description{
Check outliers for a Likert-scale type item
}
