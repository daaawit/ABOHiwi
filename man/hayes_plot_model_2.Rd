% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hayes_plot_model_2.R
\name{hayes_plot_model_2}
\alias{hayes_plot_model_2}
\title{Plot interactions generated using Process Hayes Model 2.}
\usage{
hayes_plot_model_2(
  hayes_data,
  swap_moderators = F,
  align_horizontal = F,
  round_z_vals = 3,
  first_y_hjust = 1,
  second_y_hjust = 1,
  title = NULL,
  xlab = NULL,
  ylab_left = NULL,
  ylab_right = NULL,
  legend_title = NULL,
  SD = F,
  colorblind = F,
  APA = F
)
}
\arguments{
\item{hayes_data}{An object of class HayesData, containing the relevant information extracted from
the Process Hayes output}

\item{swap_moderators}{Whether Z and W should be swapped. Will plot Z as second y axis and W as legend by default.}

\item{align_horizontal}{Whether the plots should be aligned horizontally or vertically. Defaults to false, which corresponds to vertical alignment, which is the way it is implemented in SPSS.}

\item{round_z_vals}{Number of digits for second y axis labels. Defaults to 3.}

\item{first_y_hjust}{Horizontal alignment of first y axis label. It's somewhat of a guessing game to get this right, but I haven't found a better way than this.}

\item{second_y_hjust}{Horizontal alignment of second y axis label. It's a bit difficult to get this right as well.}

\item{title}{Title of the plot}

\item{xlab}{X axis label}

\item{ylab_left}{First Y axis label}

\item{ylab_right}{Second Y axis label}

\item{legend_title}{Title above the legend}

\item{SD}{whether the labels of the legend should be replaced with +/-1 SD}

\item{colorblind}{Whether colorblind-safe colors should be used. If true, uses colors from the colorblind_1 palette in this package.}

\item{APA}{Whether black and white colors should be used.}
}
\value{
A GGPlot object.
}
\description{
A function to visualize the interactions provided
by a the Process Hayes macro. Only works with hayesData objects,
which are the data class produced by `read_hayes_plot_data`.
}
