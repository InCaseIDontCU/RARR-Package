% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resid_fitted_plot.R
\name{resid_fitted_plot}
\alias{resid_fitted_plot}
\title{Function to visualize scaling residuals versus fitted values}
\usage{
resid_fitted_plot(
  formula = "Petal.Width~Sepal.Length+Sepal.Width+Petal.Length",
  data = iris
)
}
\arguments{
\item{formula}{a symbolic description of the model to be fitted, including the response and the predictors}

\item{data}{the name of the data frame}
}
\value{
4 plots corresponding to different scaling residuals versus fitted values
}
\description{
Function to visualize scaling residuals versus fitted values
}
\examples{
library(RARR)
resid_fitted_plot()
}
