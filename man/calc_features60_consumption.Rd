% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/F_features_60min.R
\name{calc_features60_consumption}
\alias{calc_features60_consumption}
\title{Calculates features from 15-min smart meter data}
\usage{
calc_features60_consumption(B, rowname = NULL, replace_NA_with_defaults = TRUE)
}
\arguments{
\item{B}{a vector with length 24*7 = 168 measurements in one day in seven days a week}

\item{rowname}{the row name of the resulting feature vector}

\item{replace_NA_with_defaults}{replaces missing (NA) or infinite values that may appear during calculation
with default values}
}
\value{
a data.frame with the calculated features as columns and a specified
   rowname, if given the row name of the resulting feature vector
}
\description{
Calculates features from 15-min smart meter data
}
\examples{
# Create a random time series of 60-minute smart meter data (168 measurements per week)
smd <- runif(n=168, min=0, max=2)
# Calculate the smart meter data features
calc_features60_consumption(smd)
}
\author{
Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
}
