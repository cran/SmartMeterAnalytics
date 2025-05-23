% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/F_features_daily_multipleTS.R
\name{calc_features_daily_multipleTS}
\alias{calc_features_daily_multipleTS}
\title{Calculates feature from multiple time series data vectors}
\usage{
calc_features_daily_multipleTS(
  el = NULL,
  gas = NULL,
  wa = NULL,
  rowname = NULL,
  cor.useNA = "complete.obs"
)
}
\arguments{
\item{el}{electricity consumption}

\item{gas}{gas consumption}

\item{wa}{water consumption}

\item{rowname}{the name of the consumer (e.g., a household ID in a study database)}

\item{cor.useNA}{an optional character string for the \link{cor} function, specifying a method for
computing covariances in the presence of missing values.}
}
\value{
a data frame with feature values as columns, named by 'rowname'
}
\description{
This function is intended to compute features for daily consumption data from electricity, gas, and water consumption time series data.
}
\references{
Hopf, K. (2019). Predictive Analytics for Energy Efficiency and
    Energy Retailing (1st ed.). Bamberg: University of Bamberg.
    \doi{10.20378/irbo-54833}
}
\author{
Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
}
