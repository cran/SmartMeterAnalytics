
#' Interpolate missing readings
#'
#' @author The implementation is adopted from the package imputeTS, function na.interpolate
#' (https://github.com/SteffenMoritz/imputeTS/blob/master/R/na.interpolation.R)
#'
#' @param timeseries Numeric Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object in which missing values shall be replaced
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"linear" - for linear interpolation using \link{approx} }
#'    \item{"spline" - for spline interpolation using \link{spline}}
#'    \item{"stine" - for Stineman interpolation using \link[stinepack]{stinterp}}
#'    }
#' @param ... Additional parameters to be passed through to \link{approx} or \link{spline} interpolation functions
#' @return Vector (\code{\link{vector}}) or Time Series (\code{\link{ts}}) object (dependent on given input at parameter x)
#' @importFrom stats approx spline
#' @importFrom stinepack stinterp
#' @importFrom zoo na.locf
#' @details Missing values get replaced by values of a \link{approx}, \link{spline} or \link[stinepack]{stinterp} interpolation.
#'
interpolate_missingReadings <- function(timeseries, option="linear", ...){

  missindx <- is.na(timeseries)
  n <- length(timeseries)
  allindx <- 1:n
  indx <- allindx[!missindx]
  data_vec <- as.vector(timeseries)


  if(option =="linear") {
    interp <- approx(indx, data_vec[indx],1:n, rule=2, ...)$y
  }
  else if(option == "spline") {
    interp <- spline(indx, data_vec[indx],n = n, ... )$y
  }
  else if(option == "stine") {
    interp <- stinepack::stinterp(indx, data_vec[indx],1:n, ...)$y
    #avoid NAs at the beginning and end of series // same behavior like for approx with rule = 2.
    if(any(is.na(interp))) {interp <- zoo::na.locf(interp, na.remaining= "rev")}
  }
  else {
    stop("Wrong parameter 'option' given. Value must be either 'linear' or 'spline'.")
  }

  timeseries[missindx] <- interp[missindx]
  return(interp)
}
