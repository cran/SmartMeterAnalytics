#' Calculates consumption features from weekly consumption only
#'
#' @param B
#'    a vector of any length with measurements
#' @param rowname
#'    the row name of the resulting feature vector
#' @return a data.frame with the calculated features as columns and a specified
#'    rowname, if given
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @references Hopf, K. (2019). Predictive Analytics for Energy Efficiency and
#'     Energy Retailing (1st ed.). Bamberg: University of Bamberg.
#'     \url{https://doi.org/10.20378/irbo-54833}
#' @references Hopf, K., Sodenkamp, M., Kozlovskiy, I., & Staake, T. (2014).
#'     Feature extraction and filtering for household classification based on
#'     smart electricity meter data. Computer Science-Research and Development,
#'     (31) 3, 141–148. \url{https://doi.org/10.1007/s00450-014-0294-4}
#' @references Hopf, K., Sodenkamp, M., & Staake, T. (2018). Enhancing energy
#'     efficiency in the residential sector with smart meter data analytics.
#'     Electronic Markets, 28(4). \url{https://doi.org/10.1007/s12525-018-0290-9}
#' @export
calc_featuresco_consumption <- function(B, rowname=NULL){
  B <- as.numeric(B)

  #day matrix with 7 cols


  # initialization of the dataframe
  # valuex weekend/weekday
  D=data.frame(consumption=mean(B, na.rm=TRUE))
  if(!is.null(rowname))
    row.names(D) <- rowname
  #
  # daytime consumption and relations
  #

  return(D)
}
