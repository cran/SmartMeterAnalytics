#' Replaces NA values with a given ones
#'
#' Taks a \link{data.frame} and replaces all \link{NA} values with a certain value.
#'
#' @param indata
#'  a \link{data.frame}
#' @param features
#'  a vector of variable names (must be colum names of \code{indata} that are
#'  to be used for \link{NA}-replacement
#' @param replacement
#'  the alternative value, \link{NA} values should be replaced with, zero by default
#'
#' @return the modified data.frame with replaced values
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @seealso \link{naInf_omit}, \link{remove_empty_features}
#' @export

replaceNAsFeatures <- function(indata, features, replacement=0){
  for(f in features){
    if(f %in% colnames(indata)){
      indata[[f]][is.na(indata[[f]])] <- replacement
    }
  }
  return(indata)
}
