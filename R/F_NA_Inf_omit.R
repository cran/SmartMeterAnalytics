#' Removes the rows with \link{NA} or \link{Inf} values
#'
#' Cleans up a \link{data.frame} or \link{matrix} which is useful for
#' cases wehere you need complete datasets
#'
#' @param V
#'  A \link{data.frame} or \link{matrix} which has to be cleaned
#' @return A cleaned version of \link{data.frame} or \link{matrix}
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @seealso \link{replaceNAsFeatures}, \link{remove_empty_features}
#' @importFrom stats na.omit
#' @export

naInf_omit = function(V)
{
  is.na(V) <- do.call(cbind, lapply(V, is.infinite))
  #rowsToRemove = BadValuesRowIds( V)
  #V= V[- rowsToRemove, ,drop=F]
  V <- na.omit(V)
  return(V)
}
