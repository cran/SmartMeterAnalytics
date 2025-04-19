
#' Creates a set of all combinations of features
#'
#' @param set vector of available festures that are premutated
#'
#' @return a list of subsets of the input vector
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}, Ilya Kozlovskiy
#' @examples
#'     features_all_subsets(c("A", "B", "C"))
#' @importFrom plyr rlply mlply
#' @export
features_all_subsets <- function(set) {
  n <- length(set)
  bin <- expand.grid(plyr::rlply(n, c(FALSE, TRUE)))
  mlply(bin, function(...) { set[c(...)] })
}
