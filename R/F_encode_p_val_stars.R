
#' Encodes p-values with a star rating according to the Significance code:
#'
#' '.' for p-value < 0.1, '*' for < 0.05, '**' for < 0.01, '***' for < 0.001
#'
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @param pval the p-value
#'
#' @return character with the encoding
#' @export
encode_p_val_stars <- function(pval){
  return(
    ifelse(pval<0.001, "***",
           ifelse(pval<0.01, "**",
                  ifelse(pval<0.05, "*",
                         ifelse(pval<0.1, ".", ""))))
    )
}
