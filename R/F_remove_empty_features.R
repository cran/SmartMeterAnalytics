#' Removes variables with no necessary information from a \link{data.frame}
#'
#' Removes variable names from a list of variables that contain only, or a large
#' portion of, \link{NA} values or have zero bandwidth (if they are numeric)
#' and returns the variable names.
#'
#' The function checks all given column names for the portion of \link{NA} values.
#' If the number of \link{NA} of \link{Inf} exceeds \code{percentage_NA_allowed},
#' the column name is removed from the variable set. Besides, all numeric
#' variables are checked if they have almost zero \code{bandwidth}, are removed.
#'
#' @param all.features
#'    a character vector with all column names of \code{dataset} that should be
#'    considered by the function
#' @param dataset
#'    the dataset as a \link{data.frame}
#' @param percentage_NA_allowed
#'    the percentage of missing values per vector that should be allowed without removing the feature.
#'    All features with NA values that are higher than this level are excluded.
#' @param bandwidth
#'    The length of the interval that values of variable must exceed to be not
#'    removed. By default, half of \code{.Machine$double.eps} is used.
#' @param verbose
#'    boolean if debug messages should be printed when a variable is removed
#'    from the list (uses \link[futile.logger]{flog.debug})
#' @return a vector of variable names that are not considered as empty
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @seealso \link{naInf_omit}, \link{replaceNAsFeatures}
#' @importFrom futile.logger flog.debug
#' @export
remove_empty_features <- function (all.features, dataset,
                                   percentage_NA_allowed = NA,
                                   bandwidth = (.Machine$double.eps ^ 0.5),
                                   verbose = FALSE) {
  if(!is.na(percentage_NA_allowed) && (percentage_NA_allowed<0 | percentage_NA_allowed>=1)){
    stop("The percentage of NA allowed must be a number in the interval of [0; 1[")
  }

  for(f in all.features){

    #remove features with only NA values
    if(all(is.na(dataset[[f]]))){
      all.features <- all.features[-which(all.features==f)]
      if(verbose){
        #writeLines(paste0("Feature ", f, " was removed due to only NA values after scaling!"))
        futile.logger::flog.debug(paste0("Feature ", f, " was removed due to only NA values after scaling!"),
                   name = "classitool.wbf.removeEmptyFeatures")
      }
      next
    }

    if(is.numeric(dataset[[f]])){

      #remove features with only Inf values
      if(all(is.infinite(dataset[[f]])|is.na(dataset[[f]]))){

        all.features <- all.features[-which(all.features==f)]
        if(verbose){
          futile.logger::flog.debug(paste0("Feature ", f, " was removed due to only NA values after scaling!"),
                     name = "classitool.wbf.removeEmptyFeatures")
        }
        next
      }

      #remove features with zero bandwidth (only numeric features and only not NA values)
      values <- dataset[[f]][!is.na(dataset[[f]])]
      if(abs(max(values, na.rm=TRUE) - min(values, na.rm = TRUE)) < bandwidth){

        all.features <- all.features[-which(all.features==f)]
        if(verbose){
          futile.logger::flog.debug(paste0("Feature ", f, " was removed due to zero bandwidth after scaling!"),
                     name = "classitool.wbf.removeEmptyFeatures")
        }
      }

    }
      #remove features with too much missing values
    if(!is.na(percentage_NA_allowed)){

      percentage_NAs <- sum(is.na(dataset[[f]])) / length(dataset[[f]])
      if(percentage_NAs > percentage_NA_allowed){
        all.features <- all.features[-which(all.features==f)]
        if(verbose){
          futile.logger::flog.debug(paste0("Feature ", f, " was removed, since ",
                     round(percentage_NAs*100, digits = 2),
                     "% of the values are NA"), name = "classitool.wbf.removeEmptyFeatures")
        }
      }
    }
  }
  return(all.features)
}
