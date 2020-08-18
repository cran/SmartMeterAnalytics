#' Calculates feature from multiple time series data vectors
#'
#' This function is intended to compute features for daily consumption data from electricity, gas, and water consumption time series data.
#'
#' @param el
#'     electricity consumption
#' @param gas
#'     gas consumption
#' @param wa
#'     water consumption
#' @param cor.useNA
#'     an optional character string for the \link{cor} function, specifying a method for
#'     computing covariances in the presence of missing values.
#' @param rowname
#'     the name of the consumer (e.g., a household ID in a study database)
#'
#' @return a data frame with feature values as columns, named by 'rowname'
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @references Hopf, K. (2019). Predictive Analytics for Energy Efficiency and
#'     Energy Retailing (1st ed.). Bamberg: University of Bamberg.
#'     \url{https://doi.org/10.20378/irbo-54833}
#' @importFrom stats cor lm coefficients
#' @export
calc_features_daily_multipleTS <- function(el=NULL, gas=NULL, wa=NULL,
                                                rowname=NULL, cor.useNA = "complete.obs"){

  #test the input data
  if(length(el)!=length(gas) | length(el)!=length(wa)){
    stop("el, gas and wa have different lengths")
  }

  if(length(el)%%7!=0 | length(gas)%%7!=0 | length(wa)%%7!=0){
    stop("el, gas and wa must have lengths of multiples of 7 (complete weeks)")
  }

  weekdays <- rep(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE), times=(length(el)/7))
  weekend  <- rep(c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE), times=(length(el)/7))

  # if(sum(c(is.null(el),is.null(gas),is.null(wa)))>2)
  #   stop("At least two consumption variables must be given")
  suppressWarnings({
    D=data.frame(cdc_week_el=mean(el, na.rm=TRUE),
                 cdc_week_gas=mean(gas, na.rm = TRUE),
                 cdc_week_wa=mean(wa, na.rm = TRUE))
  })
  row.names(D) <- rowname

  #electricity and water features
  if(!is.null(el) & !is.null(wa) & !all(is.na(el)|is.na(wa))){
    suppressWarnings({          # Warning "stand deviation is zero" for NA-values
      D$cdc_cor_el_wa <- cor(el, wa, use = cor.useNA)
      D$cdc_cor_el_wa_wd <- cor(el[weekdays], wa[weekdays], use = cor.useNA)
      D$cdc_cor_el_wa_we <- cor(el[weekend], wa[weekend], use = cor.useNA)
    })
  } else {
    D$cdc_cor_el_wa <- NA
    D$cdc_cor_el_wa_wd <- NA
    D$cdc_cor_el_wa_we <- NA
  }

  #electricity and gas features
  if(!is.null(el) & !is.null(gas) & !all(is.na(el)|is.na(gas))){
    suppressWarnings({          # Warning "stand deviation is zero" for NA-values
      D$cdc_cor_el_gas <- cor(el, gas, use = cor.useNA)
      D$cdc_cor_el_gas_wd <- cor(el[weekdays], gas[weekdays], use = cor.useNA)
      D$cdc_cor_el_gas_we <- cor(el[weekend], gas[weekend], use = cor.useNA)
    })
  } else {
    D$cdc_cor_el_gas <- NA
    D$cdc_cor_el_gas_wd <- NA
    D$cdc_cor_el_gas_we <- NA
  }

  #gas and water features
  if(!is.null(wa) & !is.null(gas) & !all(is.na(wa)|is.na(gas))){
    suppressWarnings({          # Warning "stand deviation is zero" for NA-values
      D$cdc_cor_wa_gas <- cor(wa, gas, use = cor.useNA)
      D$cdc_cor_wa_gas_wd <- cor(wa[weekdays], gas[weekdays], use = cor.useNA)
      D$cdc_cor_wa_gas_we <- cor(wa[weekend], gas[weekend], use = cor.useNA)
    })
  } else {
    D$cdc_cor_wa_gas <- NA
    D$cdc_cor_wa_gas_wd <- NA
    D$cdc_cor_wa_gas_we <- NA
  }

  #features on all consumption traces
  if(!is.null(el) & !is.null(gas) & !is.null(wa) & !all(is.na(el)|is.na(wa)|is.na(gas))){

    m1 <- lm(el ~ gas + wa)
    D$cdc_lmCoef_gas <- coefficients(m1)[2]
    D$cdc_lmCoef_wa <- coefficients(m1)[3]
  } else {
    D$cdc_lmCoef_gas <-NA
    D$cdc_lmCoef_wa <- NA
  }
  return(D)
}
