#' Calculates consumption features from daily smart meter data
#'
#' @param B
#'    a vector with length 7 measurements
#' @param rowname
#'    the row name of the resulting feature vector
#' @param featsCoarserGranularity
#'    are the features of finer granularity levels also to be calculated (TRUE/FALSE)
#' @param replace_NA_with_defaults
#'    replaces missing (NA) or infinite values that may appear during calculation
#'    with default values
#' @return a data.frame with the calculated features as columns and a specified
#'    rowname, if given
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @references Hopf, K. (2019). Predictive Analytics for Energy Efficiency and
#'     Energy Retailing (1st ed.). Bamberg: University of Bamberg.
#'     \doi{10.20378/irbo-54833}
#' @importFrom stats var quantile ts acf na.pass stl na.omit
#' @export
calc_featuresda_consumption <- function(B, rowname=NULL,
                                             featsCoarserGranularity=FALSE,
                                             replace_NA_with_defaults=TRUE){

  if(length(B)%%7!=0){
    stop("B must have lengths of multiples of 7 (complete weeks)")
  }


  B <- as.numeric(B)

  BM <- matrix(B, nrow=7) #the matrix is filled column-wise

  #initialize the data.frame and calculate finer granularity features
  if(featsCoarserGranularity){

    # the data.frame is initialized in another feature calculation method
    D <- calc_featuresco_consumption(B, rowname)
    D$cda_week <- mean(B, na.rm=TRUE)

  } else {

    # initialization of the dataframe
    D <- data.frame(cda_week=mean(B, na.rm=TRUE))
    if(!is.null(rowname)) row.names(D) <- rowname
  }

  #
  # daytime consumption and relations ----
  #

  D$cda_weekday <- mean(BM[1:5,], na.rm = TRUE)
  D$cda_weekend <- mean(BM[6:7,], na.rm = TRUE)

  #relations
  D$rda_we_wd <- D$cda_weekday / D$cda_weekend



  #
  # statistical features ----
  #
  suppressWarnings({ #warnings when only NA values
    D$sda_min_total <- min(B, na.rm = TRUE)
    D$sda_max_total <- max(B, na.rm = TRUE)
  })

  if(ncol(BM)>1){
    suppressWarnings({ #warnings when only NA values
      D$sda_min_avgWeek <- min(rowMeans(BM), na.rm = TRUE)
      D$sda_max_avgWeek <- max(rowMeans(BM), na.rm = TRUE)
    })
  }



  #the variance
  suppressWarnings({ #warnings when SD is zero
    D$sda_variance <- var(B, na.rm = TRUE)
    D$sda_var_wd <- var(as.vector(BM[1:5,]), na.rm = TRUE)
  })

  if(ncol(BM)>1){

    suppressWarnings({ #warnings when SD is zero
      D$sda_var_we <- var(as.vector(BM[6:7,]), na.rm = TRUE)
    })

    if(!is.na(D$sda_var_we) && D$sda_var_wd!=0){
      D$rda_var_we_wd <- D$sda_var_we / D$sda_var_wd
    } else {
      D$rda_var_we_wd <- -1 # a special value for the case that variance is zero
    }
  }

  #quintiles
  quintiles <- quantile(B, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  D$sda_q1 <- quintiles[1]
  D$sda_q2 <- quintiles[2]
  D$sda_q1 <- quintiles[3]

  # sum of differences to predesessor (absolute value)
  D$sda_diff <- mean(stats::lag(ts(B),k=1), na.rm = TRUE)

  #
  # Relations ----
  #
  D$rda_max_mean <- D$sda_max_total / D$cda_week
  D$rda_min_mean <- D$sda_min_total / D$cda_week

  #
  #time properties ----
  #
  D$tda_days_below_1kWh <- sum(B<1)
  D$tda_days_below_2kWh <- sum(B<2)
  D$tda_days_below_5kWh <- sum(B<5)
  D$tda_days_above_mean <- sum(B>D$cda_week)
  D$tda_number_zeros <- sum(B==0)

  if(ncol(BM)>1){
    suppressWarnings({ #warnings when SD is zero
      #measure of auto-correlation (above 3 hours time lag) to other days
      ts_week <- ts(B, start=1, frequency = 7)
      D$tda_acf_week   <- mean(acf(ts_week, lag.max=6, plot=FALSE, na.action = na.pass)$acf)

      ts_weekday <- ts(BM[1:5,], start=1, frequency = 7)
      D$tda_acf_weekday   <- mean(acf(ts_weekday, lag.max=6, plot=FALSE, na.action = na.pass)$acf)

      #seasonal decomposition -> analysis of the remainer (NA-handling not working)
      try({
        s <- stl(ts_week, t.window=length(B), s.window=7, na.action = na.omit)

          D$tda_stl_varRem   <- var(as.matrix(s$time.series)[,3])
      }, silent = TRUE)
      if(is.null(D$tda_stl_varRem)) D$tda_stl_varRem <- NA
    })
  }

  #sanitize NA / Inf with default values
  if(replace_NA_with_defaults){
    rep_zero <- c(
      "rda_we_wd",
      "rda_max_mean",
      "rda_min_mean")
    for (feat in rep_zero){
      if(is.na(D[[feat]]) | is.infinite(D[[feat]]))
        D[[feat]] <- 0
    }
  }

  return(D)
}
