#' Calculates consumption features from daily (HT / NT) smart meter data
#'
#' The division in HT / NT is done from the input smart meter data
#'
#' HT consumption is during the time 07:00-22:00
#'
#' @param B
#'    a vector with length 2*24*7 = 336 measurements in one day in seven days a week
#' @param rowname
#'    the row name of the resulting feature vector
#' @param featsCoarserGranularity
#'    are the features of finer granularity levels also to be calculated (TRUE/FALSE)
#' @param replace_NA_with_defaults
#'    an optional boolean argument specifying if missing values will be replaced with standard values (i.e., zero values)
#'
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @references Hopf, K. (2019). Predictive Analytics for Energy Efficiency and
#'     Energy Retailing (1st ed.). Bamberg: University of Bamberg.
#'     \doi{10.20378/irbo-54833}
#' @references Hopf, K., Sodenkamp, M., Kozlovskiy, I., & Staake, T. (2014).
#'     Feature extraction and filtering for household classification based on
#'     smart electricity meter data. Computer Science-Research and Development,
#'     (31) 3, 141–148. \doi{10.1007/s00450-014-0294-4}
#' @references Hopf, K., Sodenkamp, M., & Staake, T. (2018). Enhancing energy
#'     efficiency in the residential sector with smart meter data analytics.
#'     Electronic Markets, 28(4). \doi{10.1007/s12525-018-0290-9}
#' @importFrom stats var
#' @export

calc_featuresnt_consumption <- function(B, rowname=NULL,
                                             featsCoarserGranularity=FALSE,
                                             replace_NA_with_defaults=TRUE){
  B <- as.numeric(B)

  #day matrix with 7 cols
  dm30 =matrix(B,ncol=7)

  HTCons = apply( dm30[ 15:44 ,], 1, sum)
  NTCons = apply( dm30[ c(1:14,45:48), ], 1 , sum)

  #initialize the data.frame and calculate finer granularity features
  if(featsCoarserGranularity){

    # the data.frame is initialized in another feature calculation method
    smdda = colSums(dm30)
    D <- calc_featuresda_consumption(smdda, rowname,
                                          featsCoarserGranularity,
                                          replace_NA_with_defaults)
    D$cnt_week <- mean(dm30, na.rm=TRUE)

  } else {

    # initialization of the dataframe
    D=data.frame(cnt_week=mean(dm30, na.rm=TRUE))
    if(!is.null(rowname)) row.names(D) <- rowname

  }

  #
  # daytime consumption and relations
  #

  D$cnt_weekday <- mean(dm30[,1:5], na.rm = TRUE)
  D$cnt_weekend <- mean(dm30[,6:7], na.rm = TRUE)

  D$cnt_NT <-     mean(NTCons, na.rm = TRUE)
  D$cnt_HT <-   mean(HTCons, na.rm = TRUE)

  #consumption separated by weekday / weekend
  D$cnt_we_NT <- mean(NTCons[6:7], na.rm = TRUE)
  D$cnt_wd_NT <- mean(NTCons[1:5], na.rm = TRUE)
  D$rnt_NT_wd_we <- D$cnt_wd_NT / D$cnt_we_NT

  D$cnt_we_HT <- mean(HTCons[6:7], na.rm = TRUE)
  D$cnt_wd_HT <- mean(HTCons[1:5], na.rm = TRUE)
  D$rnt_HT_wd_we <- D$cnt_wd_HT / D$cnt_we_HT

  #browser()
  #relations

  D$rnt_NT_HT <- D$cnt_NT / D$cnt_HT
  D$rnt_we_NT_HT <- D$cnt_we_NT / D$cnt_weekend
  D$rnt_wd_NT_HT <- D$cnt_wd_NT / D$cnt_weekday



  #
  # statistical features
  #
  D$snt_HT_min <- min(HTCons, na.rm = TRUE)
  D$snt_NT_min <- min(NTCons, na.rm=TRUE)
  D$snt_HT_max <- max(HTCons, na.rm = TRUE)
  D$snt_NT_max <- max(NTCons, na.rm=TRUE)


  D$rnt_HT_mean_max <- D$cnt_HT/ D$snt_HT_max
  D$rnt_NT_mean_max <- D$cnt_NT/ D$snt_NT_max

  D$rnt_HT_mean_min <- D$cnt_HT/ D$snt_HT_min
  D$rnt_NT_mean_min <- D$cnt_NT/ D$snt_NT_min

  #the variance
  D$snt_NT_variance <- var(NTCons, na.rm = TRUE)
  D$snt_NT_var_wd <- var(NTCons[1:5], na.rm = TRUE)

  D$snt_HT_variance <- var(HTCons, na.rm = TRUE)
  D$snt_HT_var_wd <- var(HTCons[1:5], na.rm = TRUE)

  #sanitize NA / Inf with default values
  if(replace_NA_with_defaults){
    rep_zero <- c(
      "rnt_NT_wd_we",
      "cnt_we_HT",
      "cnt_wd_HT",
      "rnt_NT_HT",
      "rnt_we_NT_HT",
      "rnt_wd_NT_HT",
      "rnt_NT_mean_max",
      "rnt_HT_mean_max",
      "rnt_NT_mean_min",
      "rnt_HT_mean_min")
    for (feat in rep_zero){
      if(is.na(D[[feat]]) | is.infinite(D[[feat]]))
        D[[feat]] <- 0
    }
  }

  return(D)
}
