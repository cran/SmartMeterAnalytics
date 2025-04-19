#' Calculates features from 15-min smart meter data
#'
#' @param B
#'    a vector with length 4*24*7 = 672 measurements in one day in seven days a week
#' @param rowname
#'    the row name of the resulting feature vector
#' @param featsCoarserGranularity
#'    are the features of finer granularity levels also to be calculated (TRUE/FALSE)
#' @param replace_NA_with_defaults
#'    replaces missing (NA) or infinite values that may appear during calculation
#'    with default values
#' @return a data.frame with the calculated features as columns and a specified
#'    rowname, if given
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
#'
#' @examples
#' # Create a random time series of 15-minute smart meter data (672 measurements per week)
#' smd <- runif(n=672, min=0, max=2)
#' # Calculate the smart meter data features
#' calc_features15_consumption(smd)
#'
#' @importFrom stats quantile var cor lowess ts acf stl
#' @importFrom utils combn
#' @export

calc_features15_consumption <- function(B, rowname=NULL,
                                             featsCoarserGranularity=FALSE,
                                             replace_NA_with_defaults=TRUE){
  B <- as.numeric(B)


  #day matrix with 7 cols
  dm15=matrix(as.numeric(B),ncol=7)

  #initialize the data.frame and calculate finer granularity features
  if(featsCoarserGranularity){

    # initialization of the data.frame in the 30-min function
    vec30min <- apply(matrix(B, nrow=2), 2, sum, na.rm=TRUE)
    D=calc_features30_consumption(vec30min, rowname,
                                       featsCoarserGranularity,
                                       replace_NA_with_defaults)
    D$c15_week=mean(dm15, na.rm = TRUE)

  } else {

    #initialize the result data.frame
    D=data.frame(c15_week=mean(dm15, na.rm = TRUE))
    if(!is.null(rowname)) row.names(D) <- rowname
  }


  weekday <-   1:(5*4*24)
  weekend <-   (5*4*24+1):672
  #per day!!
  night <-       ( 1*4+1):( 6*4)
  morning <-     ( 6*4+1):(10*4)
  noon <-        (10*4+1):(14*4)
  afternoon <-   (14*4+1):(18*4)
  evening <-     (18*4+1):(22*4)

  #
  # daytime consumption and relations
  #


#   D$c15_weekday <- mean(dm30[,1:5])
#   D$c15_weekend <- mean(dm30[,6:7])
#
#   D$c15_night <-     mean(dm30[3:12,1:7])
#   D$c15_morning <-   mean(dm30[13:20,1:7])
#   D$c15_noon <-      mean(dm30[21:28,1:7])
#   D$c15_afternoon <- mean(dm30[29:36,1:7])
#   D$c15_evening <-   mean(dm30[37:44,1:7])
#
#   #consumption separated by weekday / weekend
#   D$c15_we_night <- mean(dm30[3:12,6:7])
#   D$c15_wd_night <- mean(dm30[3:12,1:5])
#   D$r15_night_wd_we <- D$c15_wd_night / D$c15_we_night
#
#   D$c15_we_morning <- mean(dm30[13:20,6:7])
#   D$c15_wd_morning <- mean(dm30[13:20,1:5])
#   D$r15_morning_wd_we <- D$c15_wd_morning / D$c15_we_morning
#
#   D$c15_we_noon <- mean(dm30[21:28,6:7])
#   D$c15_wd_noon <- mean(dm30[21:28,1:5])
#   D$r15_noon_wd_we <- D$c15_wd_noon / D$c15_we_noon
#
#   D$c15_we_afternoon <- mean(dm30[29:36,6:7])
#   D$c15_wd_afternoon <- mean(dm30[29:36,1:5])
#   D$r15_afternoon_wd_we <- D$c15_wd_afternoon / D$c15_we_afternoon
#
#   D$c15_we_evening <- mean(dm30[37:44,6:7])
#   D$c15_wd_evening <- mean(dm30[37:44,1:5])
#   D$r15_evening_wd_we <- D$c15_wd_evening / D$c15_we_evening
#
#   #relations
#
#   D$r15_night_day <- D$c15_night / D$c15_week
#   D$r15_we_night_day <- D$c15_we_night / D$c15_weekend
#   D$r15_wd_night_day <- D$c15_wd_night / D$c15_weekday
#
#   D$r15_morning_noon <- D$c15_morning / D$c15_noon
#   D$r15_we_morning_noon <- D$c15_we_morning / D$c15_we_noon
#   D$r15_wd_morning_noon <- D$c15_wd_morning / D$c15_wd_noon
#
#   D$r15_evening_noon <- D$c15_evening / D$c15_noon
#   D$r15_we_evening_noon <- D$c15_we_evening / D$c15_we_noon
#   D$r15_wd_evening_noon <- D$c15_wd_evening / D$c15_wd_noon


  #
  # statistical features
  #
  D$s15_min <- min(dm15, na.rm = TRUE)
  D$s15_max <- max(dm15, na.rm = TRUE)


  D$r15_mean_max <- D$c15_week / D$s15_max
  D$r15_min_mean <- D$s15_min / D$c15_week

  D$s15_we_max <- max(dm15[weekend], na.rm = TRUE)
  D$s15_we_min <- min(dm15[weekend], na.rm = TRUE)
  D$s15_wd_max <- max(dm15[weekday], na.rm = TRUE)
  D$s15_wd_min <- min(dm15[weekday], na.rm = TRUE)

  D$r15_min_wd_we <- ifelse(D$s15_we_min>0 ,  D$s15_wd_min / D$s15_we_min, 1)
  D$r15_max_wd_we <- D$s15_wd_max / D$s15_we_max

  q <- quantile(dm15, na.rm = TRUE)
  #D$s_min <- q[1]
  D$s15_q1  <- q[2]
  D$s15_q2  <- q[3]
  D$s15_q3  <- q[4]
  #D$s_max <- q[5]

  #the average minimum / maximum in the week
  D$s15_min_avg <- mean(c(
    min(dm15[,1], na.rm = TRUE), min(dm15[,2], na.rm = TRUE), min(dm15[,3], na.rm = TRUE),
    min(dm15[,4], na.rm = TRUE), min(dm15[,5], na.rm = TRUE), min(dm15[,6], na.rm = TRUE),
    min(dm15[,7], na.rm = TRUE)))
  D$s15_max_avg <- mean(c(
    max(dm15[,1], na.rm = TRUE), max(dm15[,2], na.rm = TRUE), max(dm15[,3], na.rm = TRUE),
    max(dm15[,4], na.rm = TRUE), max(dm15[,5], na.rm = TRUE), max(dm15[,6], na.rm = TRUE),
    max(dm15[,7], na.rm = TRUE)))

  suppressWarnings({
    #the variance
    D$s15_variance <- var(dm15[1:672], na.rm = TRUE)
    D$s15_var_we <- var(dm15[weekend], na.rm = TRUE)
    D$s15_var_wd <- var(dm15[weekday], na.rm = TRUE)
    D$r15_var_wd_we <- D$s15_var_wd / D$s15_var_we

    # the mean correlation between days
    D$s15_cor <-    mean(cor(dm15[,1:7]), na.rm = TRUE)
    D$s15_cor_we <- mean(cor(dm15[,6:7]), na.rm = TRUE)
    D$s15_cor_wd <- mean(cor(dm15[,1:5]), na.rm = TRUE)
  })

  #correlation between weekdays and weekend
  profile_wd <- apply(dm15[,1:5],1,mean, na.rm = TRUE)
  profile_we <- apply(dm15[,6:7],1,mean, na.rm = TRUE)
  D$s15_cor_wd_we <- cor(profile_wd, profile_we, use="pairwise.complete.obs")

  # small variety
  D$s15_sm_variety <- quantile(abs(diff(B)),.2,na.rm=TRUE)
  # bigger variety
  D$s15_bg_variety <- quantile(abs(diff(B)),.6,na.rm=TRUE)

  #smooth max
  D$s15_sm_max <- mean(apply(dm15[,1:5], 2, function (V) {
    max(
      .5 * V[-c(1,96)]
      + .25 *( V[-c(1,2)] + V[-c(95,96)] )
    )
  }))

  # number_zeros
  D$s15_number_zeros <- sum(B==0)

  #
  #corrected consumptions features based on some statistical features
  #

  #c_evening
  daily=colSums(dm15[evening,1:5]-D$s15_min, na.rm = TRUE)
  D$c15_evening_no_min=mean(daily)

  #c_morning
  daily=colSums(dm15[morning,1:5]-D$s15_min, na.rm = TRUE)
  D$c15_morning_no_min=mean(daily)

  #c_night
  daily=colSums(dm15[night,1:5]-D$s15_min, na.rm = TRUE)
  D$c15_night_no_min=mean(daily)

  #c_noon
  daily=colSums(dm15[noon,1:5]-D$s15_min, na.rm = TRUE)
  D$c15_noon_no_min=mean(daily)

  #c_afternoon
  daily=colSums(dm15[afternoon,1:5]-D$s15_min, na.rm = TRUE)
  D$c15_afternoon_no_min=mean(daily)

  #corrected relations
  D$r15_mean_max_no_min <- min(10, mean(apply(dm15[,1:5],2, function (V)
  {m=min(V); (max(V)-m)/(mean(V)-m) }),na.rm=TRUE))
  D$r15_evening_noon_no_min <- min(10,mean(apply(dm15[,1:5],2,function (V)
  {m=min(V); (sum(V[evening]-m))/(sum(V[21:29]-m))}),na.rm=TRUE))
  D$r15_morning_noon_no_min <- min(10,mean(apply(dm15[,1:5],2,function (V)
  {m=min(V); (sum(V[morning]-m))/(sum(V[21:29]-m))}),na.rm=TRUE))
  D$r15_day_night_no_min <- min(10,mean(apply(dm15[,1:5],2, function (V)
  {m=min(V); (sum(V[night]-m))/(sum(V[3:11]-m))}),na.rm=TRUE))

  #
  # time relevant features
  #
  D$t15_above_0.5kw <- table(dm15>0.5)["TRUE"]
  D$t15_above_1kw <- table(dm15>1)["TRUE"]
  D$t15_above_2kw <- table(dm15>2)["TRUE"]
  D$t15_above_mean <- table(dm15>D$c15_week)["TRUE"]
  D$t15_daily_max <- which.max(dm15)
  D$t15_daily_min <- which.min(dm15)

  #
  # time series features
  #

  # smaller peaks (with plot smothing function stat)
  x=lowess(B[1:240],f=.02)$y
  D$s15_num_peaks=sum(diff(sign(diff(x)))==2)

  # sum of differences to predesessor (absolute value)
  D$s15_diff <- mean(stats::lag(ts(dm15[,1:5]),k=1), na.rm = TRUE)

  #handling NA values in the time series (no STL-Decomposition can be done)
  ts_week <- ts(dm15[1:672], start=1, frequency=96)

  if(anyNA(ts_week)) {
    D$ts15_acf_mean3h <- NA
    D$ts15_stl_varRem <- NA
    D$ts15_acf_mean3h_weekday <- NA
  } else {

    #measure of auto-correlation (above 3 hours time lag) to other days
    D$ts15_acf_mean3h   <- mean(acf(ts_week, lag.max=6, plot=FALSE)$acf)

    ts_weekday <- ts(dm15[,1:5], start=1, frequency = 96)
    D$ts15_acf_mean3h_weekday   <- mean(acf(ts_weekday, lag.max=6, plot=FALSE)$acf)

    #seasonal decomposition -> analysis of the remainer
    s <- stl(ts_week, t.window=672, s.window=96)
    D$ts15_stl_varRem   <- var(as.matrix(s$time.series)[,3])
  }



  #   t_above_base
  D$t15_above_base <- mean(apply(dm15[,1:5] ,2, function(V) {sum( V>(min(V)*2+.1))}))


  # difference between weekdays, +-30 min
  smart_diff <- function(V,W)
  {
    w=vector(length=3)
    w[1]=sum(abs(V[-c(1,96)]-W[-c(1,96)]), na.rm = TRUE)
    w[2]=sum(abs(V[-c(95,96)]-W[-c(1,96)]), na.rm = TRUE)
    w[3]=sum(abs(V[-c(1,2)]-W[-c(1,96)]), na.rm = TRUE)
    return(min(w))
  }

  D$s15_day_diff <- mean(smart_diff(dm15[,1],dm15[,2]),
                         smart_diff(dm15[,2],dm15[,3]),
                         smart_diff(dm15[,3],dm15[,4]),
                         smart_diff(dm15[,4],dm15[,5]))

  #difference between weekdays weak version +- 30 min always
  weak_diff <- function(V,W)
  {
    sum(pmin(abs(V[-c(1,96)]-W[-c(1,96)]),
             abs(V[-c(1,96)]-W[-c(95,96)]),
             abs(V[-c(1,96)]-W[-c(1,2)]), na.rm = TRUE), na.rm = TRUE)
  }

  D$s15_day_diff_weak <- mean(weak_diff(dm15[,1],dm15[,2]),
                              weak_diff(dm15[,2],dm15[,3]),
                              weak_diff(dm15[,3],dm15[,4]),
                              weak_diff(dm15[,4],dm15[,5]))

  #Peak breite
  if(any(is.na(B)))
  {
    D$t15_wide_peaks <- 0
    D$t15_width_peaks <- 0
  }
  else
  {
    d_peaks <- 2
    peaks <- B > (.5 * D$c15_max)
    Non_peaks <- c(which(!peaks), 673)
    lv <- 0
    N_peaks <- 0
    for(i in Non_peaks)
    {
      temp <- sum(peaks[lv:i], na.rm=TRUE)
      if(!is.na(temp) & temp > 1)
      {
        N_peaks <- N_peaks + temp
        d_peaks <- c(d_peaks, temp)
      }
      lv=i
    }
    D$t15_wide_peaks <- N_peaks
    D$t15_width_peaks <- mean(d_peaks)
  }





  #time above base / second approach

  x=sort(B[weekday])/max(B[weekday])
  m=length(x)
  temp=(-.5*x[-c(m,m-1)]+.5*x[-c(1,2)])/(2/m)
  D$t15_time_above_base2 <-   min(which(temp[20:length(temp)]>1),420)
  D$t15_percent_above_base <- x[D$t15_time_above_base2+20]
  D$t15_value_above_base <-   D$t15_percent_above_base*max(B[weekday])
  D$t15_const_time <-         min(which(diff(sign(diff(lowess(temp,f=.2)$y)))==2),460)
  D$t15_value_min_guess <-    x[D$t15_const_time]

  #First time above base
  D$t15_first_above_base <- mean(apply(dm15[20:96,1:5], 2,
                                       function(V) min( which(V>D$t15_value_above_base) ,96)))

  # Big peaks
  x=lowess(B[weekday],f=.05)$y #Glättungsfunktion f. Arbeitstage mit großer Frequenz
  D$s15_num_big_peaks=sum(diff(sign(diff(x)))==2) #Anzahl von Maxima berechnet (Vorzeichenwechsel)

  # smaller peaks
  x=lowess(B[weekday],f=.02)$y
  D$t15_number_small_peaks=sum(diff(sign(diff(x)))==2)


  #distances between big values
  dist_fun=function(V)
  {
    five_biggest=order(V,decreasing=TRUE)[1:3]
    sum(apply(combn(five_biggest,2),2,function(V) abs(diff(V)) ))
  }
  D$t15_dist_big_v=mean(apply(dm15[,1:5],2, dist_fun ))

  #sanitize NA / Inf with default values
  if(replace_NA_with_defaults){

    #values to be replaced with 0
    rep_zero <- c(
    "r15_mean_max_no_min",
    "r15_evening_noon_no_min",
    "r15_morning_noon_no_min",
    "r15_day_night_no_min",
    "r15_var_wd_we",
    "r15_mean_max",
    "r15_min_mean",
    "r15_max_wd_we",
    "ts15_acf_mean3h",
    "ts15_acf_mean3h_weekday",
    "t15_value_min_guess",
    "t15_number_small_peaks",
    "s15_num_big_peaks",
    "s15_num_peaks",
    "s15_cor_wd_we",
    "s15_sm_max",
    "s15_number_zeros",
    "ts15_stl_varRem",
    "t15_percent_above_base")
    for (feat in rep_zero){
      if(is.na(D[[feat]]) | is.infinite(D[[feat]]))
        D[[feat]] <- 0
    }

    #values to be replaced with -1
    rep_min1 <- c(
    "t15_above_0.5kw",
    "t15_above_1kw",
    "t15_above_2kw",
    "t15_above_mean",
    "t15_above_base",
    "t15_value_above_base"
    )
    for (feat in rep_min1){
      if(is.na(D[[feat]]) | is.infinite(D[[feat]]))
        D[[feat]] <- -1
    }

  }

  return(D)
}
