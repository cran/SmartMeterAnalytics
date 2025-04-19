#' Calculates features from 30-min smart meter data
#'
#' @param B
#'    a vector with length 2*24*7 = 336 measurements in one day in seven days a week
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
#' @references Hopf, K., Sodenkamp, M., Kozlovskiy, I., & Staake, T. (2014).
#'     Feature extraction and filtering for household classification based on
#'     smart electricity meter data. Computer Science-Research and Development,
#'     (31) 3, 141–148. \doi{10.1007/s00450-014-0294-4}
#' @references Hopf, K., Sodenkamp, M., & Staake, T. (2018). Enhancing energy
#'     efficiency in the residential sector with smart meter data analytics.
#'     Electronic Markets, 28(4). \doi{10.1007/s12525-018-0290-9}
#' @references Beckel, C., Sadamori, L., Staake, T., & Santini, S. (2014).
#'     Revealing household characteristics from smart meter data. Energy, 78,
#'     397–410. \doi{10.1016/j.energy.2014.10.025}
#'
#' @examples
#' # Create a random time series of 30-minute smart meter data (336 measurements per week)
#' smd <- runif(n=336, min=0, max=2)
#' # Calculate the smart meter data features
#' calc_features30_consumption(smd)
#'
#' @importFrom stats quantile var cor lowess ts acf stl na.pass na.omit
#' @importFrom utils combn
#' @export

calc_features30_consumption <- function(B, rowname=NULL,
                                             featsCoarserGranularity=FALSE,
                                             replace_NA_with_defaults=TRUE){
  B <- as.numeric(B)

  #day matrix with 7 cols
  dm30=matrix(B,ncol=7)

  #initialize the data.frame and calculate finer granularity features
  if(featsCoarserGranularity){

    # the data.frame is initialized in another feature calculation method

    #TODO: rewrite day/daynt function for 168 measurements or 60min for 336 measurements
    smd60 = B[seq(from=1, to = 335, by=2)] + B[seq(from=2, to=336, by=2)]
    f60 <- calc_features60_consumption(smd60, rowname,
                                            replace_NA_with_defaults)
    fnt <- calc_featuresnt_consumption(B, rowname,
                                            featsCoarserGranularity,
                                            replace_NA_with_defaults)
    D <- cbind(fnt,f60)

    D$c30_week <- mean(dm30, na.rm=TRUE)

  } else {

    # initialization of the dataframe
    D <- data.frame(c30_week=mean(dm30, na.rm=TRUE))
    if(!is.null(rowname)) row.names(D) <- rowname

  }

  #
  # daytime consumption and relations
  #

  D$c30_weekday <- mean(dm30[,1:5], na.rm = TRUE)
  D$c30_weekend <- mean(dm30[,6:7], na.rm = TRUE)

  D$c30_night <-     mean(dm30[3:12,1:7], na.rm = TRUE)
  D$c30_morning <-   mean(dm30[13:20,1:7], na.rm = TRUE)
  D$c30_noon <-      mean(dm30[21:28,1:7], na.rm = TRUE)
  D$c30_afternoon <- mean(dm30[29:36,1:7], na.rm = TRUE)
  D$c30_evening <-   mean(dm30[37:44,1:7], na.rm = TRUE)

  #consumption separated by weekday / weekend
  D$c30_we_night <- mean(dm30[3:12,6:7], na.rm = TRUE)
  D$c30_wd_night <- mean(dm30[3:12,1:5], na.rm = TRUE)
  D$r30_night_wd_we <- D$c30_wd_night / D$c30_we_night

  D$c30_we_morning <- mean(dm30[13:20,6:7], na.rm = TRUE)
  D$c30_wd_morning <- mean(dm30[13:20,1:5], na.rm = TRUE)
  D$r30_morning_wd_we <- D$c30_wd_morning / D$c30_we_morning

  D$c30_we_noon <- mean(dm30[21:28,6:7], na.rm = TRUE)
  D$c30_wd_noon <- mean(dm30[21:28,1:5], na.rm = TRUE)
  D$r30_noon_wd_we <- D$c30_wd_noon / D$c30_we_noon

  D$c30_we_afternoon <- mean(dm30[29:36,6:7], na.rm = TRUE)
  D$c30_wd_afternoon <- mean(dm30[29:36,1:5], na.rm = TRUE)
  D$r30_afternoon_wd_we <- D$c30_wd_afternoon / D$c30_we_afternoon

  D$c30_we_evening <- mean(dm30[37:44,6:7], na.rm = TRUE)
  D$c30_wd_evening <- mean(dm30[37:44,1:5], na.rm = TRUE)
  D$r30_evening_wd_we <- D$c30_wd_evening / D$c30_we_evening
  #browser()
  #relations

  D$r30_night_day <- D$c30_night / D$c30_week
  D$r30_we_night_day <- D$c30_we_night / D$c30_weekend
  D$r30_wd_night_day <- D$c30_wd_night / D$c30_weekday

  D$r30_morning_noon <- D$c30_morning / D$c30_noon
  D$r30_we_morning_noon <- D$c30_we_morning / D$c30_we_noon
  D$r30_wd_morning_noon <- D$c30_wd_morning / D$c30_wd_noon

  D$r30_evening_noon <- D$c30_evening / D$c30_noon
  D$r30_we_evening_noon <- D$c30_we_evening / D$c30_we_noon
  D$r30_wd_evening_noon <- D$c30_wd_evening / D$c30_wd_noon


  #
  # statistical features
  #
  suppressWarnings({
    D$s30_min <- min(dm30[1:336], na.rm = TRUE)
    D$s30_max <- max(dm30[1:336], na.rm = TRUE)
  })


  D$r30_mean_max <- D$c30_week / D$s30_max
  D$r30_min_mean <- D$s30_min / D$c30_week

  D$s30_we_max <- max(dm30[241:336], na.rm = TRUE)
  D$s30_we_min <- min(dm30[241:336], na.rm = TRUE)
  D$s30_wd_max <- max(dm30[1:240], na.rm = TRUE)
  D$s30_wd_min <- min(dm30[1:240], na.rm = TRUE)

  D$r30_min_wd_we <- ifelse( D$s30_we_min>0, D$s30_wd_min / D$s30_we_min ,1)
  D$r30_max_wd_we <- D$s30_wd_max / D$s30_we_max

  q <- quantile(dm30, na.rm = TRUE)
  #D$s_min <- q[1]
  D$s30_q1  <- q[2]
  D$s30_q2  <- q[3]
  D$s30_q3  <- q[4]
  #D$s_max <- q[5]

  #the average minimum in the week
  a <- c(min(dm30[,1], na.rm = TRUE), min(dm30[,2], na.rm = TRUE), min(dm30[,3], na.rm = TRUE), min(dm30[,4], na.rm = TRUE),
         min(dm30[,5], na.rm = TRUE), min(dm30[,6], na.rm = TRUE), min(dm30[,7], na.rm = TRUE))
  a <- ifelse(is.infinite(a), NA, a)
  D$s30_min_avg <- mean(a, na.rm = TRUE)

  #the average maximum in the week
  a <- c(max(dm30[,1], na.rm = TRUE), max(dm30[,2], na.rm = TRUE), max(dm30[,3], na.rm = TRUE), max(dm30[,4], na.rm = TRUE),
         max(dm30[,5], na.rm = TRUE), max(dm30[,6], na.rm = TRUE), max(dm30[,7], na.rm = TRUE))
  a <- ifelse(is.infinite(a), NA, a)
  D$s30_max_avg <- mean(a, na.rm = TRUE)

  #the variance
  D$s30_variance <- var(dm30[1:336], na.rm = TRUE)
  D$s30_var_we <- var(dm30[241:336], na.rm = TRUE)
  D$s30_var_wd <- var(dm30[1:240], na.rm = TRUE)
  D$r30_var_wd_we <- D$s30_var_wd / D$s30_var_we

  # the mean correlation between days
  D$s30_cor <-    mean(cor(dm30[,1:7], use="pairwise.complete.obs"), na.rm = TRUE)
  D$s30_cor_we <- mean(cor(dm30[,6:7], use="pairwise.complete.obs"), na.rm = TRUE)
  D$s30_cor_wd <- mean(cor(dm30[,1:5], use="pairwise.complete.obs"), na.rm = TRUE)

  #correlation between weekdays and weekend
  profile_wd <- apply(dm30[,1:5],1, mean, na.rm = TRUE)
  profile_we <- apply(dm30[,6:7],1, mean, na.rm = TRUE)
  D$s30_cor_wd_we <- cor(profile_wd, profile_we, use="pairwise.complete.obs")

  # small variety
  D$s30_sm_variety <- quantile(abs(diff(B)),.2, na.rm=TRUE)
  # bigger variety
  D$s30_bg_variety <- quantile(abs(diff(B)),.6, na.rm=TRUE)

  #smooth max
  D$s30_sm_max <- mean(apply(dm30[,1:5], 2, function (V) {
    max(
      .5 * V[-c(1,48)]
      + .25 *( V[-c(1,2)] + V[-c(47,48)] ), na.rm = TRUE)
  }))

  # number_zeros
  D$s30_number_zeros <- sum(B==0, na.rm = TRUE)

  #
  #corrected consumptions features based on some statistical features
  #

  #c_evening
  daily=colSums(dm30[37:45,1:5]-D$s30_min, na.rm = TRUE)
  D$c30_evening_no_min=mean(daily)

  #c_morning
  daily=colSums(dm30[13:21,1:5]-D$s30_min, na.rm = TRUE)
  D$c30_morning_no_min=mean(daily)

  #c_night
  daily=colSums(dm30[3:11,1:5]-D$s30_min, na.rm = TRUE)
  D$c30_night_no_min=mean(daily)

  #c_noon
  daily=colSums(dm30[21:29,1:5]-D$s30_min, na.rm = TRUE)
  D$c30_noon_no_min=mean(daily)

  #corrected relations
  D$r30_mean_max_no_min <- min(10, mean(apply(dm30[,1:5],2, function (V)
  {m=min(V); (max(V)-m)/(mean(V)-m) }),na.rm=TRUE))
  D$r30_evening_noon_no_min <- min(10,mean(apply(dm30[,1:5],2,function (V)
  {m=min(V); (sum(V[37:45]-m))/(sum(V[21:29]-m))}),na.rm=TRUE))
  D$r30_morning_noon_no_min <- min(10,mean(apply(dm30[,1:5],2,function (V)
  {m=min(V); (sum(V[13:21]-m))/(sum(V[21:29]-m))}),na.rm=TRUE))
  D$r30_day_night_no_min <- min(10,mean(apply(dm30[,1:5],2, function (V)
  {m=min(V); (sum(V[21:29]-m))/(sum(V[3:11]-m))}),na.rm=TRUE))

  #
  # time relevant features
  #
  D$t30_above_1kw <- table(dm30>1)["TRUE"]
  D$t30_above_2kw <- table(dm30>2)["TRUE"]
  D$t30_above_mean <- table(dm30>D$c30_week)["TRUE"]
  D$t30_daily_max <- which.max(dm30)
  D$t30_daily_min <- which.min(dm30)

  #
  # time series features
  #

  # smaller peaks (with plot smothing function stat)
  x=lowess(B[1:240],f=.02)$y
  D$s30_num_peaks=sum(diff(sign(diff(x)))==2)

  # sum of differences to predesessor (absolute value)
  D$s30_diff <- mean(stats::lag(ts(dm30[,1:5]),k=1), na.rm = TRUE)

  #measure of auto-correlation (above 3 hours time lag) to other days
  ts_week <- ts(dm30[1:336], start=1, frequency = 48)
  D$ts30_acf_mean3h   <- mean(acf(ts_week, lag.max=6, plot=FALSE, na.action = na.pass)$acf)

  ts_weekday <- ts(dm30[,1:5], start=1, frequency = 48)
  D$ts30_acf_mean3h_weekday   <- mean(acf(ts_weekday, lag.max=6, plot=FALSE, na.action = na.pass)$acf)

  #seasonal decomposition -> analysis of the remainer (NA-handling not working)
  try({
    s <- stl(ts_week, t.window=336, s.window=48, na.action = na.omit)
    D$ts30_stl_varRem   <- var(as.matrix(s$time.series)[,3])
  }, silent = TRUE)
  if(is.null(D$ts30_stl_varRem)) D$ts30_stl_varRem <- NA


  #   t_above_base
  D$t30_above_base <- mean(apply(dm30[,1:5] ,2, function(V) {sum( V>(min(V)*2+.1))}))


  # difference between weekdays, +-30 min
  smart_diff <- function(V,W)
  {
    w=vector(length=3)
    w[1]=sum(abs(V[-c(1,48)]-W[-c(1,48)]), na.rm = TRUE)
    w[2]=sum(abs(V[-c(47,48)]-W[-c(1,48)]), na.rm = TRUE)
    w[3]=sum(abs(V[-c(1,2)]-W[-c(1,48)]), na.rm = TRUE)
    return(min(w))
  }

  D$s30_day_diff <- mean(smart_diff(dm30[,1],dm30[,2]),
                         smart_diff(dm30[,2],dm30[,3]),
                         smart_diff(dm30[,3],dm30[,4]),
                         smart_diff(dm30[,4],dm30[,5]))

  #difference between weekdays weak version +- 30 min always
  weak_diff <- function(V,W)
  {
    sum(pmin(abs(V[-c(1,48)]-W[-c(1,48)]),
             abs(V[-c(1,48)]-W[-c(47,48)]),
             abs(V[-c(1,48)]-W[-c(1,2)]), na.rm = TRUE), na.rm = TRUE)
  }

  D$s30_day_diff_weak <- mean(weak_diff(dm30[,1],dm30[,2]),
                              weak_diff(dm30[,2],dm30[,3]),
                              weak_diff(dm30[,3],dm30[,4]),
                              weak_diff(dm30[,4],dm30[,5]))

  #Peak breite
  if(any(is.na(B)))
  {
    D$t30_wide_peaks <- 0
    D$t30_width_peaks <- 0
  }
  else
  {
    d_peaks <- 2
    # all values that are higher than 1/2 * max
    peaks <- B > (.5 * D$s30_max)
    # the positions of these values
    Non_peaks <- c(which(!peaks), 337)
    lv <- 0
    N_peaks <- 0
    for(i in Non_peaks)
    {
      temp <- i - lv
      if(temp > 2)
      {
        N_peaks <- N_peaks + 1
        d_peaks <- c(d_peaks, temp)
      }
      lv=i
    }
    D$t30_wide_peaks <- N_peaks
    D$t30_width_peaks <- mean(d_peaks)
  }

  #time above base / second approach

  x=sort(B[1:240])/max(B[1:240])
  m=length(x)
  temp=(-.5*x[-c(m,m-1)]+.5*x[-c(1,2)])/(2/m)
  D$t30_time_above_base2 <-   min(which(temp[20:length(temp)]>1),210)
  D$t30_percent_above_base <- x[D$t30_time_above_base2+20]
  D$t30_value_above_base <-   D$t30_percent_above_base*max(B[1:240])
  D$t30_const_time <-         min(which(diff(sign(diff(lowess(temp,f=.2)$y)))==2),230)
  D$t30_value_min_guess <-    x[D$t30_const_time]

  #First time above base
  D$t30_first_above_base <- mean(apply(dm30[10:48,1:5], 2,
                                       function(V) min( which(V>D$t30_value_above_base) ,48)))

  # Big peaks
  x=lowess(B[1:240],f=.05)$y #Glättungsfunktion f. Arbeitstage mit großer Frequenz
  D$s30_num_big_peaks=sum(diff(sign(diff(x)))==2) #Anzahl von Maxima berechnet (Vorzeichenwechsel)

  # smaller peaks
  x=lowess(B[1:240],f=.02)$y
  D$t30_number_small_peaks=sum(diff(sign(diff(x)))==2)


  #distances between big values
  dist_fun=function(V)
  {
    five_biggest=order(V,decreasing=TRUE)[1:3]
    sum(apply(combn(five_biggest,2),2,function(V) abs(diff(V)) ))
  }
  D$t30_dist_big_v=mean(apply(dm30[,1:5],2, dist_fun))

  #sanitize NA / Inf with default values
  if(replace_NA_with_defaults){

    #values to be replaced with 0
    rep_zero <- c(
      "r30_night_wd_we",
      "r30_morning_wd_we",
      "r30_noon_wd_we",
      "r30_afternoon_wd_we",
      "r30_evening_wd_we",
      "r30_night_day",
      "r30_we_night_day",
      "r30_wd_night_day",
      "r30_morning_noon",
      "r30_we_morning_noon",
      "r30_wd_morning_noon",
      "r30_evening_noon",
      "r30_we_evening_noon",
      "r30_wd_evening_noon",
      "r30_mean_max_no_min",
      "r30_evening_noon_no_min",
      "r30_morning_noon_no_min",
      "r30_day_night_no_min",
      "r30_mean_max",
      "r30_min_mean",
      "r30_max_wd_we",
      "r30_var_wd_we",
      "s30_cor",
      "s30_cor_we",
      "s30_cor_wd",
      "s30_cor_wd_we",
      "t30_value_min_guess",
      "ts30_acf_mean3h",
      "ts30_acf_mean3h_weekday",
      "t30_percent_above_base")
    for (feat in rep_zero){
      if(is.na(D[[feat]]) | is.infinite(D[[feat]]))
        D[[feat]] <- 0
    }

    #values to be replaced with -1
    rep_min1 <- c(
      "t30_above_1kw",
      "t30_above_2kw",
      "t30_above_mean",
      "t30_above_base",
      "t30_value_above_base"
    )
    for (feat in rep_min1){
      if(is.na(D[[feat]]) | is.infinite(D[[feat]]))
        D[[feat]] <- 0
    }

  }

  return(D)
}
