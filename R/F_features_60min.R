#' Calculates features from 15-min smart meter data
#'
#' @param B
#'    a vector with length 24*7 = 168 measurements in one day in seven days a week
#' @param replace_NA_with_defaults
#'    replaces missing (NA) or infinite values that may appear during calculation
#'    with default values
#' @param rowname
#'    the row name of the resulting feature vector
#' @return a data.frame with the calculated features as columns and a specified
#'    rowname, if given the row name of the resulting feature vector
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @examples
#' # Create a random time series of 60-minute smart meter data (168 measurements per week)
#' smd <- runif(n=168, min=0, max=2)
#' # Calculate the smart meter data features
#' calc_features60_consumption(smd)
#' @importFrom stats quantile var cor lowess ts acf stl na.pass na.omit
#' @importFrom utils combn
#' @export
calc_features60_consumption <- function(B, rowname=NULL,
                                             replace_NA_with_defaults=TRUE){
  B <- as.numeric(B)

  #day matrix with 7 cols
  dm60=matrix(B,ncol=7)

  # initialization of the dataframe
  # valuex weekend/weekday
  D=data.frame(c60_week=mean(dm60, na.rm=TRUE))
  if(!is.null(rowname)) row.names(D) <- rowname
  #
  # daytime consumption and relations
  #

  D$c60_weekday <- mean(dm60[,1:5], na.rm = TRUE)
  D$c60_weekend <- mean(dm60[,6:7], na.rm = TRUE)

  D$c60_night <-     mean(dm60[2:6,1:7], na.rm = TRUE)
  D$c60_morning <-   mean(dm60[7:10,1:7], na.rm = TRUE)
  D$c60_noon <-      mean(dm60[11:14,1:7], na.rm = TRUE)
  D$c60_afternoon <- mean(dm60[15:18,1:7], na.rm = TRUE)
  D$c60_evening <-   mean(dm60[19:22,1:7], na.rm = TRUE)

  #consumption separated by weekday / weekend
  D$c60_we_night <- mean(dm60[2:6,6:7], na.rm = TRUE)
  D$c60_wd_night <- mean(dm60[2:6,1:5], na.rm = TRUE)
  D$r60_night_wd_we <- D$c60_wd_night / D$c60_we_night

  D$c60_we_morning <- mean(dm60[7:10,6:7], na.rm = TRUE)
  D$c60_wd_morning <- mean(dm60[7:10,1:5], na.rm = TRUE)
  D$r60_morning_wd_we <- D$c60_wd_morning / D$c60_we_morning

  D$c60_we_noon <- mean(dm60[11:14,6:7], na.rm = TRUE)
  D$c60_wd_noon <- mean(dm60[11:14,1:5], na.rm = TRUE)
  D$r60_noon_wd_we <- D$c60_wd_noon / D$c60_we_noon

  D$c60_we_afternoon <- mean(dm60[15:18,6:7], na.rm = TRUE)
  D$c60_wd_afternoon <- mean(dm60[15:18,1:5], na.rm = TRUE)
  D$r60_afternoon_wd_we <- D$c60_wd_afternoon / D$c60_we_afternoon

  D$c60_we_evening <- mean(dm60[19:22,6:7], na.rm = TRUE)
  D$c60_wd_evening <- mean(dm60[19:22,1:5], na.rm = TRUE)
  D$r60_evening_wd_we <- D$c60_wd_evening / D$c60_we_evening
  #browser()
  #relations

  D$r60_night_day <- D$c60_night / D$c60_week
  D$r60_we_night_day <- D$c60_we_night / D$c60_weekend
  D$r60_wd_night_day <- D$c60_wd_night / D$c60_weekday

  D$r60_morning_noon <- D$c60_morning / D$c60_noon
  D$r60_we_morning_noon <- D$c60_we_morning / D$c60_we_noon
  D$r60_wd_morning_noon <- D$c60_wd_morning / D$c60_wd_noon

  D$r60_evening_noon <- D$c60_evening / D$c60_noon
  D$r60_we_evening_noon <- D$c60_we_evening / D$c60_we_noon
  D$r60_wd_evening_noon <- D$c60_wd_evening / D$c60_wd_noon


  #
  # statistical features
  #
  D$s60_min <- min(dm60, na.rm = TRUE)
  D$s60_max <- max(dm60, na.rm = TRUE)


  D$r60_mean_max <- D$c60_week / D$s60_max
  D$r60_min_mean <- D$s60_min / D$c60_week

  D$s60_we_max <- max(dm60[,6:7], na.rm = TRUE)
  D$s60_we_min <- min(dm60[,6:7], na.rm = TRUE)
  D$s60_wd_max <- max(dm60[,1:5], na.rm = TRUE)
  D$s60_wd_min <- min(dm60[,1:5], na.rm = TRUE)

  D$r60_min_wd_we <- ifelse( D$s60_we_min>0, D$s60_wd_min / D$s60_we_min ,1)
  D$r60_max_wd_we <- D$s60_wd_max / D$s60_we_max

  q <- quantile(dm60, na.rm = TRUE)
  #D$s_min <- q[1]
  D$s60_q1  <- q[2]
  D$s60_q2  <- q[3]
  D$s60_q3  <- q[4]
  #D$s_max <- q[5]

  #the average minimum in the week
  a <- c(min(dm60[,1], na.rm = TRUE), min(dm60[,2], na.rm = TRUE), min(dm60[,3], na.rm = TRUE), min(dm60[,4], na.rm = TRUE),
         min(dm60[,5], na.rm = TRUE), min(dm60[,6], na.rm = TRUE), min(dm60[,7], na.rm = TRUE))
  a <- ifelse(is.infinite(a), NA, a)
  D$s60_min_avg <- mean(a, na.rm = TRUE)

  #the average maximum in the week
  a <- c(max(dm60[,1], na.rm = TRUE), max(dm60[,2], na.rm = TRUE), max(dm60[,3], na.rm = TRUE), max(dm60[,4], na.rm = TRUE),
         max(dm60[,5], na.rm = TRUE), max(dm60[,6], na.rm = TRUE), max(dm60[,7], na.rm = TRUE))
  a <- ifelse(is.infinite(a), NA, a)
  D$s60_max_avg <- mean(a, na.rm = TRUE)

  #the variance
  D$s60_variance <- var(as.vector(dm60), na.rm = TRUE)
  D$s60_var_we <- var(as.vector(dm60[,6:7]), na.rm = TRUE)
  D$s60_var_wd <- var(as.vector(dm60[,1:5]), na.rm = TRUE)
  D$r60_var_wd_we <- D$s60_var_wd / D$s60_var_we

  # the mean correlation between days
  D$s60_cor <-    mean(cor(dm60[,1:7], use="pairwise.complete.obs"), na.rm = TRUE)
  D$s60_cor_we <- mean(cor(dm60[,6:7], use="pairwise.complete.obs"), na.rm = TRUE)
  D$s60_cor_wd <- mean(cor(dm60[,1:5], use="pairwise.complete.obs"), na.rm = TRUE)

  #correlation between weekdays and weekend
  profile_wd <- apply(dm60[,1:5],1, mean, na.rm = TRUE)
  profile_we <- apply(dm60[,6:7],1, mean, na.rm = TRUE)
  D$s60_cor_wd_we <- cor(profile_wd, profile_we, use="pairwise.complete.obs")

  # small variety
  D$s60_sm_variety <- quantile(abs(diff(B)),.2, na.rm=TRUE)
  # bigger variety
  D$s60_bg_variety <- quantile(abs(diff(B)),.6, na.rm=TRUE)

  #smooth max
  D$s60_sm_max <- mean(apply(dm60[,1:5], 2, function (V) {
    max(
      .5 * V[-c(1,24)]
      + .25 *( V[-c(1,2)] + V[-c(23,24)] ), na.rm = TRUE)
  }))

  # number_zeros
  D$s60_number_zeros <- sum(B==0, na.rm = TRUE)

  #
  #corrected consumptions features based on some statistical features
  #

  #c_evening
  daily=colSums(dm60[19:23,1:5]-D$s60_min, na.rm = TRUE)
  D$c60_evening_no_min=mean(daily)

  #c_morning
  daily=colSums(dm60[7:11,1:5]-D$s60_min, na.rm = TRUE)
  D$c60_morning_no_min=mean(daily)

  #c_night
  daily=colSums(dm60[2:6,1:5]-D$s60_min, na.rm = TRUE)
  D$c60_night_no_min=mean(daily)

  #c_noon
  daily=colSums(dm60[12:16,1:5]-D$s60_min, na.rm = TRUE)
  D$c60_noon_no_min=mean(daily)

  #corrected relations
  D$r60_mean_max_no_min <- min(10, mean(apply(dm60[,1:5],2, function (V)
  {m=min(V); (max(V)-m)/(mean(V)-m) }),na.rm=TRUE))
  D$r60_evening_noon_no_min <- min(10,mean(apply(dm60[,1:5],2,function (V)
  {m=min(V); (sum(V[19:23]-m))/(sum(V[12:16]-m))}),na.rm=TRUE))
  D$r60_morning_noon_no_min <- min(10,mean(apply(dm60[,1:5],2,function (V)
  {m=min(V); (sum(V[7:11]-m))/(sum(V[12:16]-m))}),na.rm=TRUE))
  D$r60_day_night_no_min <- min(10,mean(apply(dm60[,1:5],2, function (V)
  {m=min(V); (sum(V[12:16]-m))/(sum(V[2:6]-m))}),na.rm=TRUE))

  #
  # time relevant features
  #
  D$t60_above_1kw <- table(dm60>2)["TRUE"]
  D$t60_above_2kw <- table(dm60>4)["TRUE"]
  D$t60_above_mean <- table(dm60>D$c60_week)["TRUE"]
  D$t60_daily_max <- which.max(dm60)
  D$t60_daily_min <- which.min(dm60)

  #
  # time series features
  #

  # smaller peaks (with plot smothing function stat)
  x=lowess(B[1:120],f=.04)$y
  D$s60_num_peaks=sum(diff(sign(diff(x)))==2)

  # sum of differences to predesessor (absolute value)
  D$s60_diff <- mean(stats::lag(ts(dm60[,1:5]),k=1), na.rm = TRUE)

  #measure of auto-correlation (above 3 hours time lag) to other days
  ts_week <- ts(dm60[1:168], start=1, frequency = 24)
  D$ts60_acf_mean3h   <- mean(acf(ts_week, lag.max=3, plot=FALSE, na.action = na.pass)$acf)

  ts_weekday <- ts(dm60[,1:5], start=1, frequency = 24)
  D$ts60_acf_mean3h_weekday   <- mean(acf(ts_weekday, lag.max=3, plot=FALSE, na.action = na.pass)$acf)

  #seasonal decomposition -> analysis of the remainer (NA-handling not working)
  try({
    s <- stl(ts_week, t.window=168, s.window=24, na.action = na.omit)
    D$ts60_stl_varRem   <- var(as.matrix(s$time.series)[,3])
  }, silent = TRUE)
  if(is.null(D$ts60_stl_varRem)) D$ts60_stl_varRem <- NA


  #   t_above_base
  D$t60_above_base <- mean(apply(dm60[,1:5] ,2, function(V) {sum( V>(min(V)*2+.2))}))


  # difference between weekdays, +-60 min
  smart_diff <- function(V,W)
  {
    w=vector(length=3)
    w[1]=sum(abs(V[-c(1,24)]-W[-c(1,24)]), na.rm = TRUE)
    w[2]=sum(abs(V[-c(23,24)]-W[-c(1,24)]), na.rm = TRUE)
    w[3]=sum(abs(V[-c(1,2)]-W[-c(1,24)]), na.rm = TRUE)
    return(min(w))
  }

  D$s60_day_diff <- mean(smart_diff(dm60[,1],dm60[,2]),
                         smart_diff(dm60[,2],dm60[,3]),
                         smart_diff(dm60[,3],dm60[,4]),
                         smart_diff(dm60[,4],dm60[,5]))

  #difference between weekdays weak version +- 60 min always
  weak_diff <- function(V,W)
  {
    sum(pmin(abs(V[-c(1,24)]-W[-c(1,24)]),
             abs(V[-c(1,24)]-W[-c(23,24)]),
             abs(V[-c(1,24)]-W[-c(1,2)]), na.rm = TRUE), na.rm = TRUE)
  }

  D$s60_day_diff_weak <- mean(weak_diff(dm60[,1],dm60[,2]),
                              weak_diff(dm60[,2],dm60[,3]),
                              weak_diff(dm60[,3],dm60[,4]),
                              weak_diff(dm60[,4],dm60[,5]))

  #Peak breite
  if(any(is.na(B)))
  {
    D$t60_wide_peaks <- 0
    D$t60_width_peaks <- 0
  }
  else
  {
    d_peaks <- 2
    # all values that are higher than 1/2 * max
    peaks <- B > (.5 * D$s60_max)
    # the positions of these values
    Non_peaks <- c(which(!peaks), 168+1)
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
    D$t60_wide_peaks <- N_peaks
    D$t60_width_peaks <- mean(d_peaks)
  }

  #time above base / second approach

  x=sort(B[1:120])/max(B[1:120])
  m=length(x)
  temp=(-.5*x[-c(m,m-1)]+.5*x[-c(1,2)])/(2/m)
  D$t60_time_above_base2 <-   min(which(temp[10:length(temp)]>1),110)
  D$t60_percent_above_base <- x[D$t60_time_above_base2+20]
  D$t60_value_above_base <-   D$t60_percent_above_base*max(B[1:120])
  D$t60_const_time <-         min(which(diff(sign(diff(lowess(temp,f=.2)$y)))==2),130)
  D$t60_value_min_guess <-    x[D$t60_const_time]

  #First time above base
  D$t60_first_above_base <- mean(apply(dm60[5:24,1:5], 2,
                                       function(V) min( which(V>D$t60_value_above_base) ,24)))

  # Big peaks
  x=lowess(B[1:120],f=.025)$y #Glättungsfunktion f. Arbeitstage mit großer Frequenz
  D$s60_num_big_peaks=sum(diff(sign(diff(x)))==2) #Anzahl von Maxima berechnet (Vorzeichenwechsel)

  # smaller peaks
  x=lowess(B[1:120],f=.01)$y
  D$t60_number_small_peaks=sum(diff(sign(diff(x)))==2)


  #distances between big values
  dist_fun=function(V)
  {
    five_biggest=order(V,decreasing=TRUE)[1:3]
    sum(apply(combn(five_biggest,2),2,function(V) abs(diff(V)) ))
  }
  D$t60_dist_big_v=mean(apply(dm60[,1:5],2, dist_fun ))

  #sanitize NA / Inf with default values
  if(replace_NA_with_defaults){
    #values to be replaced with 0
    rep_zero <- c(
      "r60_night_wd_we",
      "r60_morning_wd_we",
      "r60_noon_wd_we",
      "r60_afternoon_wd_we",
      "r60_evening_wd_we",
      "r60_night_day",
      "r60_we_night_day",
      "r60_wd_night_day",
      "r60_morning_noon",
      "r60_we_morning_noon",
      "r60_wd_morning_noon",
      "r60_evening_noon",
      "r60_we_evening_noon",
      "r60_wd_evening_noon",
      "r60_mean_max",
      "r60_min_mean",
      "s60_cor",
      "s60_cor_we",
      "s60_cor_wd",
      "s60_cor_wd_we",
      "r60_mean_max_no_min",
      "r60_evening_noon_no_min",
      "r60_morning_noon_no_min",
      "r60_day_night_no_min",
      "t60_value_min_guess",
      "ts60_acf_mean3h",
      "ts60_acf_mean3h_weekday",
      "t60_percent_above_base")
    for (feat in rep_zero){
      if(is.na(D[[feat]]) | is.infinite(D[[feat]]))
        D[[feat]] <- 0
    }

    #values to be replaced with -1
    rep_min1 <- c(
      "t60_above_1kw",
      "t60_above_2kw",
      "t60_above_mean",
      "t60_above_base",
      "t60_value_above_base"
    )
    for (feat in rep_min1){
      if(is.na(D[[feat]]) | is.infinite(D[[feat]]))
        D[[feat]] <- 0
    }
  }

  return(D)
}
