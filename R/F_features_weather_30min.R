#'Calculates features from one environmental time-series variable and smart meter data
#'
#' @param SMD
#'        the load trace for one week (vector with 672 or 336 elements)
#' @param WEATHER
#'        weather observations (e.g. temperature) in 30-minute readings
#'        (vector with 336 elements)
#' @param rowname
#'        the row name of the current data point
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}, Ilya Kozlovslkiy
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
#' @importFrom stats cor lm coefficients na.omit
#' @export

calc_features_weather=function(SMD, WEATHER, rowname=NULL){
  #convert the 15-min readings to 30-minute intervals (calculate the sum of each
  #columns in a 2 x 336 matrix) and create a matrix with 7 columns for each day
  if(length(SMD)==672){
    dm30 <- matrix(colSums(matrix(SMD, nrow=2)), ncol=7)
  } else if(length(SMD)==336){
    dm30 <- matrix(SMD, ncol=7)
  } else {
    stop(paste0("The smart meter data vector has length ", length(SMD), ", but needs to be either 672 or 336."))
  }

  dm30[is.infinite(dm30)] <- NA

  #create a matrix with 7 columns for each day
  dw30 <- matrix(WEATHER,ncol=7)
  dw30[is.infinite(dw30)] <- NA

  D=data.frame(cor_overall=NA)

  #set the row name if parameter is set
  if(!is.null(rowname)) rownames(D) <- rowname

  #overall correlation
  D$cor_overall=cor(as.vector(dm30),
                    as.vector(dw30),
                    use = "pairwise.complete.obs")

  #the weather-impact on electricity consumption (using linear models)
  M=lm(colMeans(dm30, na.rm = TRUE)~colMeans(dw30, na.rm = TRUE))
  D$cor_daily=coefficients(M)[2]

  #the weather-impact during the night 0am - 5:59am (on weekdays)
  M=lm(as.vector(dm30[1:12,1:5]) ~ as.vector(dw30[1:12,1:5]))
  D$cor_night=coefficients(M)[2]

  #the weather-impact during the day 6am - 5:59pm (on weekdays)
  M=lm(as.vector(dm30[13:36,1:5]) ~ as.vector(dw30[13:36,1:5]))
  D$cor_daytime=coefficients(M)[2]

  #the weather-impact during the day 6:30pm - 9:59pm (on weekdays)
  M=lm(as.vector(dm30[38:44,1:5])~as.vector(dw30[38:44,1:5]))
  D$cor_evening=coefficients(M)[2]

  #correlation of the minima
  suppressWarnings({ #complete days with NA lead to INF
    cmin <- apply(dm30,2,min,na.rm=TRUE)
    wmin <- apply(dw30,2,min,na.rm=TRUE)
  })
  cmin <- ifelse(is.infinite(cmin), NA, cmin)
  wmin <- ifelse(is.infinite(wmin), NA, wmin)
  M=lm(cmin~wmin, na.action = na.omit)
  D$cor_minima=coefficients(M)[2]

  #correlation of consumptio maxima and weather minima
  suppressWarnings({ #complete days with NA lead to INF
    cmax <- apply(dm30,2,max,na.rm=TRUE)
  })
  cmax <- ifelse(is.infinite(cmax), NA, cmax)
  M=lm(cmax~wmin)
  D$cor_maxmin=coefficients(M)[2]

  #correlation of weekday and weekend
  c_wd=mean(dm30[,1:5], na.rm = TRUE)
  t_wd=mean(dw30[,1:5], na.rm = TRUE)
  c_we=mean(dm30[,6:7], na.rm = TRUE)
  t_we=mean(dw30[,6:7], na.rm = TRUE)
  D$cor_weekday_weekend=(c_wd-c_we)/(t_wd-t_we)

  return(D)
}
