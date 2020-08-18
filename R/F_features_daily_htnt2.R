#' Calculates consumption features from daily (HT / NT) smart meter data
#'
#' The division in HT / NT is done from the input smart meter data
#'
#' @param HTCons
#'    a vector with 7 measurements for HT consumption in one week (beginning with monday)
#' @param NTCons
#'    a vector with 7 measurements for NT consumption in one week (beginning with monday)
#' @param rowname
#'    the row name of the resulting feature vector
#' @param featsCoarserGranularity
#'    are the features of finer granularity levels also to be calculated (T/FALSE)
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @references Hopf, K. (2019). Predictive Analytics for Energy Efficiency and
#'     Energy Retailing (1st ed.). Bamberg: University of Bamberg.
#'     \url{https://doi.org/10.20378/irbo-54833}
#' @importFrom stats var
#' @export

calc_featureshtnt_consumption2 <- function(HTCons, NTCons, rowname=NULL,
                                             featsCoarserGranularity=FALSE){

  #test the input data
  if(length(HTCons)!=length(NTCons)){
    stop("HTCons and NTCons have different lengths")
  }

  if(length(HTCons)%%7!=0 | length(NTCons)%%7!=0){
    stop("HTCons and NTCons must have lengths of multiples of 7 (complete weeks)")
  }

  HTConsM <- matrix(HTCons, nrow=7) #the matrix is filled column-wise
  NTConsM <- matrix(NTCons, nrow=7) #the matrix is filled column-wise
  #B <- as.numeric(B)

  #day matrix with 7 cols
  #dm30 =matrix(B,ncol=7)

  #HTCons = apply( dm30[ 15:44 ,], 1, sum)
  #NTCons = apply( dm30[ c(1:14,45:48), ], 1 , sum)

  #initialize the data.frame and calculate finer granularity features
  if(featsCoarserGranularity){

    # the data.frame is initialized in another feature calculation method
    smdda = HTCons + NTCons
    D <- calc_featuresda_consumption(smdda, rowname, featsCoarserGranularity)
    D$cnt_week <- mean(smdda, na.rm=T)

  } else {

    # initialization of the dataframe
    D=data.frame(cnt_week=mean(smdda, na.rm=T))
    if(!is.null(rowname)) row.names(D) <- rowname

  }

  #
  # daytime consumption and relations
  #

  D$cnt_NT <-   mean(NTCons, na.rm = T)
  D$cnt_HT <-   mean(HTCons, na.rm = T)

  #consumption separated by weekday / weekend
  D$cnt_we_NT <- mean(NTConsM[6:7,], na.rm = T)
  D$cnt_wd_NT <- mean(NTConsM[1:5,], na.rm = T)
  D$rnt_NT_wd_we <- D$cnt_wd_NT / D$cnt_we_NT

  D$cnt_we_HT <- mean(HTConsM[6:7,], na.rm = T)
  D$cnt_wd_HT <- mean(HTConsM[1:5,], na.rm = T)
  D$rnt_HT_wd_we <- D$cnt_wd_HT / D$cnt_we_HT

  #relations

  D$rnt_HT_percentage <- D$cnt_HT / (D$cnt_HT + D$cnt_HT)
  D$rnt_HT_we_percentage <- D$cnt_we_NT / (D$cnt_HT + D$cnt_HT)
  D$rnt_HT_wd_percentage <- D$cnt_wd_NT / (D$cnt_HT + D$cnt_HT)



  #
  # statistical features
  #
  suppressWarnings({ #warnings when only NA values
    D$snt_HT_min_total <- min(HTCons, na.rm = T)
    D$snt_NT_min_total <- min(NTCons, na.rm=T)
    D$snt_HT_max_total <- max(HTCons, na.rm = T)
    D$snt_NT_max_total <- max(NTCons, na.rm=T)
  })


  #when multiple weeks are available
  if(ncol(HTConsM)>1){
    suppressWarnings({ #warnings when only NA values
      D$snt_HT_min_avgWeek <- min(rowMeans(HTConsM), na.rm = T)
      D$snt_NT_min_avgWeek <- min(rowMeans(NTConsM), na.rm=T)
      D$snt_HT_max_avgWeek <- max(rowMeans(HTConsM), na.rm = T)
      D$snt_NT_max_avgWeek <- max(rowMeans(NTConsM), na.rm=T)
    })
  }

  D$rnt_HT_max_mean_total <- D$snt_HT_max_total / D$cnt_HT
  D$rnt_NT_max_mean_total <- D$snt_NT_max_total / D$cnt_NT

  D$rnt_HT_min_mean_total <- D$snt_HT_min_total / D$cnt_HT
  D$rnt_NT_min_mean_total <- D$snt_NT_min_total / D$cnt_NT

  #the variance
  suppressWarnings({ #warnings when SD is zero
    D$snt_NT_variance <- var(as.vector(NTCons), na.rm = T)
    D$snt_NT_var_wd <- var(as.vector(NTConsM[1:5,]), na.rm = T)

    D$snt_HT_variance <- var(as.vector(HTCons), na.rm = T)
    D$snt_HT_var_wd <- var(as.vector(HTConsM[1:5,]), na.rm = T)
  })

  # #temporal properties
  # D$tsnt_HT_var_wd <- var(HTCons[1:5], na.rm = T)
  #time properties
  D$tnt_NT_below_1kWh <- sum(NTCons<1)
  D$tnt_NT_below_2kWh <- sum(NTCons<2)
  D$tnt_NT_below_5kWh <- sum(NTCons<5)
  D$tnt_NT_above_mean <- sum(NTCons>D$cnt_NT)
  D$tnt_NT_num_zeros <- sum(NTCons==0)

  D$tnt_HT_below_1kWh <- sum(HTCons<1)
  D$tnt_HT_below_2kWh <- sum(HTCons<2)
  D$tnt_HT_below_5kWh <- sum(HTCons<5)
  D$tnt_HT_above_mean <- sum(HTCons>D$cnt_HT)
  D$tnt_HT_num_zeros <- sum(HTCons==0)


  return(D)
}
