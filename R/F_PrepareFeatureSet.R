#' Compiles a list of features from energy consumption data
#'
#' Returns a vector of feature names that can be calculated by methods in the
#' *SmartMeterAnalytics* package obtains the feature set according
#'
#' @param features.granularity
#'          Character: The granularity of the input data, either "15-min" (only 15-min features), "30-min" (only 30-minute features),
#'          "all_30min_to_week" (all features on daily, weekly, hourly, ..., up to 30-min data), "all_15_week" (all up to 15-min dara),
#'          "week" (only the consumption of one week as a single feature).
#' @param features.w_adj
#'          Boolean: are the features to be weather adjusted with DiD-Class
#'          (NOT IMPLEMENTED YET!)
#' @param features.anonymized
#'          Boolean: are anonymized geographic features used
#'          (NOT IMPLEMENTED YET!)
#' @param features.categorical
#'          Boolean: use categorical features additionally (if only numeric
#'          features are used)
#' @param features.geo
#'          Character: Version of the geographic feature set (either "none", "osm-v1", "osm-v2")
#' @param features.temperature
#'          Boolean, if features for the temperature should be included
#' @param features.weather
#'          Boolean, if other weather features should be included
#' @param features.neighborhood
#'          Boolean, if features for the neighborhood should be included
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @references Hopf, K. (2019). Predictive Analytics for Energy Efficiency and
#'     Energy Retailing (1st ed.). Bamberg: University of Bamberg.
#'     \url{https://doi.org/10.20378/irbo-54833}
#' @references Hopf, K., Sodenkamp, M., Kozlovskiy, I., & Staake, T. (2014).
#'     Feature extraction and filtering for household classification based on
#'     smart electricity meter data. Computer Science-Research and Development,
#'     (31) 3, 141–148. \url{https://doi.org/10.1007/s00450-014-0294-4}
#' @references Hopf, K., Sodenkamp, M., & Staake, T. (2018). Enhancing energy
#'     efficiency in the residential sector with smart meter data analytics.
#'     Electronic Markets, 28(4). \url{https://doi.org/10.1007/s12525-018-0290-9}
#' @references Beckel, C., Sadamori, L., Staake, T., & Santini, S. (2014).
#'     Revealing household characteristics from smart meter data. Energy, 78,
#'     397–410. \url{https://doi.org/10.1016/j.energy.2014.10.025}

#' @return Character vector
#' @export

prepareFeatureSet <- function(features.granularity=NA,  #15-min / 30-min / all_30min_to_week / all_15_week / week
                              features.w_adj=FALSE,         #weather-adjusted (TRUE/FALSE)
                              features.anonymized=FALSE,    #anonymized (TRUE/FALSE)
                              features.categorical=FALSE,   #are cateorical features included
                              features.geo="osm-v1",    #name of the geographic feature set used
                              features.temperature=TRUE,   #are temperature-features included
                              features.weather=TRUE,       #are weather features (except temperature included)
                              features.neighborhood = FALSE #neighborhood features
                              ){

  fset.smd_week <- c("consumption")
  fset.smd_daynt <- c("cnt_week","cnt_weekday","cnt_weekend","cnt_NT","cnt_HT","cnt_we_NT","cnt_wd_NT","rnt_NT_wd_we","cnt_we_HT","cnt_wd_HT",
                      "rnt_HT_wd_we","rnt_NT_HT","rnt_we_NT_HT","rnt_wd_NT_HT","snt_HT_min","snt_NT_min","snt_HT_max","snt_NT_max","rnt_HT_mean_max",
                      "rnt_NT_mean_max","rnt_HT_mean_min","rnt_NT_mean_min","snt_NT_variance","snt_NT_var_wd","snt_HT_variance","snt_HT_var_wd")
  fset.smd_day <- c("cda_week","cda_weekday","cda_weekend","rda_we_wd","sda_min","sda_max","rda_mean_max","rda_mean_min","sda_variance")
  fset.smd60 <- c("c60_week","c60_weekday","c60_weekend","c60_night","c60_morning","c60_noon","c60_afternoon","c60_evening","c60_we_night",
                  "c60_wd_night","r60_night_wd_we","c60_we_morning","c60_wd_morning","r60_morning_wd_we","c60_we_noon","c60_wd_noon",
                  "r60_noon_wd_we","c60_we_afternoon","c60_wd_afternoon","r60_afternoon_wd_we","c60_we_evening","c60_wd_evening","r60_evening_wd_we",
                  "r60_night_day","r60_we_night_day","r60_wd_night_day","r60_morning_noon","r60_we_morning_noon","r60_wd_morning_noon","r60_evening_noon",
                  "r60_we_evening_noon","r60_wd_evening_noon","s60_min","s60_max","r60_mean_max","r60_min_mean","s60_we_max","s60_we_min","s60_wd_max",
                  "s60_wd_min","r60_min_wd_we","r60_max_wd_we","s60_q1","s60_q2","s60_q3","s60_min_avg","s60_max_avg","s60_variance","s60_var_we","s60_var_wd",
                  "r60_var_wd_we","s60_cor","s60_cor_we","s60_cor_wd","s60_cor_wd_we","s60_sm_variety","s60_bg_variety","s60_sm_max","s60_number_zeros",
                  "c60_evening_no_min","c60_morning_no_min","c60_night_no_min","c60_noon_no_min","r60_mean_max_no_min","r60_evening_noon_no_min",
                  "r60_morning_noon_no_min","r60_day_night_no_min","t60_above_1kw","t60_above_2kw","t60_above_mean","t60_daily_max","t60_daily_min",
                  "s60_num_peaks","s60_diff","ts60_acf_mean3h","ts60_acf_mean3h_weekday","ts60_stl_varRem","t60_above_base","s60_day_diff",
                  "s60_day_diff_weak","t60_wide_peaks","t60_width_peaks","t60_time_above_base2","t60_percent_above_base","t60_value_above_base",
                  "t60_const_time","t60_value_min_guess","t60_first_above_base","s60_num_big_peaks","t60_number_small_peaks","t60_dist_big_v")
  # 30-minute SMD features
  fset.smd30 <- c("c30_week", "c30_weekday", "c30_weekend", "c30_night", "c30_morning",
                  "c30_noon", "c30_afternoon", "c30_evening", "c30_we_night", "c30_wd_night",
                  "r30_night_wd_we", "c30_we_morning", "c30_wd_morning", "r30_morning_wd_we",
                  "c30_we_noon", "c30_wd_noon", "r30_noon_wd_we", "c30_we_afternoon",
                  "c30_wd_afternoon", "r30_afternoon_wd_we", "c30_we_evening", "c30_wd_evening",
                  "r30_evening_wd_we", "r30_night_day", "r30_we_night_day", "r30_wd_night_day",
                  "r30_morning_noon", "r30_we_morning_noon", "r30_wd_morning_noon",
                  "r30_evening_noon", "r30_we_evening_noon", "r30_wd_evening_noon",
                  "s30_min", "s30_max", "r30_mean_max", "r30_min_mean", "s30_we_max",
                  "s30_we_min", "s30_wd_max", "s30_wd_min", "r30_min_wd_we",
                  "r30_max_wd_we", "s30_q1", "s30_q2", "s30_q3", "s30_min_avg",
                  "s30_max_avg", "s30_variance", "s30_var_we", "s30_var_wd", "r30_var_wd_we",
                  "s30_cor", "s30_cor_we", "s30_cor_wd", "s30_cor_wd_we", "s30_sm_variety",
                  "s30_bg_variety", "s30_sm_max", "s30_number_zeros", "c30_evening_no_min",
                  "c30_morning_no_min", "c30_night_no_min", "c30_noon_no_min", "r30_mean_max_no_min",
                  "r30_evening_noon_no_min", "r30_morning_noon_no_min", "r30_day_night_no_min",
                  "t30_above_1kw", "t30_above_2kw", "t30_above_mean", "t30_daily_max",
                  "t30_daily_min", "s30_num_peaks", "s30_diff", "ts30_acf_mean3h", "ts30_acf_mean3h_weekday",
                  "ts30_stl_varRem", "t30_above_base", "s30_day_diff", "s30_day_diff_weak", "t30_wide_peaks",
                  "t30_width_peaks", "t30_time_above_base2", "t30_percent_above_base", "t30_value_above_base",
                  "t30_const_time", "t30_value_min_guess", "t30_first_above_base", "s30_num_big_peaks",
                  "t30_number_small_peaks", "t30_dist_big_v")

  #15-minute SMD features (only additional ones)
  fset.smd15 <- c(#consumption
    "c15_week", "s15_min", "s15_max",
    "c15_evening_no_min", "c15_morning_no_min", "c15_night_no_min", "c15_noon_no_min",
    "c15_afternoon_no_min",

    #relations
    "r15_mean_max", "r15_min_mean", "s15_we_max",
    "r15_min_wd_we", "r15_max_wd_we",
    "r15_mean_max_no_min", "r15_evening_noon_no_min", "r15_morning_noon_no_min",
    "r15_day_night_no_min", "r15_var_wd_we",

    #time
    "t15_above_0.5kw", "t15_above_1kw", "t15_above_2kw", "t15_above_mean",
    "t15_daily_max", "t15_daily_min",
    "ts15_acf_mean3h", "ts15_acf_mean3h_weekday", "ts15_stl_varRem",
    "t15_wide_peaks", "t15_width_peaks", "t15_above_base", "t15_time_above_base2",
    "t15_percent_above_base", "t15_value_above_base", "t15_const_time", "t15_value_min_guess",
    "t15_first_above_base", "t15_number_small_peaks", "t15_dist_big_v",

    #statistical
    "s15_num_big_peaks", "s15_day_diff_weak", "s15_num_peaks", "s15_diff",   "s15_day_diff",
    "s15_we_min", "s15_we_max", "s15_wd_max", "s15_wd_min",
    "s15_q1", "s15_q2", "s15_q3", "s15_min_avg", "s15_max_avg", "s15_variance",
    "s15_var_we", "s15_var_wd",  "s15_cor", "s15_cor_we", "s15_cor_wd",
    "s15_cor_wd_we", "s15_sm_variety", "s15_bg_variety", "s15_sm_max", "s15_number_zeros")

  #15-min with 30-min features (combined feature set with 93 features)
  fset.smdComb <- c(#consumption
    "c30_week", "c30_weekday", "c30_weekend", "c30_night", "c30_morning",
    "c30_noon", "c30_afternoon", "c30_evening", "c30_we_night", "c30_wd_night",
    "c30_we_morning", "c30_wd_morning",
    "c30_we_noon", "c30_wd_noon", "c30_we_afternoon",
    "c30_wd_afternoon",  "c30_we_evening", "c30_wd_evening",
    "c15_evening_no_min", "c15_morning_no_min", "c15_night_no_min", "c15_noon_no_min",
    "c15_afternoon_no_min",

    #relations
    "r30_night_wd_we","r30_morning_wd_we", "r30_noon_wd_we", "r30_afternoon_wd_we",
    "r30_evening_wd_we", "r30_night_day", "r30_we_night_day", "r30_wd_night_day",
    "r30_morning_noon", "r30_we_morning_noon", "r30_wd_morning_noon",
    "r30_evening_noon", "r30_we_evening_noon", "r30_wd_evening_noon",
    "r15_mean_max", "r15_min_mean",
    "r15_min_wd_we", "r15_max_wd_we",
    "r15_mean_max_no_min", "r15_evening_noon_no_min", "r15_morning_noon_no_min",
    "r15_day_night_no_min", "r15_var_wd_we",

    #time
    "t15_above_0.5kw", "t15_above_1kw", "t15_above_2kw", "t15_above_mean",
    "t15_daily_max", "t15_daily_min",
    "ts15_acf_mean3h", "ts15_acf_mean3h_weekday", "ts15_stl_varRem",
    "t15_wide_peaks", "t15_width_peaks", "t15_above_base", "t15_time_above_base2",
    "t15_percent_above_base", "t15_value_above_base", "t15_const_time", "t15_value_min_guess",
    "t15_first_above_base", "t15_number_small_peaks", "t15_dist_big_v",

    #statistical
    "s15_min", "s15_max",
    "s15_num_big_peaks", "s15_day_diff_weak", "s15_num_peaks", "s15_diff",   "s15_day_diff",
    "s15_we_min", "s15_we_max", "s15_wd_max", "s15_wd_min",
    "s15_q1", "s15_q2", "s15_q3", "s15_min_avg", "s15_max_avg", "s15_variance",
    "s15_var_we", "s15_var_wd",  "s15_cor", "s15_cor_we", "s15_cor_wd",
    "s15_cor_wd_we", "s15_sm_variety", "s15_bg_variety", "s15_sm_max", "s15_number_zeros")

  fset.geoExtNum <- c("lat", "lng", "citydist1000", "citydist5000", "citydist15000", "elevation", "ele.resolution")


  fset.geoOSMNum_v1 <- c("numNodes", "numNodeTags", "numWays", "numWayTags", "numRelations", "numRelationTags",
                         "buildingarea.mean", "buildingarea.median", "buildingarea.var", "nextbuildingarea",
                         "nextbuildingsdistMean", "nextbuildingsdistVar", "building.distMean", "building.distVar",
                         "mindist.publicInstitutions", "mindist.business", "mindist.food", "mindist.transportation",
                         "mindist.recreation", "mindist.culture", "mindist.sights", "mindist.countryside",
                         "mindist.roadSystem",
                         #wcount is redundant with meandist!
                         #"wcount.publicInstitutions", "wcount.business", "wcount.food",
                         #"wcount.transportation", "wcount.recreation", "wcount.culture", "wcount.sights", "wcount.countryside", "wcount.roadSystem",
                         "meandist.publicInstitutions", "meandist.business", "meandist.food",
                         "meandist.transportation", "meandist.recreation", "meandist.culture", "meandist.sights",
                         "meandist.countryside", "meandist.roadSystem", "totalarea.apartments", "totalarea.singlefamily",
                         "totalarea.nonresidential", "totalarea.notspecified", "totalarea.countryside",
                         "totalarea.residential", "totalarea.city")

  fset.geoOSMNum_v2 <- c("numNodes", "numNodeTags", "numWays",
                         "numWayTags", "numRelations", "numRelationTags", "buildingarea.mean",
                         "buildingarea.median", "buildingarea.var", "nextbuildingarea",
                         "nextbuildingsdistMean", "nextbuildingsdistVar", "buidling.distMean",
                         "buidling.distVar", "num.buildings", "num.publicInstitutions",
                         "num.business", "num.food", "num.transportation", "num.recreation",
                         "num.culture", "num.sights", "num.countryside", "num.roadSystem",
                         "wcount.buildings", "wcount.residentialBuildings", "wcount.publicInstitutions",
                         "wcount.business", "wcount.food", "wcount.transportation", "wcount.recreation",
                         "wcount.culture", "wcount.sights", "wcount.countryside", "wcount.roadSystem",
                         "mindist.publicInstitutions", "mindist.business", "mindist.food",
                         "mindist.transportation", "mindist.recreation", "mindist.culture",
                         "mindist.sights", "mindist.countryside", "mindist.roadSystem",
                         "meandist.publicInstitutions", "meandist.business", "meandist.food",
                         "meandist.transportation", "meandist.recreation", "meandist.culture",
                         "meandist.sights", "meandist.countryside", "meandist.roadSystem",
                         "totalarea.apartments",
                         "totalarea.singlefamily", "totalarea.nonresidential", "totalarea.notspecified",
                         "totalarea.countryside",
                         "totalarea.residential", "totalarea.city")

  fset.geoOSMCat <- c("thisbuildingType", "nextbuildingType", "buildingTypeMode", "thislanduseType", "nextlanduseType")

  fset.temp <- c("w_temp_cor_overall", "w_temp_cor_daily", "w_temp_cor_night",
                 "w_temp_cor_daytime", "w_temp_cor_evening", "w_temp_cor_minima",
                 "w_temp_cor_maxmin", "w_temp_cor_weekday_weekend")

  fset.other_weather <- c("w_airPr_cor_overall",
                          "w_airPr_cor_daily", "w_airPr_cor_night", "w_airPr_cor_daytime",
                          "w_airPr_cor_evening", "w_airPr_cor_minima", "w_airPr_cor_maxmin",
                          "w_airPr_cor_weekday_weekend",
                          "w_windSp_cor_overall", "w_windSp_cor_daily",
                          "w_windSp_cor_night", "w_windSp_cor_daytime", "w_windSp_cor_evening",
                          "w_windSp_cor_minima", "w_windSp_cor_maxmin", "w_windSp_cor_weekday_weekend",
                          "w_prec_cor_overall", "w_prec_cor_daily", "w_prec_cor_night",
                          "w_prec_cor_daytime", "w_prec_cor_evening", "w_prec_cor_minima",
                          "w_prec_cor_maxmin", "w_prec_cor_weekday_weekend",
                          "w_skyc_cor_overall",
                          "w_skyc_cor_daily", "w_skyc_cor_night", "w_skyc_cor_daytime",
                          "w_skyc_cor_evening", "w_skyc_cor_minima", "w_skyc_cor_maxmin",
                          "w_skyc_cor_weekday_weekend")

  fset.neigborhood <- c("nn10_avgDist", "nn_numNBs50m", "nn_numNBs250m", "nn_numNBs500m",
                        "nn_numNBs1000m", "nn10_cons_relDiff", "nn10_max_relDiff", "nn10_corDays_absDiff",
                        "nn10_numPeaks_relDiff", "nn10_numAboveMean_relDiff", "nn10_consNoon_wd_wd_absDiff",
                        "nn10_meanCor", "nn10_meanCor_wd", "nn10_meanCor_we")

  featureset <- c()

  if(features.granularity=="none"){
    # add nothing
  } else if(features.granularity=="15-min"){    #add the 15-min / 30-min combined features
    featureset <- c(featureset, fset.smdComb)
  } else if(features.granularity=="30-min"){    #add the 30-min features
    featureset <- c(featureset, fset.smd30)
  } else if(features.granularity=="all_30min_to_week"){    #add features for all granularities (30-min ... week)
    featureset <- c(featureset, fset.smd30, fset.smd60, fset.smd_day, fset.smd_daynt, fset.smd_week)
  } else if (features.granularity =="all_15_week") {
    featureset <- c(featureset, fset.smd15, fset.smd30, fset.smd60, fset.smd_day, fset.smd_daynt, fset.smd_week)
  } else if (features.granularity =="week") {
    featureset <- c(fset.smd_week)
  } else {
    stop("Method not implemented for this granularity")
  }

  if(features.w_adj){
    #TODO implement
    stop("Weather adjustment not implemented yet!")
  }

  if(features.anonymized){
    #TODO implement
    stop("Anonimized features not implemented yet!")
  }

  #add temperature-features
  if(features.temperature){
    featureset <- c(featureset, fset.temp)
  }

  #add weather features other than temperature
  if(features.weather){
    featureset <- c(featureset, fset.other_weather)
  }

  #add geographic features
  if(features.geo!="none"){
    if(features.categorical){
      featureset <- c(featureset, fset.geoOSMCat)
    }
    if(features.geo=="osm-v1"){
      featureset <- c(featureset, fset.geoOSMNum_v1)
    } else if(features.geo=="osm-v2"){
      featureset <- c(featureset, fset.geoOSMNum_v2)
    }
  }

  #add the neigborhood features
  if(features.neighborhood){
    featureset <- c(featureset, fset.neigborhood)
  }

  return(featureset)
}

