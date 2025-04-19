#' Determines two clusters of high and low consumption times (e.g., non-ocupancy during holidays)
#'
#' @param consumption the consumption time series
#' @param n_days_check number of consecutive days that should be considered as a minimal cluster
#' @param sds_between_clusters the multiples of standatd deviation that must be at least between the cluster centers (decimal number)
#'
#' @return list with cluster assignments and the k-Means clustering model
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}
#' @importFrom stats kmeans sd
#' @export
occupancy_cluster <- function(consumption, n_days_check = 4, sds_between_clusters=1.5){
  kmr <- kmeans(consumption, centers = 2)
  c_memership <- kmr$cluster
  cluster_lowCons <- which.min(kmr$centers)
  cluster_highCons <- which.max(kmr$centers)

  #S <- t.test(consumption[c_memership==cluster_lowCons], consumption[c_memership==cluster_highCons])
  #if(S$p.value<0.05){
  cons_low <- consumption[c_memership==cluster_lowCons]
  cons_high <- consumption[c_memership==cluster_highCons]

  #if(abs(diff(kmr$centers)) > sds_between_clusters*sd(consumption)){
  if(mean(cons_low)+sds_between_clusters*sd(cons_low) < mean(cons_high)-sds_between_clusters*sd(cons_high)){
    #when the current day is identified as non-occupancy, check the following *n_days_check* days
    for(i in which(c_memership==cluster_lowCons)){
      if(i>1 & i < length(c_memership)-n_days_check){
        # change cluster to occupancy when the next *n_days_check* have occupancy,
        # but not when the previous days have been occupancy;
        # assume that the first day was correctly classified as (non-)occupancy
        if(any(c_memership[i+c(1:n_days_check)] == cluster_highCons)){
          if(c_memership[i-1] == cluster_highCons){
            c_memership[i] <- cluster_highCons
          }
        }
      }
    }
    #when the first element is different from the following, change the cluster membership,
    #since the following element was checked
    if(c_memership[1]!=c_memership[2]){
      c_memership[1] <- c_memership[2]
    }

    #handle the last three values
    # when all *n_days_check* have no occupancy -> skip
    if(!all(c_memership[(length(c_memership)-n_days_check-1):(length(c_memership))]==cluster_lowCons)){
      #case 1: the non-occupancy proceeds from earlier
      last_day_no_occupancy <- c_memership[(length(c_memership)-n_days_check-1)] == cluster_lowCons
      for(j in (length(c_memership)-n_days_check-1):(length(c_memership))){
        #leave it, when the previous one was non-occupancy
        if(c_memership[j]==cluster_lowCons & last_day_no_occupancy) next
        #otherwise change all future ones
        c_memership[j] <- cluster_highCons
        last_day_no_occupancy <- FALSE
      }
    }
  } else {
    c_memership <- rep(cluster_highCons, times=length(c_memership))
  }

  if(sum(c_memership==cluster_lowCons) >  sum(c_memership==cluster_highCons)){
    c_memership <- rep(cluster_highCons, times=length(c_memership))
  }

  c_memership_factor <- factor(ifelse(c_memership==cluster_highCons, "occupancy", "non_occupancy"), levels=c("occupancy", "non_occupancy"))

  return(list(cluster_highConss = c_memership_factor,
              model = kmr))
}
