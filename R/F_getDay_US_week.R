#' Retrieves the date of the monday in a US week-string (as implemented by R as.Date)
#'
#' According to date formats defined by ISO 8601:
#'  * Single days are written in yyy-mm-dd (y: year, m: month, d: day); e.g., 2016-07-19
#'  * Weeks are written in yyyy-WUww; e.g., 2016-WU29 (typically with the first Sunday of the year as day 1 of week 1)
#'
#' @param theweek
#'  the string with the week name
#' @param day
#'  the weekday that shall be returned
#'
#' @return
#'  the date of the weekday in the given week
#'
#' @author Konstantin Hopf \email{konstantin.hopf@uni-bamberg.de}

getDay_US_week <- function(theweek, day=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")){
  daynum <- which(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")==day[1])
  theweek <- as.character(theweek)

  if(any(!grepl("[[:digit:]]{4}\\-WU[[:digit:]]{1,2}",theweek))){
    stop("Wrong date format!")
  }

  if(length(daynum)==0){
    stop("Wrong day value!")
  }

  #use the US format, since option %V is not accepted, replace week 53 with week 52
  monday_US <- as.Date(ifelse(substr(theweek, 8,9)!="00",
                              as.character(as.Date(paste0(theweek,"-1"), format="%Y-WU%U-%w")),
                              as.character(as.Date(paste0(substr(theweek,0,7),"01-1"), format="%Y-WU%U-%w")+7)))

  if(daynum==1){
    return(monday_US)
  } else {
    return(monday_US+(daynum-1))
  }
}
