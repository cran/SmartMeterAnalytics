#' Retrieves the date of the monday in a ISO8601 week-string
#'
#' Example date formats defined by ISO 8601:
#'  * Single days are written in yyy-mm-dd (y: year, m: month, d: day); e.g., 2016-07-19
#'  * Weeks are written in yyyy-Www; e.g., 2016-W29
#'
#'  The function uses format und as.Date internally and can therefore not handle ISO8601
#'  week formats. Therefore, a workaround is implemented that can lead to suspicious
#'  behavior in future versions
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

getDay_ISO8601_week <- function(theweek, day=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")){
  daynum <- which(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")==day[1])
  theweek <- as.character(theweek)

  if(any(!grepl("[[:digit:]]{4}\\-W[[:digit:]]{1,2}",theweek))){
    stop("Wrong date format!")
  }

  if(length(daynum)==0){
    stop("Wrong day value!")
  }

  #handle case of week 53
  ifelse(substr(theweek, 7,8)=="53",TRUE,FALSE)

  #use the US format, since option %V is not accepted, replace week 53 with week 52
  monday_US <- as.Date(ifelse(substr(theweek, 7,8)!="53",
                              as.character(as.Date(paste0(theweek,"-1"), format="%Y-W%U-%w")),
                              as.character(as.Date(paste0(substr(theweek,0,6),"52-1"), format="%Y-W%U-%w"))))

  #convert the dates back to week format and compute the difference in weeks
  ISOweek_ofMondayUS <- format(monday_US, format="%Y-W%V")
  weekdiff <- as.integer(substr(theweek, 7,8))-as.integer(substr(ISOweek_ofMondayUS, 7,8))
  monday_ISO <- monday_US + weekdiff*7
  # ISOweek_ofMondayISO <- format(monday_ISO, format="%Y-W%V")
  # weekdiff2 <- as.integer(substr(theweek, 7,8))-as.integer(substr(ISOweek_ofMondayISO, 7,8))

  if(daynum==1){
    return(monday_ISO)
  } else {
    return(monday_ISO+(daynum-1))
  }
}
