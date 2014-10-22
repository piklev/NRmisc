#' Lets historical data grow
#' 
#' @author Niklas Roming
#' @param data NRmis compatible data frame
#' @param myitem Item/variable for which calculations will be made.
#' @param hist.time Historical periods to take into account (e.g. for
#' calculation of growth rates)
#' @param scen.time Scenario perios to take into account (for which new values
#' will be calculated)
#' @param fade.time Number of periods over which to fade out historical growth
#' rates
#' @export

GrowthNR <- function(data, myitem, hist.time, scen.time, fade.time){
  tmp <- subset(data, item %in% myitem & year %in% hist.time)
  tmp.list <- split(tmp, tmp[ , c("scenario", "iso3c")])
  if(hasArg("fade.time")){
    result.list <- lapply(tmp.list, FadeOutGrowthNR, fade.time=fade.time)
  } else {
    result.list <- lapply(tmp.list, ContinueGrowthNR)
  }
  result <- do.call("rbind", result.list)
  result <- result[result$year != 2100, ] # remove 2100 (already in data)
  data <- rbind(data, result)
  return(data)
}