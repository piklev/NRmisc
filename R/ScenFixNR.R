#' Sets future/scenario values to a fixed historical value
#' 
#' @author Niklas Roming
#' @param data A NRmisc compatible data frame.
#' @param myitem Variable for which data should be calculated.
#' @param history Period of which value will be used for future values.
#' @param scenario Periods for which historical value will be used.
#' @export

ScenFixNR <- function(data, myitem, history, scenario){
  tmp <- subset(data, item == myitem & year %in% history)
  #   tmp[tmp$value == 0, "value"] <- NA
  tmp <- tmp[is.finite(tmp$value), ] # this should substitute the 2 lines above and below, right?
  #   tmp <- tmp[complete.cases(tmp$value), ]
  tmp$model <- "EDGE"
  tmp1 <- data.frame()
  for (s in ssp){
    for (t in scenario){
      tmp$year <- t
      tmp$scenario <- s
      tmp1 <- rbind(tmp1, tmp)
    }
  }
  result <- rbind(data, tmp1)
  return(result)
}