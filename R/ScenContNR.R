#' Continues historical growth for several scnearios
#' 
#' @author Niklas Roming
#' @param data NRmisc compatible data frame.
#' @param myitem Used variable.
#' @param scen.time Scenario periods.
#' @param scenarios Scenarios for which to apply the function.
#' @export

ScenContNR <- function(data, myitem, scen.time, scenarios){
  #  tmp <- subset(data, item == myitem & year %in% hist.time)
  tmp <- subset(data, item == myitem & scenario == "history")
  tmp <- subset(tmp, value != 0)
  tmp <- tmp[is.finite(tmp$value), ]
  
  # split data frane by iso code or region
  if("iso3c" %in% colnames(tmp)){
    country.list <- split(tmp, tmp$iso3c)
  } else {
    country.list <- split(tmp, tmp$region)
  }
  country.list.n <- lapply(country.list, ContinueGrowthNR)
  tmp1 <- do.call(rbind, country.list.n)
  tmp1$scenario <- NA
  dummy.df <- data.frame()
  for (s in scenarios){
    tmp2 <- tmp1
    tmp2$scenario <- s
    dummy.df <- rbind(dummy.df, tmp2)
  }
  result <- rbind(data, dummy.df)
  return(result)
}