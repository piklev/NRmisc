#' Computes average growth rate of a time series
#' 
#' @author Niklas Roming
#' @param x Data frame containing a column 'year', unique in 'item' and 'iso3c'
#' @return Average growth rate of 'value' column in the dataframe
#' @export
GetGrowthRateNR <- function(x){
  #remove zero values
  x <- subset(x, value != 0)
  
  # order the data frame by year
  x <- x[order(x$year),]
  first.year <- min(x$year)
  last.year  <- max(x$year)
  gr <- (x[x$year == last.year, "value"] - x[x$year == first.year, "value"]) / x[x$year == first.year, "value"] / (last.year - first.year)
  return(gr)
}