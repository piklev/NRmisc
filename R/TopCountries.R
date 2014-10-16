#' Identifiers of largest spatial units 
#'
#' Returns identifiers of the n spatial units that have highest values of a
#' certain variable in a certain year
#' 
#' @author Niklas Roming
#'
#' @param x A data frame.
#' @param var The variable or item for which highest realizations shall be taken into account.
#' @param t The year for which the maximum values shall be returned.
#' @param n The number of spatial units to be returned.
#' @return A character vector (of length n).
#' @export

TopCountries<- function(x, var, t, n=10){  
  df <- subset(x, item == var & year == t)
  df <- df[order(df$value, decreasing=TRUE), ]
  return(df[1:n, "iso3c"])
}