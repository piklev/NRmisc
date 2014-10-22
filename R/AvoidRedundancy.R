#' Cleans data to avoid redundancy
#' 
#' Checks if a variable is already present in a dataset and removes it. This is
#' useful if you use a function like ProductNR or RatioNR, where often a
#' certain variable is available for historical, but not for scenario data
#' @author Niklas Roming
#' 
#' @param x A data frame.
#' @param newitem Variable for which to check
#' 
#' @export
#' 
#' @return A dataframe without data on newitem

AvoidRedundancy <- function(x, newitem){
  t1 <- unique(x[x$item == newitem, "year"]) # for which years 'newitem' is already present
  t2 <- unique(x$year) # determine all timesteps
  t3 <- setdiff(t2,t1) # for which years 'newitem' is not defined?
  y <- subset(x, year %in% t3)
  return(y)
}