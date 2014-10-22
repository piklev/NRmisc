#' Return n-smallest value of a vector of numbers
#' 
#' @author Niklas Roming
#' @param x A vector (numeric or integer)
#' @param n Which value to return (e.g. n=2 => second-smallest)
#' @return The n-smallest value of x
#' @export
SmallNR <- function(x, n){
  # this function returns the second smallest value of a vector 
  # http://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-
  # highest-lowest-value-in-vector-or-column
  y <- unlist(sort(x)[n])
  return(y)
}