#' Reads a REMIND csv, separated by semicolon
#' 
#' @author Niklas roming
#' @param path Path to csv
#' @return A NRmisc compatible data frame
#' @export

read_remind_csv <- function(path){
  require(reshape2)
  # read in the csv
  tmp1 <- read.csv2(path, strip.white=TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  
  #remove last column, if empty:
  if(names(tmp1)[[length(tmp1)]] == "") tmp1 <- tmp1[,-length(tmp1)]
  
  # convert the columns with numbers to actual numbers
  tmp1[,c(6:24)] <- sapply(tmp1[,c(6:24)], as.numeric)
  
  # convert to a long dataframe
  tmp2 = melt(tmp1,id.vars=c("Model", "Scenario", "Region", "Variable", "Unit"))
  names(tmp2)[names(tmp2) == "variable"] <- "year"
  tmp2$year <- as.integer(as.character(tmp2$year)) # convert from factor to integer
  names(tmp2) <- tolower(names(tmp2))
  names(tmp2)[names(tmp2) == "variable"] <- "item"
  
  return(tmp2)
}