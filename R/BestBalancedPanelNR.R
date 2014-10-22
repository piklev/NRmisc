#' Balanced panel with the highest number of observations
#' 
#' @author Niklas Roming
#' @param data NRmisc compatible data frame.
#' @param myitem Variable for which to find the best balanced panel.
#' @return NRmisc compatible data frame containing the best balanced panel.
#' @export
BestBalancedPanelNR <- function(data, myitem){
  # Finds the balanced panel with highest number of observations
  # 
  # Args:
  #   data: a dataframe in the standard EDGE layout
  #   myitem: the item from
  #           the data.frame for which the best balanced panel is to be found
  #
  # Returns:
  #   a data.frame in EDGE layout containing balanced data
  
  require(reshape2)
  
  tmp <- subset(data, item == myitem & scenario == "history")
  
  # preallocate data.frame
  value <- data.frame(year = unique(tmp$year), n = NA)
  
  for(y in unique(tmp$year)){
    tmp1 <- subset(tmp, year >= y)
    tmp2 <- dcast(tmp1, year ~ iso3c)
    for(c in unique(tmp1$iso3c)){
      if(anyNA(tmp2[, c])){
        tmp2[, c] <- NULL
      }      
    }
    
    value[value$year == y, "n"] <- dim(tmp2)[1] * (dim(tmp2)[2] - 1)  
  }
  
  theYear <- value[which.max(value$n),"year"]
  
  tmp3 <- subset(tmp, year >= theYear)
  tmp3 <- dcast(tmp3, year ~ iso3c)
  for(c in unique(tmp$iso3c)){
    if(anyNA(tmp3[, c])){
      tmp3[, c] <- NULL
    }      
  }
  
  tmp4 <- melt(tmp3, id.vars="year", factorsAsStrings = FALSE)
  names(tmp4) <- c("year", "iso3c", "value")
  
  return(tmp4)
}