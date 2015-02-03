#' Interpolates (linearly) values for gaps in time-series
#' 
#' @author Niklas Roming
#' @param data A NRmisc compatible data frame
#' @param myitem Variable for which interpolation shall be carried out.
#' @return A NRmisc compatible data frame without the gaps in the respective
#' time-series
#' @export
InterpolateNR <- function(data, myitem,zeroAsNA=TRUE){
  tmp <- subset(data, item == myitem)
  if (zeroAsNA) {
  tmp[tmp == 0] <- NA
  }
  
  # remove rows containing zero from the original data frame
  # the gaps remaining will be filled using interpolated data
  data[data$value == 0 & data$item == myitem, "value"] <- NA
  data <- data[!is.na(data$value), ]
  
  #   data.n <- data.frame()
  
  for(c in unique(tmp$iso3c)){
    
    tmp.c <- subset(tmp, iso3c == c)
    
    # test if there are only NAs in the dataset
    if(length(unique(tmp.c$value)) > 1){
      
      # find non missing years
      non.missing <- tmp.c[!is.na(tmp.c$value), "year"]
      
      # drop values before the first and after the last non-missing year
      tmp.c <- subset(tmp.c, year >= min(non.missing) & year <= max(non.missing))
      
      # determine gaps
      gaps <- tmp.c[is.na(tmp.c$value), "year"]
      
      # only interpolate if there are any gaps
      if(length(gaps) > 0){
        cat("Interpolation", myitem, c, gaps, "\n")
        
        zoo.c <- zoo(subset(tmp.c, select=value), order.by=tmp.c$year)
        
        zoo.c <- na.approx(zoo.c)
        
        tmp.c.n <- as.data.frame(zoo.c)
        tmp.c.n$year <- as.integer(rownames(tmp.c.n))
        
        # only keep the filled gaps
        tmp.c.n <- subset(tmp.c.n, year %in% gaps)
        
        tmp.c.n$iso3c <- c
        #         tmp.c.n$comment <- "interpolated"
        tmp.c.n$model <- "EDGE"
        tmp.c.n$scenario <- "history"
        tmp.c.n$item <- unique(tmp$item)
        tmp.c.n$unit <- unique(tmp$unit)[1]
      }
    }
    if(exists("tmp.c.n")){
      #       data.n <- rbind(data.n, tmp.c.n)
      data <- rbind(data, tmp.c.n)
      rm(tmp.c.n)
    }
    
    #   data <- rbind(data, data.n)
  }
  
  
  return(data)
}