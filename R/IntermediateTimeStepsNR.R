#' Computes values for intermediate time steps by linear interpolation
#' 
#' @author Niklas Roming
#' @param data NRmisc compatible data frame
#' @param myitem Variable for which intermediate time steps are to be computed
#' @param ts.lenght Target time step length
#' @param hist.scen Scenario name of historical data.
#' @param temp.hist Years of historical data (last time step of historical data
#' is needed for computing values between historical data and first scenario
#' data)
#' @return NRmisc data frame with the new time steps attached to the original
#' data
#' @export
#' 
IntermediateTimeStepsNR <- function(data, myitem, ts.length= 5, hist.scen,
                                        temp.hist){
  
  tmp <- subset(data, item == myitem & year >= max(temp.hist))
  
  # find out which years are missing from data
  years.miss <- setdiff(
    seq(min(tmp$year), max(tmp$year), ts.length), # needed years
    unique(tmp$year) # available years
  )
  
  # if there are historical values (probably in different time step lenght), you
  # can use the last value as starting point, but spatial entities that are not
  # in scenario data have to be removed
  if(exists("hist.scen")){
    isos <- unique(subset(tmp, scenario != hist.scen, select=iso3c))
    tmp <- tmp[tmp$iso3c %in% isos$iso3c, ]
    
    # remove hist.scen from scenarios
    scenarios <- unique(tmp$scenario)[unique(tmp$scenario) != hist.scen]
  } else {
    scenarios <- unique(tmp$scenario)
  }
  
  result <- data.frame() # preallocate results data frame
  
  # iterate over scenarios
  for(scen in scenarios){
    
    # create dataset to be manipulated in the loop dependent on whether
    # historical data is used or not
    if(exists("hist.scen")){
      tmp.loop <- subset(tmp, scenario %in% c(hist.scen, scen))
    } else {
      tmp.loop <- subset(tmp, scenario == scen)
    }
    
    # reshape to wide format
    tmp.loop <- dcast(tmp.loop, year ~ iso3c + reg11 + reg33, value.var = "value")
    
    # add rows for missing years
    for(year in years.miss){
      row <- subset(tmp.loop, year == min(tmp.loop$year))
      row[row$year == min(tmp.loop$year), ] <- NA
      row$year <- year
      tmp.loop <- rbind(tmp.loop, row)
    }
    
    # convert to zoo (for interpolation)
    tmp.loop <- zoo(tmp.loop, order.by = tmp.loop$year)
    tmp.loop <- na.approx(tmp.loop) # interpolate
    
    tmp.loop <- as.data.frame(tmp.loop) # convert back to dataframe
    
    # convert to long format
    tmp.loop <- melt(tmp.loop, id.vars=c("year"), variable.name = "iso3c")
    
    # fill in missing columns
    tmp.loop$model <- unique(subset(tmp, scenario == scen &
                                      year < max(years.miss))$model)
    tmp.loop$scenario <- scen
    tmp.loop$item <- unique(subset(tmp, scenario == scen &
                                     year < max(years.miss))$item)
    
    tmp.loop$unit <- unique(subset(tmp, scenario == scen &
                                     year < max(years.miss))$unit)
    tmp.loop$reg11<-substr(tmp.loop$iso3c,5,7)
    tmp.loop$reg33<-substr(tmp.loop$iso3c,9,11)
    tmp.loop$iso3c<-substr(tmp.loop$iso3c,1,3)
      
    # remove the years for which data is already available
    tmp.loop <- subset(tmp.loop, year %in% years.miss)
    
    result <- rbind(result, tmp.loop)
  }
  return(rbind(data, result))
}
