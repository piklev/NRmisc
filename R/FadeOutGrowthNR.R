#' Continues a time series by fading out historical change rates over a certain
#' time
#' 
#'  @author Niklas Roming
#'  @param tmp A NRmisc compatible data frame unique wrt. 'iso3c', 'scenario' and 'item'.
#'  @export

FadeOutGrowthNR <- function(tmp, fade.time){
  row.template <- subset(tmp, year == max(tmp$year))
  
  # which years are not in the data
  missing.years.tmp <- setdiff(t.all, unique(tmp$year))
  
  # only keep future years
  missing.years <- missing.years.tmp[missing.years.tmp > max(unique(tmp$year))]
  years <- data.frame(year = missing.years)
  tmp1 <- merge(row.template, years, all=TRUE)
  tmp1$model <- "EDGE"
  tmp1$scenario <- unique(tmp$scenario)
  tmp1$item <- unique(tmp$item)
  tmp1$unit <- unique(tmp$unit)
  tmp1$iso3c <- unique(tmp$iso3c)
  tmp1$reg11 <- unique(tmp$reg11)
  tmp1$reg33 <- unique(tmp$reg33)
  
  gr <- GetGrowthRateNR(subset(tmp, year %in% unique(tmp$year)))
  change <- gr/fade.time
  gr <- gr - (missing.years - max(unique(tmp$year))) * change
  
  
  tmp1[tmp1$year > max(unique(tmp$year)), "value"] <- tmp1[tmp1$year == max(unique(tmp$year)), "value"] * exp(gr * (missing.years - max(unique(tmp$year))))
  
  return(tmp1)
}