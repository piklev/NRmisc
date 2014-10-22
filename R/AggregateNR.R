#' Performs regional aggregation
#' 
#' @author Niklas Roming
#' @param x A NRmisc compatible data frame
#' @param ag.vars Variables that should be aggregated
#' @param region.mapping Region mapping to be applied (must be present in data
#' frame)
#' @return A data frame containing regional aggregates of ag.vars
#' @export 

AggregateNR <- function(x, ag.vars, region.mapping="reg11"){
  require(reshape2)
  # select appropriate subset
  a <- subset(x, item %in% ag.vars)
  
  # extract combination of units and items for remapping units later
  units.tmp <- a[c("year","unit","item")]
  units <- units.tmp[!duplicated(units.tmp),]
  
  # extract combination of model and items for remapping model later
  models.tmp <- a[c("year", "model", "item")]
  models <- models.tmp[!duplicated(models.tmp),]
  
  # delete the unit column because it tampers with the automated 'dcast'ing below
  a$unit <- NULL
  
  # use all columns except value as identifiers
  my.ids <- names(a)[names(a) != "value" & names(a) != "item"]
  
  # create formula
  my.formula <- as.formula(paste(paste(my.ids, collapse="+"), "~item", sep=""))
  
  # create a wide data.frame
  b <- dcast(a, my.formula)
  
  # aggregate values according to region mapping and create global aggregates
  c <- aggregate(b[ag.vars], b[c("scenario", "year", region.mapping)], FUN=sum, na.rm=TRUE)
  names(c)[names(c)==region.mapping] <- "region" # renaming
  c.glob <- aggregate(b[ag.vars], b[c("scenario", "year")], FUN=sum, na.rm=TRUE)
  c.glob$region <- "glob"
  c.glob <- c.glob[names(c)]
  c <- rbind(c, c.glob)
  
  # create a long data frame
  d <- melt(c, id.vars=c("scenario","year","region"))
  names(d)[names(d)=="variable"] <- "item" # renaming
  d$item <- as.character(d$item) # convert from factor to character
  d <- merge(d, models)
  
  e <- merge(d, units)
  e <- merge(e, models)
  
  return(e)
}
