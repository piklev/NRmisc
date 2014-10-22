#' Calculates the ratio of two variables.
#' 
#' @author Niklas Roming
#' @param data A data frame in standard long format.
#' @param newitem Name of the new variable that is calculated.
#' @param numerator Name of the numerator.
#' @param denominator Name of the denominator.
#' @return The original data frame with the newly calculated data appended.
#' @export

RatioNR <- function(data, newitem, numerator, denominator){
  require(NRmisc)
  
  tmp0 <- subset(data, item %in% c(newitem, numerator, denominator))
  
  tmp1 <-AvoidRedundancy(tmp0, newitem)
  
  my.ids <- names(tmp1)[!(names(data) %in% c("value", "item", "model", "unit"))]
  
  # create a combined dataframe that contains all information
  tmp <- merge(subset(tmp1, item == numerator),
               subset(tmp1, item == denominator),
               by=my.ids,
               suffixes = c(".numerator", ".denominator"), all=TRUE)
  
  # do the division
  tmp$value <- tmp$value.numerator / tmp$value.denominator
  
  # determine the unit
  unit.denominator <- unique(tmp$unit.denominator)
  unit.denominator <- unit.denominator[!is.na(unit.denominator)]
  if(length(unit.denominator) != 1){
    cat("myRatio:", numerator, "- no or more than one unit in denominator:",
        unit.denominator, ", using only the first one.", "\n")
  }
  unit.numerator <- unique(tmp$unit.numerator)
  unit.numerator <- unit.numerator[!is.na(unit.numerator)]
  if(length(unit.numerator) != 1){
    cat("myRatio:", denominator, "- no or more than one unit in numerator:", unit.numerator,
        ", using only the first one.", "\n")
  }
  
  #   if(unit.numerator == unit.denominator){
  #     tmp$unit <- "1"
  #   } else {  
  tmp$unit  <- paste(unit.numerator[1], "/", unit.denominator[1], sep="")
  #   }
  
  # set the item to a sensible default if none is given as argument
  if(missing(newitem)){
    tmp$item <- paste(numerator, "/", denominator, sep="")
  } else {
    tmp$item <- newitem
  }
  tmp$model <- "EDGE"
  
  
  tmp <- tmp[, names(data)]
  tmp <- tmp[complete.cases(tmp$value), ]
  tmp <-tmp[is.finite(tmp$value), ]
  
  data <- rbind(data, tmp)
  
  return(data)
}
