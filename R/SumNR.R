#' Calculates the sum of variables
#' 
#' @author Niklas Roming
#' @param data A data frame in standard long format.
#' @param newitem Name of the new item to be calculated.
#' @param add Vector of variable names to be summed up.
#' @param substract Vector of variable names to be substracted.
#' @return The original data frame with the newly calculated data appended.
#' @export
#' 
SumNR <- function(data, newitem, add, substract){
  if(missing(substract)){
    variables <- c(add)
  } else {variables <- c(add, substract)}
  
  tmp <- subset(data, item %in% variables)
  
  
 
  
  if ("iso3c" %in% colnames(tmp)) {
    tmp.w <- dcast(tmp, scenario + iso3c + year +reg11 +reg33~ item)
  } else {
    selectCol <- names(tmp)[!(names(tmp) %in% c("model","unit","item", "value"))]
    tmp.w <- dcast(tmp, paste(paste(selectCol, collapse = " + "), " ~ item"))
  }
  
  # check for unit consistency (all variables must have the same unit)
  unit <- unique(tmp$unit)
  
  #   unit <- unit[!is.na(unit)]
  if (length(unit) > 1){
    cat(newitem, ": More than one unit detected:", unit, ".\n")
  }
  
  tmp.w$unit <- unit[1]
  
  if(!missing(substract)){
    tmp.w[ , substract] <- tmp.w[ , substract] * (-1)
  }
  
  tmp.w$value <- rowSums(tmp.w[ , variables], na.rm=TRUE)
  tmp.w <- tmp.w[ , !(names(tmp.w) %in% variables)]
  
  tmp.w$model <- "EDGE"
  tmp.w$item <- newitem
  #   tmp.w$comment <- ""
  tmp.w <- tmp.w[ , names(data)]
  
  data <- rbind(data, tmp.w) # attach the result to the original data
  
  return(data)
  
}
