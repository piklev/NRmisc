#' Calculates the product of two variables
#' 
#' @author Niklas Roming
#' @param data A data frame in standard long format.
#' @param newitem Name of the new variable that is calculated.
#' @param factor1 Name of the first variable to be multiplied.
#' @param factor2 Name of the second variable to be multiplied.
#' @param redundant Determine whether redundant information should be
#'   included (default = FALSE).
#' @return The original data frame with the newly calculated data appended.
#' @export
ProdNR <- function(data, newitem, factor1, factor2, redundant = FALSE){
  tmp0 <- subset(data, item %in% c(newitem, factor1, factor2))
  
  if(redundant){
    tmp1 <- tmp0
  } else {
    tmp1 <- AvoidRedundancy(tmp0, newitem)
  }
  tmp2 <- subset(tmp1, item == factor1)
  tmp3 <- subset(tmp1, item == factor2)
  
  #   if("iso3c" %in% colnames(data)){
  #   tmp4 <- merge(tmp2, tmp3,
  #                by=c("scenario", "iso3c", "year", "reg11", "reg33"),
  #                suffixes = c(".factor1", ".factor2"), all=TRUE)
  #   } else {
  #     tmp4 <- merge(tmp2, tmp3,
  #                   by=c("scenario", "region", "year"),
  #                   suffixes = c(".factor1", ".factor2"), all=TRUE)
  #   }
  tmp4 <- merge(tmp2, tmp3,
                by = names(tmp0)[!(names(tmp0) %in%
                                     c("model", "unit", "item", "value"))],
                suffixes = c(".factor1", ".factor2"), all = TRUE)  
  
  # do the multiplication
  tmp4$value <- tmp4$value.factor1 * tmp4$value.factor2
  
  # determine the unit
  unit.factor1 <- unique(tmp4$unit.factor1)
  unit.factor1 <- unit.factor1[!is.na(unit.factor1)]
  if(length(unit.factor1) != 1){
    cat("Calculation of:", newitem,"- unit ambiguity in factor1 (",
        unit.factor1, "), using", unit.factor1[1],"\n")
  }
  unit.factor2 <- unique(tmp4$unit.factor2)
  unit.factor2 <- unit.factor2[!is.na(unit.factor2)]
  if(length(unit.factor2) != 1){
    cat("Calculation of:", newitem,"- unit ambiguity in factor2 (",
        unit.factor2, "), using", unit.factor2[1],"\n")
  }
  
  #   if(unit.factor1 == "%"){unit.factor1 <- ""}
  #   
  #   if(unit.factor2 == "%"){unit.factor2 <- ""}
  
  tmp4$unit  <- paste(unit.factor2[1], unit.factor1[1], sep="")
  
  # set the item to a sensible default
  if(missing(newitem)){
    tmp4$item <- paste(factor2, "*", factor1, sep="")
  } else {
    tmp4$item <- newitem
  }
  tmp4$model <- paste(tmp4$model.factor2, tmp4$model.factor1, sep=", ")
  
  tmp4 <- tmp4[, names(data)]
  tmp4 <- tmp4[complete.cases(tmp4$value), ]
  
  result <- rbind(data, tmp4)
  
  return(result)
  
}