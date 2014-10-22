#' Return countries for which LSDV estimation was carried out
#' 
#' @author Niklas Roming
#' @param lm A linear model
#' @param formula The formula used for estimation (TODO: explain why this is needed)
#' @return A character vector containing the ISO codes of the country fixed
#' effects
#' @export

LMIsoNR <- function(lm, formula){  
  # for which countries the estimation was carried out? this information is 
  # stored in lm$coefficients, but we need to filter out the first elements of
  # the vector of coefficients since these are those of the elements of the
  # estimated formula
  formula.rhs <- as.character(formula[3]) # 3rd element contains rhs of formula
  regressors <- unlist(strsplit(formula.rhs, " + ", fixed = TRUE))
  coefficients <- names(lm$coefficients)
  
  # delete the first n elements of the coefficients vector so that only the 
  # country fixed effects are left. These are strings like 'factor(iso3c)USA'
  # so the first parts needs to be filtered
  coefficients <- coefficients[-c(1:(length(regressors)))]
  iso3c.est <- gsub("factor(iso3c)", "", coefficients, fixed = TRUE)
  
  # manual inclusion of Angola (AGO) is necessary because R uses AGO as reference
  # point for the country specific intercepts; therefore no coefficient for
  # AGO is included by default
  iso3c.est <- c("AGO", iso3c.est)
  return(iso3c.est)
}