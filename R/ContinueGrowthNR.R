#' Lets historical growth continue
#' 
#' @author Niklas Roming
#' @param x Data frame containing a column 'year', unique in 'item' and 'iso3c'
#' @export
ContinueGrowthNR <- function(x){
  row.template <- subset(x, year == max(x$year))
  
  # which years are not in the data
  missing.years.x <- setdiff(t.all, unique(x$year))
  
  # only keep future years
  missing.years <- missing.years.x[missing.years.x > max(unique(x$year))]
  missing.in.between <- missing.years.x[missing.years.x > min(x$year) & missing.years.x < max(x$year)]
  years <- data.frame(year = missing.years) # contains future years
  delta.t <- max(x$year) - min(x$year) # timespan for which data is available
  
  y <- merge(row.template, years, all=TRUE)
  y$model <- "EDGE"
  y$scenario <- unique(x$scenario) # this comes from the wrapping function
  y$item <- unique(x$item)
  y$unit <- unique(x$unit)
  if ("iso3c" %in% colnames(x)){
    y$iso3c <- unique(x$iso3c)
    y$reg11 <- unique(x$reg11)
    y$reg33 <- unique(x$reg33)
  } else {
    y$region <- unique(x$region)
  }
  
  # long term growth rate
  gr.l <- GetGrowthRateNR(subset(x, year %in% unique(x$year)))
  
  # short term growth rate (last two timesteps)
  gr.s <- GetGrowthRateNR(subset(x, year %in% c(max(x[x$year != max(x$year),
                                                      "year"]), max(x$year))))
  
  # determine whether the function is being run on historical data and print
  # some diagnostic output if this is the case
  if(max(x$year) < min(t.scen)){
    cat("\n#################################################################\n")
    cat(unique(x$iso3c), unique(x$item), ":\n")
    cat("First year:", min(x$year), "last year:", max(x$year),
        ", missing in between:", missing.in.between, "\n")
    cat("growth rate between", min(x$year), "and", max(x$year), ":", gr.l, "\n")
    cat("growth rate between", max(x[x$year != max(x$year), "year"]), "and", max(x$year), ":", gr.s, "\n")
  }
  
  # if an exogenous growth rate exists (comes from 'parameters.csv'), fade out
  # historical growth rate over as many years into the future as historical data
  # is available and go over to scenario rate; if no rate is given,continues
  # long run historical growth into the future
  if(exists("rate")){
    # vector of weights for scenario change rate
    w <- data.frame(year = years$year,
                    scenario = 1, # initialize with ones
                    history = 0) # initialize with zeros
    
    # this determines weights as many years into the future as there is historical data
    # available, weights go to zero for history data and to one for scenario (sum = 1)
    w[w$year <= (max(x$year)+delta.t), "scenario"] <-
      1/delta.t * (w[w$year <= (max(x$year)+delta.t), "year"] - max(x$year))
    w$history <- 1 - w$scenario
    w$gr.l <- 1 + w$history * gr.l + w$scenario * rate
    
    y[y$year > max(unique(x$year)), "value"] <-
      y[y$year == max(unique(x$year)), "value"] * cumprod(w$gr.l)
    #      y[y$year == max(unique(x$year)), "value"] * exp(w$gr.l * (missing.years - max(unique(x$year))))
  } else {
    y[y$year > max(unique(x$year)), "value"] <- y[y$year == max(unique(x$year)), "value"] * exp(gr.l * (missing.years - max(unique(x$year))))
  }
  
  return(y)
}