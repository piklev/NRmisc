% Generated by roxygen2 (4.0.2): do not edit by hand
\name{AggregateNR}
\alias{AggregateNR}
\title{Performs regional aggregation}
\usage{
AggregateNR(x, ag.vars, region.mapping = "reg11", glob = TRUE)
}
\arguments{
\item{x}{A NRmisc compatible data frame}

\item{ag.vars}{Variables that should be aggregated}

\item{region.mapping}{Region mapping to be applied (must be present in data
frame)}

\item{glob}{Decide whether to include global numbers in the output}
}
\value{
A data frame containing regional aggregates of ag.vars
}
\description{
Performs regional aggregation
}
\author{
Niklas Roming
}

