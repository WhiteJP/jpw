% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_funs.R
\name{na2na}
\alias{na2na}
\title{Evaluate logical expressions, where NAs proliferate (any NA in = NA out)}
\usage{
na2na(x)
}
\arguments{
\item{x}{logical expression}
}
\value{
logical vector evaluating expression x, but where all NAs in input
proliferate in output
}
\description{
Evaluate logical expressions, where NAs proliferate (any NA in = NA out)
}
