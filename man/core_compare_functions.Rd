% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_functions.R
\name{core_compare_functions}
\alias{core_compare_functions}
\title{core_compare_functions}
\usage{
core_compare_functions(functions, param, size)
}
\arguments{
\item{functions}{A vector of single parameter functions. Do not include parameters. Only the function names.}

\item{param}{A single vector, list or data.table parameter for the \code{functions}.}

\item{size}{An integer value. This is the sample size of \code{param} to run the \code{functions}}
}
\value{
A data.table showing execution times and execution rates for the \code{functions} when run with sample of \code{param} 
for sample size \code{size}
}
\description{
Compare Two or more functions on execution times using a given sample size of the given parameter
}
\details{
Runs the functions using random sampling of provided parameter for given sample size, and returns a data table with 
executions times and execution rates for the different functions
}
\examples{
\dontrun{
sample_function_1 <- function(arg){#Logic}
sample_function_2 <- function(arg){#Logic}
param # <A vector/list/data.table object to be passed as arg to the sample_functions>
size # Size of Sample
compare_functions(functions = c(sample_function_1, sample_function_2), param = param, 
size = size)
}
}
\author{
Sujeet G Pillai
}
\keyword{compare}
\keyword{execution}
\keyword{functions,}
\keyword{times}
