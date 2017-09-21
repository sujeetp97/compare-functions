#' @title compare_functions
#' @author Sujeet G Pillai
#' @keywords compare functions, execution times
#' @description Compare Two or more single parameter functions on execution times with multiple iterations
#' @details Runs the functions using random sampling of the provided parameter for different sample sizes, and returns a data table with executions times and execution rates for the different functions for different sample sizes of the parameter
#' @param functions A vector of single parameter functions. Do not include parameters. Only the function names.
#' @param param A single vector, list or data.table parameter for the \code{functions}.  
#' @param increments An integer value. This is the number of different sample sizes of \code{param} to run the \code{functions} with, each with an incrementally larger \code{size} of sample.
#' @return A data.table showing different execution times and execution rates for the \code{functions} when run with \code{param} for \code{increments} of run each with an incrementally larger \code{size} of sample.
#' @examples
#' sample_function_1 <- function(arg){<Do Something>}
#' sample_function_2 <- function(arg){<Do Something else>}
#' param <- <A vector/list/data.table object to be passed as arg to the sample_functions> 
#' increments <- Number of times to run the functions with incrementally larger sample sizes of param. Eg. If size of param is 10, and increment is 3, then both functions will be run with 3 samples of param with sizes 1, 5 and 10 (evenly spaced out sample sizes)
#' compare_functions(functions = c(sample_function_1, sample_function_2), param = param, increments = increments)
#' @export
compare_functions <- function(functions, param, increments){
  exec_times_dt <- data.table::data.table("user_time"=NA, "sys_time"=NA, "elapsed_time"=NA, "function_index"=NA, "size"=NA)
  sizes <- unique(as.integer(seq(from = 1, to = NROW(param), length.out = increments)))
  
  exec_times_dt <- data.table::rbindlist(lapply(X = sizes, FUN = function(X){
    core_compare_functions(functions, param, X)
  }))
  exec_times_dt[, c("user_rate", "sys_rate", "elapsed_rate") := .(user_time/size, sys_time/size, elapsed_time/size)]
  return(exec_times_dt[order(function_index, size),])
}

#' @title core_compare_functions
#' @author Sujeet G Pillai
#' @keywords compare functions, execution times
#' @description Compare Two or more functions on execution times using a given sample size of the given parameter
#' @details Runs the functions using random sampling of provided parameter for given sample size, and returns a data table with executions times and execution rates for the different functions
#' @param functions A vector of single parameter functions. Do not include parameters. Only the function names.
#' @param param A single vector, list or data.table parameter for the \code{functions}.  
#' @param size An integer value. This is the sample size of \code{param} to run the \code{functions}
#' @return A data.table showing execution times and execution rates for the \code{functions} when run with sample of \code{param} for sample size \code{size}
#' @examples
#' sample_function_1 <- function(arg){<Do Something>}
#' sample_function_2 <- function(arg){<Do Something else>}
#' param <- <A vector/list/data.table object to be passed as arg to the sample_functions> 
#' size <- Size of Sample
#' compare_functions(functions = c(sample_function_1, sample_function_2), param = param, size = size)
#' @export
core_compare_functions <- function(functions, param, size){
  exec_times_dt <- data.table::data.table(t(
    data.table::as.data.table(
      lapply(X = seq_along(functions), FUN = function(X){
        c(as.vector(unlist(system.time(functions[[X]](sampler(collection = param, size = size)))))[1:3],X, size)
      }))))
  names(exec_times_dt) <- c("user_time", "sys_time", "elapsed_time", "function_index", "size")
  return(exec_times_dt)
  
}
