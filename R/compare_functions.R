#' @title compare_functions
#' @author Sujeet G Pillai
#' @keywords compare functions, execution times
#' @description Compare Two or more single parameter functions on execution times with multiple iterations
#' @details Runs the functions using random sampling of the provided parameter for different sample sizes, 
#' and returns a data table with executions times and execution rates for the different functions 
#' for different sample sizes of the parameter
#' @param functions A vector of single parameter functions. Do not include parameters. Only the function names.
#' @param param A single vector, list or data.table parameter for the \code{functions}.
#' @param increments An integer value. This is the number of different sample sizes of \code{param} to run the 
#' \code{functions} with, each with an incrementally larger \code{size} of sample.
#' @return A data.table showing different execution times and execution rates for the \code{functions} when run 
#' with \code{param} for \code{increments} of run each with an incrementally larger \code{size} of sample.
#' @examples
#' \dontrun{
#' sample_function_1 <- function(arg){#Logic}
#' sample_function_2 <- function(arg){#Logic}
#' param # <A vector/list/data.table object to be passed as arg to the sample_functions>
#' increments # Number of times to run the functions with incrementally larger sample 
#' sizes of param. Eg. If size of param is 10, and increment is 3, then both functions 
#' will be run with 3 samples of param with sizes 1, 5 and 10 (evenly spaced out sample 
#' sizes)
#' compare_functions(functions = c(sample_function_1, sample_function_2), param = param, 
#' increments = increments)
#' }
#' @export
compare_functions <- function(functions, param, increments){
  exec_times_dt <- data.table::data.table("user_time"=NA, "sys_time"=NA, "elapsed_time"=NA, "function_index"=NA, "size"=NA)
  sizes <- unique(as.integer(seq(from = 1, to = NROW(param), length.out = increments)))

  exec_times_dt <- data.table::rbindlist(lapply(X = sizes, FUN = function(X){
    core_compare_functions(functions, param, X)
  }))
  exec_times_dt[, c("user_rate", "sys_rate", "elapsed_rate") := .(user_time/size, sys_time/size, elapsed_time/size)]
  class(exec_times_dt) <- c("comp.funcs.res.dt", "data.table", "data.frame")
  return(exec_times_dt[order(function_index, size),])
}

#' @title core_compare_functions
#' @author Sujeet G Pillai
#' @keywords compare functions, execution times
#' @description Compare Two or more functions on execution times using a given sample size of the given parameter
#' @details Runs the functions using random sampling of provided parameter for given sample size, and returns a data table with 
#' executions times and execution rates for the different functions
#' @param functions A vector of single parameter functions. Do not include parameters. Only the function names.
#' @param param A single vector, list or data.table parameter for the \code{functions}.
#' @param size An integer value. This is the sample size of \code{param} to run the \code{functions}
#' @return A data.table showing execution times and execution rates for the \code{functions} when run with sample of \code{param} 
#' for sample size \code{size}
#' @examples
#' \dontrun{
#' sample_function_1 <- function(arg){#Logic}
#' sample_function_2 <- function(arg){#Logic}
#' param # <A vector/list/data.table object to be passed as arg to the sample_functions>
#' size # Size of Sample
#' compare_functions(functions = c(sample_function_1, sample_function_2), param = param, 
#' size = size)
#' }
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

#' @title summary.comp.funcs.res.dt
#' @author Sujeet G Pillai
#' @keywords summary, compare functions, s3 method
#' @description Gives a summary by function index
#' @details S3 \code{summary} method for class \code{comp.funcs.res.dt}. Prints traditional summary output by function index
#' @param comp_funcs_res Object of class \code{comp.funcs.res.dt} - result of function \code{compare_functions()}
#' @export
summary.comp.funcs.res.dt <- function(comp_funcs_res){
  for(index in unique(comp_funcs_res$function_index)){
    cat(paste0("\nFunction Index : ", index, "\n"))
    print(summary.data.frame(comp_funcs_res[function_index == index, .(user_time, sys_time, elapsed_time, size, user_rate, 
                                                                       sys_rate, elapsed_rate)]))
  }
}


#' @title lm.comp.funcs.res.dt
#' @author Sujeet G Pillai
#' @keywords lm, compare functions, s3 method, linear regression
#' @description Function for \code{lm} for Linear regression over time attributes by size for every function
#' @details Runs Linear regression over time attributes \code{user_time, sys_time, elapsed_time} as Y and \code{size} as X.
#' @param comp_funcs_res Object of class \code{comp.funcs.res.dt} - result of function \code{compare_functions()}
#' @return Returns a list where first level index corresponds to \code{function_index} 
#' and second level names \code{user_time, sys_time and elapsed_time}  have lm result
#' objects of respective time attributes \code{user_time, sys_time and elapsed_time} 
#' with \code{size} for the corresponding \code{function_index}
#' @export
lm.comp.funcs.res.dt <- function(comp_funcs_res){
  lm_list <- list()
  y_params <- c("user_time", "sys_time", "elapsed_time")
  for(index in unique(comp_funcs_res$function_index)){
    temp_list <- list()
    length(temp_list) <- length(y_params)
    names(temp_list) <- y_params
    for(param in y_params){
      temp_list[[param]] <- lm(formula = get(param) ~ size, data = comp_funcs_res[function_index == index, ])
    }
    lm_list[[index]] <- temp_list

  }
  return(lm_list)

}

#' @title plot.comp.funcs.res.dt
#' @author Sujeet G Pillai
#' @keywords plot, compare functions, s3 method, ggplot
#' @description Gives a plot object for specified Y axis
#' @details S3 \code{plot} method for class \code{comp.funcs.res.dt}. Plots a line chart with function_index split by 
#' function_index with \code{size} in X axis
#' @param comp_funcs_res Object of class \code{comp.funcs.res.dt} - result of function \code{compare_functions()}
#' @param y \code{character} value of column to use for Y axis
#' @return ggplot2 object with the plot
#' @export
plot.comp.funcs.res.dt <- function(comp_funcs_res, y){
  return(ggplot(data = comp_funcs_res, aes_string(x = "size", y = y)) + geom_line(aes(color = factor(function_index))))
}
