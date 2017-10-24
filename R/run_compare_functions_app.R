#' @title compare_functions
#' @author Sujeet G Pillai
#' @keywords compare functions, app, example
#' @description Launches a shiny app that runs on 'compareFunctions'
#' @details Users can load custom parameters and compare between 2-6 functions at the same time to get summary results, plots and regression outputs of the comparison.
#' @export

run_compare_functions_app <- function(){
  app_dir <- system.file("shiny-apps", "Compare Functions", package = "compareFunctions")
  if(app_dir == ""){
    stop("Could not find app directory. Try re-installing 'compareFunctions'.", call. = FALSE)
  }
  shiny::runApp(appDir = app_dir, display.mode = "normal", launch.browser = TRUE)
  
}