library(shiny)
library(compareFunctions)
library(data.table)


num_box_plots <- 6


shinyServer(function(input, output, session){
  
  # Initializing all session variables and reactive values
  num_funcs <- reactiveVal(0)
  summary_table <- NULL
  lm_list <- NULL
  num_functions <- 0
  measure_vars <- c("user_time", "user_rate", "sys_time", "sys_rate", "elapsed_time", "elapsed_rate")
  
  
  ##-------------------------------------------------------------## 
  ## ------------------------ Input Tab ------------------------ ##  
  ##-------------------------------------------------------------##
  
  # Renders the text areas to input functions
  output$function_txt_areas <- renderUI({
    lapply(X = 1:input$num_funcs, FUN = function(X){
      column(width = 6, 
             shiny::textAreaInput(inputId = paste0("func_text_", X), label = paste0("Function ", X), placeholder = "Enter function here", 
                                  value = paste0("function_", X, " <- function(param){ }"), width = '100%', height = '100%')
             )
      
    })
  })
  
  # Runs when Load Parameter is clicked
  # Loads the parameter into variable param and renders slider with number of increments for running comparison relevant to the param
  observeEvent(input$load_param, {
    
    progress <- shiny::Progress$new(session, min = 0, max = 2)
    on.exit(progress$close())
    progress$set(message = "Load param ", detail = "Loading parameter ")
    
    # run parameter expression
    eval(parse(text = input$param_expr), envir = .GlobalEnv)
    progress$set(value = 1, detail = "Updating UI")
    # create slider for increments and the run comparison button
    output$num_increments_ui <- renderUI({
      tagList(
        sliderInput(inputId = "num_increments", label = "Choose number of incrememnts", min = 2, max = NROW(param), step = 1, value = 2),
        br(),
        actionButton(inputId = "run_comparison", label = "Run Comparison")
      )
    })
    progress$set(value = 2, detail = "Parameters loaded successfully")
    
  })
  
  # Runs when Run Comparison is clicked
  # Loads the functions, runs comparison based on incremenets selected; then computes summary and linear regression on comparison results
  observeEvent(input$run_comparison, {
    progress_index <- 0
    progress = shiny::Progress$new(session, min = progress_index, max = input$num_funcs + 5)
    on.exit(progress$close())
    progress$set(message = "Run Comparison ", detail = "Loading Functions")
    lapply(X = 1:input$num_funcs, FUN = function(X){
      progress_index <- progress_index + 1
      progress$set(value = progress_index, detail = paste0("Loading Functions (", X, "/", input$num_funcs, ")"))
      eval(parse(text = input[[paste0("func_text_", X)]]), envir = .GlobalEnv)
    })
    progress$set(value = progress_index + 1, detail = "Loading functions for comparison")
    function_vector <- lapply(X = 1:input$num_funcs, function(X){
      get(paste0("function_", X))
    })
    progress$set(value = progress_index + 1, detail = "Running and Comparing Functions")
    comp_func_res <<- compare_functions(functions = function_vector, param = param, increments = input$num_increments)
    
    progress$set(value = progress_index + 1, detail = "Getting summary of comparison results")
    get_summary() # This function also sets the value for reactive val num_funcs
    num_functions <- num_funcs()
    progress$set(value = progress_index + 1, detail = "Computing Linear regression on comparison results")
    compute_regression()
    progress$set(value = progress_index + 1, detail = "Run Comparison completed successfully!")
    
  })
  
  ##---------------------------------------------------------------##
  ## ------------------------ Summary Tab ------------------------ ##  
  ##---------------------------------------------------------------##
  
  # Renders the Summary tab
  output$summary_ui <- renderUI({
    
    if(num_funcs() != 0){
        tagList(
          h4("Summary Table"),
          dataTableOutput(outputId = "summ_table"),
          h4("Box Plots"),
          fluidRow(
              lapply(X = measure_vars, FUN = function(X){
                column(width = 2, plotOutput(outputId = paste0(X, "_box_plot")))
               
              })
              
          )
          
        )
      }
    
  })
  
  # Rendering all the box plot
  for(index in 1:num_box_plots){
    local({
      i <- index
      output[[paste0(measure_vars[i], "_box_plot")]] <- renderPlot(
        {
          if(num_funcs() != 0){
            get_boxplot(measure_vars[i])
          }
        }
      )
    })
  }
  

  # Renders the summary data table
  output$summ_table <- renderDataTable({
    get_summary()
  })
  
  # Computes summary and returns a summary table to be displayed
  get_summary <- function(){
    num_funcs(NROW(unique(comp_func_res$function_index)))
    summary_table <- rbind(
      cbind(comp_func_res[, lapply(.SD, mean), by = function_index], as.data.table(list("summ_type" = rep("mean", num_funcs())))),
      cbind(comp_func_res[, lapply(.SD, max), by = function_index], as.data.table(list("summ_type" = rep("max", num_funcs())))),
      cbind(comp_func_res[, lapply(.SD, min), by = function_index], as.data.table(list("summ_type" = rep("min", num_funcs()))))
    )
    summary_table <- summary_table[, .(summ_type, function_index, size, user_time, user_rate, sys_time, sys_rate, elapsed_time, elapsed_rate)]
    setnames(summary_table, old = c("summ_type", "function_index", "size", "user_time", "user_rate", "sys_time", "sys_rate", "elapsed_time", "elapsed_rate"),
             new = c("Type", "Function Index", "Sample Size", "User Time", "User Rate", "System Time", "System Rate", "Elapsed Time", "Elapsed Rate"))
    return(summary_table)
    
  }
  
  # Returns box plots
  get_boxplot <- function(y){
    ggplot(data = comp_func_res, aes(factor(function_index), get(y))) + 
      geom_boxplot() + coord_flip() + 
      geom_boxplot(varwidth = TRUE, fill = "#4D8FAC", color = "black", outlier.shape = 16, outlier.color = "#F62459") +
      theme(panel.background = element_rect(fill = "white")) +
      ylab(label = make_label(y)) + xlab(label = "Function Index")
    
  }
  
  
  ##------------------------------------------------------------------##
  ## ------------------------ Regression Tab ------------------------ ##  
  ##------------------------------------------------------------------##
  
  # Computes Linear Regression and returns list of regression outputs
  compute_regression <- function(){
    lm_list <<- lm.comp.funcs.res.dt(comp_funcs_res = comp_func_res)
    
  }
  
  # Renders Regression Tab
  output$regression_ui <- renderUI({
    if(num_funcs() != 0){
      compute_regression()
      
      tagList(
        h4("Regression Summary"), br(),
        h6("Regression of measures against size of sample"),
        dataTableOutput(outputId = "regression_summary_table"),
        h4("Plots"), br(),
        selectInput(inputId = "lm_plot_type", label = "Choose the type of plot to see", 
                    choices = list("Residuals vs Fitted" = 1, 
                                "Normal Q-Q" = 2, 
                                "Scale-Location" = 3, 
                                "Cook's Distance" = 4, 
                                "Residuals vs Leverage" = 5, 
                                "Cook's dist vs Leverage" = 6)), br(),
        fluidRow(
          lapply(X = 1:num_funcs(), FUN = function(X){
            column(width = as.integer(12/num_funcs()), 
                   plotOutput(outputId = paste0("lm_plot_", X)))
          })
        )
        
      )
      
    }
    
  })
  
  # Renders regression results' data table
  output$regression_summary_table <- renderDataTable({
    measure <- c()
    function_index <- c()
    slope <- c()
    intercept <- c()
    
    for(measure_var in measure_vars){
      for(index in 1:length(lm_list)){
        measure <- c(measure, measure_var)
        function_index <- c(function_index, index)
        intercept <- c(intercept, lm_list[[index]][[measure_var]]$coefficients[[1]])
        slope <- c(slope, lm_list[[index]][[measure_var]]$coefficients[[2]])
      }
    }
    regression_summ_dt <- data.table(measure, function_index, slope, intercept)
    regression_summ_dt$measure <- as.vector(unlist(lapply(X = regression_summ_dt$measure, FUN = make_label)))
    setnames(regression_summ_dt, old = c("measure", "function_index", "slope", "intercept"),
              new = c("Measure", "Function Index", "Slope", "Intercept"))
    regression_summ_dt
    
  })

  # Rendering all lm plots
  observe({
    Map(function(index){
      output[[paste0("lm_plot_", index)]] <- renderPlot(
        {
          if(num_funcs() != 0){
            plot(lm_list[[index]][["user_time"]], which = as.integer(input$lm_plot_type))
          }
        }
      )
    }, c(1:num_funcs()))
    
  })
  
  ##---------------------------------------------------------------------## 
  ## ------------------------ Utility Functions ------------------------ ##  
  ##---------------------------------------------------------------------##
  
  # Converts underscored name string to a Camel Cased, spaced name string
  make_label <- function(name){
    return(
      paste(unlist(lapply(X = unlist(strsplit(x = name, split = "_", fixed = TRUE)), FUN = function(X){
        substr(X, 1, 1) <- toupper(substr(X, 1, 1))
        return(X)
      })), 
      collapse = " ")
    )
  }
  
  
  
})


