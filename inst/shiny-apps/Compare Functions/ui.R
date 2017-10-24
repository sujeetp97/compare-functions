library(shiny)
library(data.table)

shinyUI(shiny::navbarPage(title = "Compare Functions", id = "root_nav_page", 
                          shiny::tabPanel(title = "Input Tab",
                                          shiny::sidebarLayout(
                                            sidebarPanel(
                                              shiny::sliderInput(inputId = "num_funcs", label = "Number of Functions to compare:", value = 2, min = 2, max = 6, step = 1),
                                              shiny::br(), shiny::hr(),
                                              shiny::textAreaInput(inputId = "param_expr", label = "Parameter Expression", placeholder = "Enter your expression to set the parameter to compare with.\nName of parameter should be 'param'"),
                                              shiny::br(),
                                              shiny::actionButton(inputId = "load_param", label = "Load Parameter"),
                                              shiny::br(), shiny::hr(),
                                              shiny::uiOutput(outputId = "num_increments_ui"),
                                              shiny::textOutput(outputId = "status_text")
                                              ),
                                            mainPanel(shiny::fluidPage(
                                              fluidRow(
                                                shiny::uiOutput(outputId = "function_txt_areas")
                                                
                                            ))
                                            ) 
                                            )),
                          shiny::tabPanel(title = "Summary", 
                                          fluidPage(
                                            fluidRow(
                                              shiny::uiOutput(outputId = "summary_ui")
                                            )
                                          )),
                          shiny::tabPanel(title = "Regression",
                                          fluidPage(
                                            fluidRow(
                                              shiny::uiOutput(outputId = "regression_ui")
                                            )
                                          ))
                          )
        )