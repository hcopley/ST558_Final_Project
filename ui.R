library(tidyverse)
library(caret)
library(bslib)
library(shinythemes)
library(plotly)

fluidPage(
   #shinythemes::themeSelector(),
    navbarPage(
    theme = shinytheme("superhero"),
    "Modeling Heart Failure",
        tabPanel("About", 
             tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/3/32/Noto_Emoji_v2.034_1fac0.svg", height = "200px")
        ),
        tabPanel("Data Exploration",
                 sidebarPanel("Data Exploration Selections",
                              
                              selectizeInput(
                                  inputId = "filter_var",
                                  label = "Filter Variable",
                                  choices = c('none', colnames(dat)),
                                  selected = 'none'
                              ),
                              
                              conditionalPanel(condition = paste0("[",paste0("'",cat_vars,"'", collapse = ', '),"].includes(input.filter_var)"),
                                  selectizeInput(
                                      inputId = "filter_criteria",
                                      label = "Filter",
                                      choices = c('Yes', 'No')
                                  ) 
                              ),
                              
                              conditionalPanel(condition = paste0("[",paste0("'",num_vars,"'", collapse = ', '),"].includes(input.filter_var)"),
                                               sliderInput(
                                                   inputId = "filter_slider",
                                                   label = "Filter",
                                                   min = 0, max = 100, value = c(40,60)
                                               ) 
                              ),
                              
                              radioButtons(inputId = "plot_type",
                                           label = "Plot Type",
                                           choices = c('bar', 'scatter', 'box', 'density')
                                  
                              ),
                              
                              conditionalPanel(
                                  condition = "input.plot_type != 'density' & input.plot_type !=  'bar'",
                              selectizeInput(
                                  inputId = "y",
                                  label = "Y Axis Variable",
                                  choices = num_vars
                              )),
                              
                              selectizeInput(
                                  inputId = "x",
                                  label = "X Axis Variable",
                                  choices = num_vars
                              ),
                              
                              
                              selectizeInput(
                                  inputId = "color",
                                  label = "Color By:",
                                  choices = c("none", colnames(dat))
                              ),
                              
                              selectizeInput(
                                  inputId = "facet",
                                  label = "Facet By:",
                                  choices = c("none", cat_vars)
                              ),
                              
                              ),
                 mainPanel("Data Exploration Plots", 
                           
                           plotlyOutput("plot")
                           
                           )
                              
                 
                 ),
        tabPanel("Modeling",
                 
            sidebarPanel("Model Fitting Selections",
                     
                     sliderInput(
                         inputId = "split",
                         label = "Training Data Split Percentage",
                         min = 0,
                         max = 100,
                         post = " %",
                         value = 80,
                     ),
                     
                    selectizeInput(
                            inputId = "var_logit",
                            label = "Predictor Variables for Logistic Regression",
                            choices = colnames(dat)[!colnames(dat) == 'DEATH_EVENT'],
                            multiple = TRUE
                        ),
                    
                    selectizeInput(
                        inputId = "var_rf",
                        label = "Predictor Variables for Random Forest",
                        choices = colnames(dat)[!colnames(dat) == 'DEATH_EVENT'],
                        multiple = TRUE
                    ),
                    
                    sliderInput(inputId = "mtry_range", 
                                label ="Range for Tuning mtry in Random Forest", 
                                min = 1, 
                                max = 12, 
                                value = c(1, 4)
                    ),
                    
                    actionButton("fit", "Fit Models")
                        
                     
                     ),
    mainPanel(
            tabsetPanel(
                tabPanel("Model Info",),
                tabPanel("Model Fitting",
                        
                         verbatimTextOutput('LR'),

                        
                        verbatimTextOutput('RF')
                             
                         ),
                tabPanel("Model Prediction",))
              
                   
   )
     
)))

