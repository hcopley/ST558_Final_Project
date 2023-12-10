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
                 sidebarPanel(width = 2, "Data Exploration Selections",
                              
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
                 
            sidebarPanel(width = 2, "Model Fitting Selections",
                     
                     sliderInput(
                         inputId = "split",
                         label = "Training Data Split Percentage",
                         min = 0,
                         max = 100,
                         post = " %",
                         value = 80
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
                    
                    numericInput(
                      inputId = "number",
                      label = "Cross Validation: Number of Folds",
                      value = 5,
                      min = 1,
                      max = 10
                    ),
                    
                    numericInput(
                        inputId = "repeats",
                        label = "Cross Validation: Number of Repeats",
                        value = 3,
                        min = 1,
                        max = 10
                    ),
                
                    sliderInput(inputId = "mtry_range", 
                            label ="Range for Tuning The Number of Variables (mtry) in Random Forest", 
                            min = 1, 
                            max = 12, 
                            value = c(1, 5)
                    ),
                 
                    
                    actionButton("fit", "Fit Models", class = "btn-danger")
                    
           
           ), 
                    
                     
                     
    mainPanel(
            tabsetPanel(id = "tabset",
                tabPanel("Model Info",),
                tabPanel("Model Fitting",
                        
                    fluidRow(
                         
                        column(4, h4("Logistic Regression"),
                        tags$b("Fit Statistics"),
                        tableOutput('LR'),
                        tags$b("Coefficients"),
                        tableOutput('coeff_LR'),
                        tags$b("Test Statistics"),
                        tags$style(type='text/css', '#confMatLR {background-color: rgba(0,0,0,0); color: white;}'),
                        verbatimTextOutput('confMatLR')
                        
                         ),
                        column(4, h4("Random Forest"),
                        tags$b("Fit Statistics"), 
                        tableOutput('RF'),
                        tags$b("Variable Importance"),
                        tableOutput('var_imp_RF'),
                        tags$b("Test Statistics"),
                        tags$style(type='text/css', '#confMatRF {background-color: rgba(0,0,0,0); color: white;}'),
                        verbatimTextOutput('confMatRF')
                         )
                    )
                             
                         ),
                tabPanel("Model Prediction",
                         
                         column(3, h4("Logistic Regression"),
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('age')",
                                           sliderInput(
                                               inputId = "age_val_lr",
                                               label = "Enter a value for Age",
                                               value = mean(dat$age),
                                               min = min(dat$age),
                                               max = max(dat$age),
                                               step = 1
                                           )),
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('anaemia')",
                                           selectizeInput(
                                               inputId = "anaemia_val_lr",
                                               label = "Enter a value for Anaemia",
                                               choices = c("Yes", "No")
                                           )),
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('creatinine_phosphokinase')",
                                           sliderInput(
                                               inputId = "creatinine_phosphokinase_val_lr",
                                               label = "Enter a value for Creatinine Phosphokinase",
                                               value = mean(dat$creatinine_phosphokinase),
                                               min = min(dat$creatinine_phosphokinase),
                                               max = max(dat$creatinine_phosphokinase),
                                               step = 1
                                           )),
                                       
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('diabetes')",
                                           selectizeInput(
                                               inputId = "diabetes_val_lr",
                                               label = "Enter a value for Diabetes",
                                               choices = c("Yes", "No")
                                           )),
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('ejection_fraction')",
                                           sliderInput(
                                               inputId = "ejection_fraction_val_lr",
                                               label = "Enter a value for Ejection Fraction",
                                               value = mean(dat$ejection_fraction),
                                               min = min(dat$ejection_fraction),
                                               max = max(dat$ejection_fraction),
                                               step = 1
                                           )),
                                       
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('high_blood_pressure')",
                                           selectizeInput(
                                               inputId = "high_blood_pressure_val_lr",
                                               label = "Enter a value for High Blood Pressure",
                                               choices = c("Yes", "No")
                                           )),
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('platelets')",
                                           sliderInput(
                                               inputId = "platelets_val_lr",
                                               label = "Enter a value for Platelets",
                                               value = mean(dat$platelets),
                                               min = min(dat$platelets),
                                               max = max(dat$platelets),
                                               step = 1
                                           )),
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('serum_creatinine')",
                                           sliderInput(
                                               inputId = "serum_creatinine_val_lr",
                                               label = "Enter a value for Serum Creatinine",
                                               value = mean(dat$serum_creatinine),
                                               min = min(dat$serum_creatinine),
                                               max = max(dat$serum_creatinine),
                                               step = .1
                                           )),
                                       
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('serum_sodium')",
                                           sliderInput(
                                               inputId = "serum_sodium_val_lr",
                                               label = "Enter a value for Serum Sodium",
                                               value = mean(dat$serum_sodium),
                                               min = min(dat$serum_sodium),
                                               max = max(dat$serum_sodium),
                                               step = 1
                                           )),
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('sex')",
                                           selectizeInput(
                                               inputId = "sex_val_lr",
                                               label = "Enter a value for Sex",
                                               choices = c("Female", "Male")
                                           )),
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('smoking')",
                                           selectizeInput(
                                               inputId = "smoking_val_lr",
                                               label = "Enter a value for Smoking",
                                               choices = c("Yes", "No")
                                           )),
                                       
                                       conditionalPanel(
                                           condition = "input.var_logit.includes('time')",
                                           sliderInput(
                                               inputId = "time_val_lr",
                                               label = "Enter a value for Time",
                                               value = mean(dat$time),
                                               min = min(dat$time),
                                               max = max(dat$time),
                                               step = 1
                                           )),
                                       
                                       actionButton("predict_logit", "Predict Logistic Regression", class = "btn-danger"),    
                                       tableOutput('show_inputs')
                                   ),
                         column(3, h4("Random Forest"),
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('age')",
                                    sliderInput(
                                        inputId = "age_val_rf",
                                        label = "Enter a value for Age",
                                        value = mean(dat$age),
                                        min = min(dat$age),
                                        max = max(dat$age),
                                        step = 1
                                    )),
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('anaemia')",
                                    selectizeInput(
                                        inputId = "anaemia_val_rf",
                                        label = "Enter a value for Anaemia",
                                        choices = c("Yes", "No")
                                    )),
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('creatinine_phosphokinase')",
                                    sliderInput(
                                        inputId = "creatinine_phosphokinase_val_rf",
                                        label = "Enter a value for Creatinine Phosphokinase",
                                        value = mean(dat$creatinine_phosphokinase),
                                        min = min(dat$creatinine_phosphokinase),
                                        max = max(dat$creatinine_phosphokinase),
                                        step = 1
                                    )),
                                
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('diabetes')",
                                    selectizeInput(
                                        inputId = "diabetes_val_rf",
                                        label = "Enter a value for Diabetes",
                                        choices = c("Yes", "No")
                                    )),
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('ejection_fraction')",
                                    sliderInput(
                                        inputId = "ejection_fraction_val_rf",
                                        label = "Enter a value for Ejection Fraction",
                                        value = mean(dat$ejection_fraction),
                                        min = min(dat$ejection_fraction),
                                        max = max(dat$ejection_fraction),
                                        step = 1
                                    )),
                                
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('high_blood_pressure')",
                                    selectizeInput(
                                        inputId = "high_blood_pressure_val_rf",
                                        label = "Enter a value for High Blood Pressure",
                                        choices = c("Yes", "No")
                                    )),
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('platelets')",
                                    sliderInput(
                                        inputId = "platelets_val_rf",
                                        label = "Enter a value for Platelets",
                                        value = mean(dat$platelets),
                                        min = min(dat$platelets),
                                        max = max(dat$platelets),
                                        step = 1
                                    )),
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('serum_creatinine')",
                                    sliderInput(
                                        inputId = "serum_creatinine_val_rf",
                                        label = "Enter a value for Serum Creatinine",
                                        value = mean(dat$serum_creatinine),
                                        min = min(dat$serum_creatinine),
                                        max = max(dat$serum_creatinine),
                                        step = .1
                                    )),
                                
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('serum_sodium')",
                                    sliderInput(
                                        inputId = "serum_sodium_val_rf",
                                        label = "Enter a value for Serum Sodium",
                                        value = mean(dat$serum_sodium),
                                        min = min(dat$serum_sodium),
                                        max = max(dat$serum_sodium),
                                        step = 1
                                    )),
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('sex')",
                                    selectizeInput(
                                        inputId = "sex_val_rf",
                                        label = "Enter a value for Sex",
                                        choices = c("Female", "Male")
                                    )),
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('smoking')",
                                    selectizeInput(
                                        inputId = "smoking_val_rf",
                                        label = "Enter a value for Smoking",
                                        choices = c("Yes", "No")
                                    )),
                                
                                conditionalPanel(
                                    condition = "input.var_rf.includes('time')",
                                    sliderInput(
                                        inputId = "time_val_rf",
                                        label = "Enter a value for Time",
                                        value = mean(dat$time),
                                        min = min(dat$time),
                                        max = max(dat$time),
                                        step = 1
                                    )),
                                
                                actionButton("predict_rf", "Predict Random Forest", class = "btn-danger") 
                                
                                
                                ),
                column(3, h4("Results"),
                       textOutput('predictionLR'),
                       textOutput('predictionRF')
                       
                       )
                )
                       
                        
                         
                         
                         ))
              
                   
     
)))

