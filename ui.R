library(tidyverse)
library(caret)
library(bslib)
library(shinythemes)

fluidPage(
   #shinythemes::themeSelector(),
    navbarPage(
    theme = shinytheme("superhero"),
    "Modeling Heart Failure",
        tabPanel("About", 
             #tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/7/70/Fluent_Emoji_Color_1fac0.svg", height = "200px")
             tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/3/32/Noto_Emoji_v2.034_1fac0.svg", height = "200px")
        ),
        tabPanel("Data Exploration",
                 ),
        tabPanel("Modeling",
                 
            sidebarPanel("Selections",
                     
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
                            choices = colnames(dat),
                            multiple = TRUE
                        ),
                    
                    selectizeInput(
                        inputId = "var_rf",
                        label = "Predictor Variables for Random Forest",
                        choices = colnames(dat),
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
                tabPanel("Model Fitting",),
                tabPanel("Model Prediction",))
              
                   
   )
     
)))

