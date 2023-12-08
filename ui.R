library(shinydashboard)
library(tidyverse)
library(caret)
library(bslib)

dashboardPage(
    skin = "purple",
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("circle-info")),
            menuItem("Data Exploration", tabName = "data_exploration", icon = icon("compass")),
            menuItem("Modeling", tabName = "model_info", icon = icon("brain"),
                     startExpanded = FALSE,
                     menuSubItem("Modeling Info",tabName = "model_info"),
                     menuSubItem("Model Fitting",tabName = "model_fit"),
                     
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
                    
                    actionButton("fit", "Fit Models"),
                        
                     menuSubItem("Prediction",tabName = "model_predict")
                     
                     )
        )
    ),
    dashboardBody( dashboardBody(
        
        
        tabItems(
            tabItem(tabName = "about", h2("About"),
                    
                    #tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/7/70/Fluent_Emoji_Color_1fac0.svg", height = "200px")
                    tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/3/32/Noto_Emoji_v2.034_1fac0.svg", height = "200px")
                    
                    ),
            tabItem(tabName = "data_exploration", h2("Data Exploration")),
            tabItem(tabName = "model_info", h2("Model Info")),
            tabItem(tabName = "model_fit", h2("Model Fitting"),
                    
                    textOutput("split_cols"),
                    textOutput("train_rf_cols")
                    
                    ),
            tabItem(tabName = "model_predict", h2("Model Prediction"))
            )
        ))
)

