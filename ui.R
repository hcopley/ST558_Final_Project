library(tidyverse)
library(shinythemes)
library(plotly)
library(caret)
library(randomForest)

#Read in the data and set appropriate levels for the categorical variables
dat <- read_csv('heart_failure_clinical_records_dataset.csv') %>%
    mutate_at(vars(anaemia, diabetes, high_blood_pressure, smoking, DEATH_EVENT), 
              ~factor(.,levels = c(0,1), labels = c('Yes', 'No'))) %>%
    mutate(sex = factor(sex, levels = c(0,1), labels = c('Female', 'Male')))

#identify the categorical variables
cat_vars <- dat %>%
    select_if(is.factor) %>%
    colnames()

#identify the numeric variables
num_vars <- dat %>%
    select_if(is.numeric) %>%
    colnames()

#start the ui
fluidPage(
    
    #do not allow columns to overlap
    tags$head(tags$style(
        "body { word-wrap: break-word; }"
    )),
    
    navbarPage(
    theme = shinytheme("superhero"),
    "Modeling Heart Failure",
    
#ABOUT PAGE **************************----
        tabPanel("About", 
            column(3,
             tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/3/32/Noto_Emoji_v2.034_1fac0.svg", height = "200px")
            ),
             column(9,
             tags$div(
                
                 tags$h2('Predictive Modeling of Heart Failure Survival'),
                tags$p('This application is designed to facilitate the analysis and modeling of heart failure data. It offers tools for data exploration, visualization, and predictive modeling.'),
                tags$h3('Heart Failure Data'),
                tags$p('The data used in this app was sourced from',
                        tags$a(href="https://archive.ics.uci.edu/", "UC Irvine Machine Learning Repository."),
                        '  More information about this data can be found ',
                        tags$a(href = "https://archive.ics.uci.edu/dataset/519/heart+failure+clinical+records", "here.")
                        ),
                tags$p('The dataset contains 299 heart failure patients, covering 13 clinical features per patient. The data, collected during follow-up periods, offers insights into various health indicators relevant to heart failure research.'),
                
                tags$h3('Navigation'),
                tags$h4('Data Exploration Tab'),
                tags$p('In the Data Exploration tab, users can engage with data through a variety of interactive tools:'),
                tags$ul(
                    tags$li('Filtering Options: Select and filter variables.'),
                    tags$li('Plot Selection: Choose from bar, scatter, box, or density plots.'),
                    tags$li('Customization: Set axes and color variables; facet by categories.'),
                    tags$li('Dynamic Visualization: Plot updates based on selections.')
                    ),
                
                tags$h4('Modeling Tab'),
                tags$p('The Modeling tab is divided into three subtabs, each offering unique functionalities:'),
                tags$ul(
                    tags$li('Model Info Tab'),
                        tags$ul(
                            tags$li('Learn about logistic regression and random forest models.')
                        ),
                    tags$li('Model Fitting Tab:'),
                        tags$ul(
                            tags$li('Set training/test data split.'),
                            tags$li('Select predictors for each model type.'),
                            tags$li('Tune random forest parameters.'),
                            tags$li('Fit models and review statistical summaries.')
                        ),
                    tags$li('Model Prediction Tab:'),
                        tags$ul(
                            tags$li('Input values for predictions in logistic regression and random forest models based on variables selected in training.'),
                            tags$li('View prediction results.')
                            
                        )
                    
                )
                
                )
             )
             
             
             
        ),

#DATA EXPLORATION PAGE **************************----
        tabPanel("Data Exploration",
                 sidebarPanel(width = 3, "Data Exploration Selections",
                    
                            #Data exploration widgets            
                              selectizeInput(
                                  inputId = "filter_var",
                                  label = "Filter Variable",
                                  choices = c('none', colnames(dat)),
                                  selected = 'none'
                              ),
                              
                            #if the filter variable is categorical show a selector 
                              conditionalPanel(condition = paste0("[",paste0("'",cat_vars,"'", collapse = ', '),"].includes(input.filter_var)"),
                                  selectizeInput(
                                      inputId = "filter_criteria",
                                      label = "Filter",
                                      choices = c('Yes', 'No')
                                  ) 
                              ),
                              
                            #if the filter variable is numeric show a slider
                              conditionalPanel(condition = paste0("[",paste0("'",num_vars,"'", collapse = ', '),"].includes(input.filter_var)"),
                                               sliderInput(
                                                   inputId = "filter_slider",
                                                   label = "Filter",
                                                   min = 0, max = 100, value = c(40,60)
                                               ) 
                              ),
                              
                            #allow the user to enter the plot type
                              radioButtons(inputId = "plot_type",
                                           label = "Plot Type",
                                           choices = c('bar', 'scatter', 'box', 'density')
                                  
                              ),
                              
                            #if the plot type is a bar allow the user to select a numeric variable on the y axis
                              conditionalPanel(
                                  condition = "input.plot_type != 'density' & input.plot_type !=  'bar'",
                              selectizeInput(
                                  inputId = "y",
                                  label = "Y Axis Variable",
                                  choices = num_vars
                              )),
                              
                            #allow the user to select a numeric variable for the x axis (this will be updated in the server)
                              selectizeInput(
                                  inputId = "x",
                                  label = "X Axis Variable",
                                  choices = num_vars
                              ),
                              
                              #allow the user to select a variable for the color
                              selectizeInput(
                                  inputId = "color",
                                  label = "Color By:",
                                  choices = c("none", colnames(dat))
                              ),
                              
                            #allow the user to select a variable for the facet
                              selectizeInput(
                                  inputId = "facet",
                                  label = "Facet By:",
                                  choices = c("none", cat_vars)
                              ),
                              
                              ),
                 #output the plot in the main panel
                 mainPanel("Data Exploration Plots", 
                           
                           plotlyOutput("plot")
                           
                           )
                              
#MODELING PAGE **************************----                 
                 ),
        tabPanel("Modeling",
                 
            sidebarPanel(width = 3, "Model Fitting Selections",
                     
                    #allow the user to choose the train test split %
                     sliderInput(
                         inputId = "split",
                         label = "Training Data Split Percentage",
                         min = 0,
                         max = 100,
                         post = " %",
                         value = 80
                     ),
                     
                    #allow the user to choose the predictor variables for the logistic regression model
                    selectizeInput(
                            inputId = "var_logit",
                            label = "Predictor Variables for Logistic Regression",
                            choices = colnames(dat)[!colnames(dat) == 'DEATH_EVENT'],
                            multiple = TRUE
                        ),
            
                    #allow the user to choose the predictor variables for the random forest model
                    selectizeInput(
                        inputId = "var_rf",
                        label = "Predictor Variables for Random Forest",
                        choices = colnames(dat)[!colnames(dat) == 'DEATH_EVENT'],
                        multiple = TRUE
                    ),
                    
                    #allow the user to choose the number of folds
                    numericInput(
                      inputId = "number",
                      label = "Cross Validation: Number of Folds",
                      value = 5,
                      min = 1,
                      max = 10
                    ),
                    
                    #allow the user to choose the number of repeats
                    numericInput(
                        inputId = "repeats",
                        label = "Cross Validation: Number of Repeats",
                        value = 3,
                        min = 1,
                        max = 10
                    ),
                
                    #allow the user to enter the tuning grid range for mtry
                    sliderInput(inputId = "mtry_range", 
                            label ="Range for Tuning The Number of Variables (mtry) in Random Forest", 
                            min = 1, 
                            max = 12, 
                            value = c(1, 5)
                    ),
                 
                    #allow the user to fit the models when the button is pressed
                    actionButton("fit", "Fit Models", class = "btn-danger")
                    
           
           ), 
                    
                     
                     
    mainPanel(
            tabsetPanel(id = "tabset",
                tabPanel("Model Info",
                         
                         #Display information about Logistic Regression
                         column(3,
                         
                         tags$h3('Logistic Regression'),
                         
                         tags$p(tags$a(href='https://simple.wikipedia.org/wiki/Logistic_regression','Logistic regression'),
                                             'is a generalized linear model appropriate to binary outcome data. 
                                The predictor variables may be continuous or categorical. It models the probability of success of the outcome class using the logistic function'),
                         
                         withMathJax('$$P(success) = \\frac{e^{\\beta_0 + B_1x_1 + B_2x_2 +... + B_nx_n}}{1 + e^{\\beta_0 + B_1x_1 + B_2x_2 +... + B_nx_n}}$$'),
                             
                         tags$p('In logistic regression the logit function links the probability to a linear combination of the parameters:'),
                             
                         withMathJax('$$logit(p) = log(\\frac{p}{1-p})$$'),
                         
                        tags$p('It can be shown that:'),
                             
                         withMathJax(' $$log(\\frac{p}{1-p}) = \\beta_0 + \\beta_1x_1 + \\beta_2x_2 + ... + \\beta_nx_n + \\epsilon$$'),
                             
                            
                        tags$h4('Benefits'),
                        tags$ul(
                            tags$li('Ease of Interpretability'),
                            tags$li('Ease of Implementation'),
                            tags$li('Allows for responses from non-normal distributions')
                            
                        ),
                        
                        tags$h4('Drawbacks'),
                        tags$ul(
                            tags$li('Assumes The predictor variables are linearly related to the log odds/logit'),
                            tags$li('Assumes absence of Multicollinearity'),
                            tags$li('Assumes absence of strong influential outliers')
                            
                        ),
                        
                         
                         ),
                        
                        #Display information about Random Forest
                        column(3, 
                               
                               tags$h3('Random Forest'),
                               
                               tags$p(tags$a(href = 'https://en.wikipedia.org/wiki/Random_forest', 'Random forest'), 'is an ensemble tree based method.' ),
                               
                               tags$p('Tree based methods split up the predictor space into different regions and provide predictions based on those regions (usually the mean for regression, and the most prevalent value for classification. 
                                 They are fit using recursive binary splitting.'),
                               
                               tags$p('For every possible value of each predictor the Residual Sum of Squares is found such that:'),
                                    withMathJax('$$R_1(j,s)=\\{x|x_j<s\\}$$'),
                                    withMathJax('$$R_2(j,s)=\\{x|x_j\\geq s\\}$$'),
                               
                               tags$p('j and s are found such that they minimize the equation:'),
                                    withMathJax('$$\\sum_{i:x_i\\in R_1(j,s)}(y_i - \\bar{y}_{R_1})^2 + \\sum_{i:x_i\\in R_2(j,s)}(y_i - \\bar{y}_{R_2})^2$$'),
                    
                  
                                tags$p('In a Random Forest model, the "forest" is composed of a large number of individual decision trees, each contributing to the final output.'),

                               
                               tags$p('Bootstrap Sampling is used to generate multiple subsets of the training data through random selection with replacement, ensuring each subset is the same size as the original dataset.
                                 Many Trees are then fit to these samples. Each tree is constructed with a random subset of the parameters. This random subset helps to reduce variability of the model'),
                               
                               tags$p('Random Forest employs an ensemble approach that combines all of the outcomes of the individual trees to make a final prediction. 
                                      In classification this is typically a majority vote among the trees. In regression this is typically an average of all of the tree outcomes.'),
                               
                                  
                               tags$h4('Benefits'),
                               tags$ul(
                                   tags$li('Does not assume a particular distribution or linearity'),
                                   tags$li('Robust to outliers'),
                                   tags$li('It is not necessary to include interaction terms as the trees will "find" them on their own'),
                                   tags$li('Improved accuracy over linear methods'),
                                   tags$li('Reduced variance over individual decision trees'),
                                   tags$li('Less prone to overfitting')
                                   
                               ),
                               
                               tags$h4('Drawbacks'),
                               tags$ul(
                                   tags$li('The end model is more difficult to interpret (though variable importance can be extracted)'),
                                   tags$li('Can be computationally expensive'),
                                   tags$li('Can be biased towards more dominant classes particularly in imbalanced datasets')
                               
                               )
                        )),
                tabPanel("Model Fitting",
                         
                        
                    fluidRow(
                         
                        #Display the Model fit Statistics for the Logistic Regression cross validation and testing
                        column(4, h4("Logistic Regression"),
                        tags$b("Fit Statistics"),
                        tableOutput('LR'),
                        tags$b("Coefficients"),
                        tableOutput('coeff_LR'),
                        tags$b("Test Statistics"),
                        tags$style(type='text/css', '#confMatLR {background-color: rgba(0,0,0,0); color: white;}'),
                        verbatimTextOutput('confMatLR')
                         ),
                        
                        #Display the Model fit Statistics for the Random Forest cross validation and testing
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
                                
                                    #Logistic Regression - Allow the user to enter values for any predictor variable
                                    #Each variable widget is shown only if it was selected in the model that was fit
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
                                       #allow the user to predict when pressing the button
                                       actionButton("predict_logit", "Predict Logistic Regression", class = "btn-danger"),    
                                    h4("Logisitc Regression Prediction:"),
                                    textOutput('predictionLR')
                                   ),
                         
                         column(3, h4("Random Forest"),
                                
                                #Random Forest - Allow the user to enter values for any predictor variable
                                #Each variable widget is shown only if it was selected in the model that was fit
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
                                #allow the user to predict when pressing the button
                                actionButton("predict_rf", "Predict Random Forest", class = "btn-danger"), 
                                h4("Random Forest Prediction:"),
                                textOutput('predictionRF')
                                
                                )
                
                )
                       
                        
                         
                         
    ))
              
                   
     
)))

