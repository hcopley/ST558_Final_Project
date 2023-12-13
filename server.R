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

#start the server
server <- function(input, output, session) { 
    
    #get data for exploration----
    getData <- reactive({
        
        newData <- dat
        
        if(input$filter_var %in% cat_vars) {
        
            newData <- newData %>%
                filter(!!sym(input$filter_var) == input$filter_criteria)
        } else if  (input$filter_var %in% num_vars) {
            
          newData <- newData %>%
                filter(between(!!sym(input$filter_var), input$filter_slider[1], input$filter_slider[2] ))
            
        }
        
        newData
        
    })
    

    #update filter selectors----
    observe({
        
        #if the filter variable is categorical
        if(input$filter_var %in% cat_vars) {
            
            choices <- dat %>% 
                pull(!!sym(input$filter_var)) %>%
                unique()
            
            #update the filter selector to the factor levels available 
            updateSelectizeInput(inputId = "filter_criteria", choices = choices)
        
        #if the filter variable is numeric    
        } else if (input$filter_var %in% num_vars) {
            
            range <- dat %>%
                select(!!sym(input$filter_var)) %>%
                summarise(min = min(!!sym(input$filter_var)), max = max(!!sym(input$filter_var)))
            
            #update the slider input to limit to the minimum and maximum value of that variable
            updateSliderInput(inputId = "filter_slider", min = range$min, max = range$max, value = c(range$min, range$max))
        }
        
        
    })
    
    #update plot vars selectors----
    observe({
        
        switch(input$plot_type,
               
               #if the plot selected is a bar limit the choice of x and color to categorical
               "bar" = { updateSelectizeInput(inputId = "x", choices = c(cat_vars))
                   updateSelectizeInput(inputId = "color", choices = c("none", cat_vars)) 
               },
               
               #if the plot selected is boxplot limit the choice of x and color to categorical
               "box" = { updateSelectizeInput(inputId = "x", choices = c("none", cat_vars))
                        updateSelectizeInput(inputId = "color", choices = c("none", cat_vars)) 
                        },
               #if the plot selected is scatter limit the choice of x to numerical and allow any variable for color
                "scatter" = {
                    updateSelectizeInput(inputId = "x", choices =  num_vars)
                        updateSelectizeInput(inputId = "color", choices = c("none", colnames(dat)))
                    
                        },
               #if the plot selected is density limit the choice of x to numerical and color to categorical
                "density" = {
                    updateSelectizeInput(inputId = "x", choices =  num_vars)
                    updateSelectizeInput(inputId = "color", choices = c("none", cat_vars))
                        }
                )
        })
    
    #plots-----    
    
    #render the barplot
    bar <-   renderPlotly({
        
        req(input$x != 'none')
        
        #get user selections and exclude "none"
        vec <- c(input$x, input$color, input$facet) %>%
            .[!.=='none']
        
        #get filtered data
        newData <- getData() %>%
            select_at(vec) %>%
            mutate(count = 1) %>%
            group_by_at(vec) %>%
            summarise(count = sum(count))
        
        #create initial bar plot with x & y
        p <- ggplot(newData, aes(x = !!sym(input$x), y = count)) 
        
        
        #add color variable if included
        if(input$color != 'none') {
            
            p <- p + geom_bar(stat = 'identity', aes(fill = !!sym(input$color)))
            
        } else {
            
            p <- p + geom_bar(stat = 'identity')
        }
        
        #add facet variable if included
        if(input$facet != 'none') {
            
            p <- p + facet_wrap(input$facet, labeller = label_both)
            
        }
        p
        
    })
    
    #render the scatter plot
    scatter <-   renderPlotly({
        
        
        req(input$x != 'none')
        
        #get filtered data
        newData <- getData()
        
        
        
        #create initial scatter plot with x & y
        s <- ggplot(newData, aes(x = !!sym(input$x), y = !!sym(input$y))) 
        
        
        #add color variable if included
        if(input$color != 'none') {
        
        s <- s + geom_point(aes(col = !!sym(input$color)))
        
        } else {
            
            s <- s + geom_point()
        }
        
        #add facet variable if included
        if(input$facet != 'none') {
        
        s <- s + facet_wrap(input$facet, labeller = label_both)
            
        }
       s
        
    })
    
    #render the boxplot
    boxplots <-   renderPlotly({
        
        newData <- getData()
        
        
        b <- ggplot(newData, aes(y = !!sym(input$y)))
        
        #add the x variable if included
        if(input$x != 'none') {
          
            #add the color variable if included  
            if(input$color == 'none') {
            
            b <- b + geom_boxplot(aes(x = !!sym(input$x)))
            
            } else {
                
                b <- b + geom_boxplot(aes(x = !!sym(input$x), fill = !!sym(input$color)))
            }
        
        } else {
            
            #if x is none and color is none just output the boxplot
            if(input$color == 'none') {
                
                b <- b + geom_boxplot()
            
            #if x none, but color is not none output the boxplot with color and the color as the x variable    
            } else {
                
                b <- b + geom_boxplot(aes(x = !!sym(input$color), fill = !!sym(input$color)))
            }
            
        }
        
        #if a facet is selected but no x or color are selected output a boxplot with the facet as the x variable and the facet
        if(input$facet != 'none' & input$x == 'none' & input$color == 'none') {
            
            b <- ggplot(newData, aes(y = !!sym(input$y))) + 
                geom_boxplot(aes(x = !!sym(input$facet))) + 
                facet_wrap(input$facet, labeller = label_both) 
                
        #if a facet is selected but an x or a color are selected add the facet to the plot    
        } else if (input$facet != 'none') {
            
            b <- b + facet_wrap(input$facet, labeller = label_both)
            
        }
        
        #return the plot (boxmode = "group" ensures that the boxes dodge as needed)
        b %>% ggplotly() %>% layout(boxmode = "group")
        
    })
    
    #render the density plot
    density <-   renderPlotly({
        #get filtered data
        newData <- getData()
        
        req(input$x != 'none')
            
            d <- ggplot(newData, aes(!!sym(input$x))) 
       
            
        
        #add color variable if included
        if(input$color == 'none') {
        
           
            d <- d + stat_density(geom = 'line')
            
        } else {
            
            d <- d + stat_density(geom = 'line', aes(fill = !!sym(input$color), col = !!sym(input$color) ))
        }
        
        #add facet variable if included
        if(input$facet != 'none') {
            
            d <- d + facet_wrap(input$facet, labeller = label_both)
            
        }
        d
        
    })
    
    #output the rendered plot that was selected by the user
    output$plot <- reactive({
        
        switch(input$plot_type,
        
            'scatter' = scatter(),
            'box' = boxplots(),
            'density' = density(),
            'bar' = bar()
    )
    
    })
    
    

    
    #get data splits----
    getPartition <- reactive({
        
        set.seed(42)
        partition <- createDataPartition(dat$DEATH_EVENT, p = input$split/100, list = FALSE) 
        
        partition
    
    })
    
    
    splitTrain <- reactive({
        
        training <- dat[getPartition(),]
        
        })
    
    
    splitTest <- reactive({
        
        testing <- dat[-getPartition(),]
    
        })
    
    #preprocess the training and test sets
    preProc <- reactive({
        
        preProcess(splitTrain(), method = c("center", "scale"))
        
    })
    
    getTrain <- reactive({
        
        predict(preProc(), splitTrain())
        
    })
    
    getTest <- reactive({
        
        predict(preProc(), splitTest())
        
    })
    
    #when the fit button is clicked select the Model fitting tab for viewing
    observeEvent(input$fit, {
        updateTabsetPanel(session, "tabset", selected = "Model Fitting")
    }
    )
    
    #create the CV method with the folds and repeats selected
    getCV <- reactive({
        
        fitControl <- trainControl(
            method = "repeatedcv",
            number = input$number,
            repeats = input$repeats)
        
        fitControl
        
    })

    #fit the logistic regression model to the training data when the user presses the fit button
    trainLR <- eventReactive(input$fit, {
        
        
        # Show a progress bar
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Fitting Logistic Regression Model", value = 0)
        
        #get the training set and select the variables selected by the user
        train_set <- getTrain() %>%
            select_at(c('DEATH_EVENT', input$var_logit))
        
        #fit the model
        set.seed(42)
        log_reg <- train(DEATH_EVENT ~ ., 
                            data = train_set, 
                            method = 'glm', 
                         family = 'binomial',
                         trControl = getCV()) 
        
        log_reg
        
    })
    
    #fit the random forest model to the training data when the user presses the fit button
    trainRF <- eventReactive(input$fit, {
        
        # Show a progress bar
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Fitting Random Forest Model", value = 0)
        
        #get the training set and select the variables selected by the user
        train_set <- getTrain() %>%
            select_at(c('DEATH_EVENT', input$var_rf))
        
        #fit the model
        set.seed(42)
        random_forest <- train(DEATH_EVENT ~ .,
                               data = train_set,
                               method = "rf",
                               trControl = getCV(),
                               tuneGrid = expand.grid(mtry = input$mtry_range[1]:input$mtry_range[2]))
        
        random_forest
        
    })

    #output the fitted logistic regression results
    output$LR <- renderTable({
        
        trainLR()$results
        
        
        
    })
    
    #output the fitted random forest results
    output$RF <- renderTable({
        
        trainRF()$results
        
    })
    
   
    #output the coefficients of the logistic regression model
    output$coeff_LR <- renderTable({
        
        summary(trainLR())$coefficients %>% 
            data.frame() %>% 
            rownames_to_column('Variable')
        
    })
    
    #output the variable importance for the random forest model
    output$var_imp_RF <- renderTable({
        
        varImp(trainRF())$importance %>%
            arrange(-Overall) %>%
            rename('Overall Importance' = Overall) %>%
            rownames_to_column('Variable')
    })
    
    #output the confusion matrix for the logistic regression model
    output$confMatLR <- renderPrint({
        
        test_pred <- predict(trainLR(), newdata = getTest())
        conf_mat <- confusionMatrix(test_pred, getTest()$DEATH_EVENT, positive = 'Yes')
        
        conf_mat
        
    })
    
    #output the confusion matrix for the random forest model
    output$confMatRF <- renderPrint({
        
        test_pred <- predict(trainRF(), newdata = getTest())
        conf_mat <- confusionMatrix(test_pred, getTest()$DEATH_EVENT, positive = 'Yes')
        
        conf_mat
        
    })

   
    
    #gather all of the user entered values into a dataframe for logistic regression prediction
    userValuesLR <- eventReactive(input$predict_logit, {
        
            tibble('age' = input$age_val_lr,
                   'anaemia' = input$anaemia_val_lr,
                   'creatinine_phosphokinase' = input$creatinine_phosphokinase_val_lr,
                   'diabetes' = input$diabetes_val_lr,
                   'ejection_fraction' = input$ejection_fraction_val_lr,
                   'high_blood_pressure' = input$high_blood_pressure_val_lr,
                   'platelets' = input$platelets_val_lr,
                   'serum_creatinine' = input$serum_creatinine_val_lr,
                   'serum_sodium' = input$serum_sodium_val_lr,
                   'sex' = input$sex_val_lr,
                   'smoking' = input$smoking_val_lr,
                   'time' = input$time_val_lr)
    })
    
    #gather all of the user entered values into a dataframe for random forest prediction
    userValuesRF <- eventReactive(input$predict_rf, {
        
        tibble('age' = input$age_val_rf,
               'anaemia' = input$anaemia_val_rf,
               'creatinine_phosphokinase' = input$creatinine_phosphokinase_val_rf,
               'diabetes' = input$diabetes_val_rf,
               'ejection_fraction' = input$ejection_fraction_val_rf,
               'high_blood_pressure' = input$high_blood_pressure_val_rf,
               'platelets' = input$platelets_val_rf,
               'serum_creatinine' = input$serum_creatinine_val_rf,
               'serum_sodium' = input$serum_sodium_val_rf,
               'sex' = input$sex_val_rf,
               'smoking' = input$smoking_val_rf,
               'time' = input$time_val_rf)
    })
    
    #predict using the logistic regression model on the values entered by the user
    output$predictionLR <- renderText({
        
        test_pred <- predict(trainLR(), newdata = userValuesLR()) %>%
            levels(dat$DEATH_EVENT)[.]
        
        paste0('Predicted Death Event = ' , test_pred)
        
    })
    
    #predict using the random forest model on the values entered by the user
    output$predictionRF <- renderText({
        
        test_pred <- predict(trainRF(), newdata = userValuesRF()) %>%
            levels(dat$DEATH_EVENT)[.]
        
        paste0('Predicted Death Event = ' , test_pred)
        
    })
    
    
      
}