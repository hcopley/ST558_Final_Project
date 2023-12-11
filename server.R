
dat <- read_csv('heart_failure_clinical_records_dataset.csv') %>%
    mutate_at(vars(anaemia, diabetes, high_blood_pressure, smoking, DEATH_EVENT), 
              ~factor(.,levels = c(0,1), labels = c('Yes', 'No'))) %>%
    mutate(sex = factor(sex, levels = c(0,1), labels = c('Female', 'Male')))


cat_vars <- dat %>%
    select_if(is.factor) %>%
    colnames()

num_vars <- dat %>%
    select_if(is.numeric) %>%
    colnames()

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
        
        if(input$filter_var %in% cat_vars) {
            
            choices <- dat %>% 
                pull(!!sym(input$filter_var)) %>%
                unique()
            
            updateSelectizeInput(inputId = "filter_criteria", choices = choices)
            
        } else if (input$filter_var %in% num_vars) {
            
            range <- dat %>%
                select(!!sym(input$filter_var)) %>%
                summarise(min = min(!!sym(input$filter_var)), max = max(!!sym(input$filter_var)))
            
            updateSliderInput(inputId = "filter_slider", min = range$min, max = range$max, value = c(range$min, range$max))
        }
        
        
    })
    
    #update plot vars selectors----
    observe({
        
        switch(input$plot_type,
               
               "bar" = { updateSelectizeInput(inputId = "x", choices = c(cat_vars))
                   updateSelectizeInput(inputId = "color", choices = c("none", cat_vars)) 
               },
               
               "box" = { updateSelectizeInput(inputId = "x", choices = c("none", cat_vars))
                        updateSelectizeInput(inputId = "color", choices = c("none", cat_vars)) 
                        },
                "scatter" = {
                    updateSelectizeInput(inputId = "x", choices =  num_vars)
                        updateSelectizeInput(inputId = "color", choices = c("none", colnames(dat)))
                    
                        },
                "density" = {
                    updateSelectizeInput(inputId = "x", choices =  num_vars)
                    updateSelectizeInput(inputId = "color", choices = c("none", cat_vars))
                        }
                )
        })
    
    #plots-----    
    
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
    
    boxplots <-   renderPlotly({
        
        newData <- getData()
        
        
        b <- ggplot(newData, aes(y = !!sym(input$y)))
        
        if(input$x != 'none') {
            
            if(input$color == 'none') {
            
            b <- b + geom_boxplot(aes(x = !!sym(input$x)))
            
            } else {
                
                b <- b + geom_boxplot(aes(x = !!sym(input$x), fill = !!sym(input$color)))
            }
        
        } else {
            
            if(input$color == 'none') {
                
                b <- b + geom_boxplot()
                
            } else {
                
                b <- b + geom_boxplot(aes(x = !!sym(input$color), fill = !!sym(input$color)))
            }
            
        }
        
        if(input$facet != 'none' & input$x == 'none' & input$color == 'none') {
            
            b <- ggplot(newData, aes(y = !!sym(input$y))) + 
                geom_boxplot(aes(x = !!sym(input$facet))) + 
                facet_wrap(input$facet, labeller = label_both) 
                
            
        } else if (input$facet != 'none') {
            
            b <- b + facet_wrap(input$facet, labeller = label_both)
            
        }
        
   
        b %>% ggplotly() %>% layout(boxmode = "group")
        
    })
    
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
    
    getCV <- reactive({
        
        fitControl <- trainControl(
            method = "repeatedcv",
            number = input$number,
            repeats = input$repeats)
        
        fitControl
        
    })

    trainLR <- eventReactive(input$fit, {
        
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Fitting Logistic Regression Model", value = 0)
        
        train_set <- getTrain() %>%
            select_at(c('DEATH_EVENT', input$var_logit))
        
        set.seed(42)
        log_reg <- train(DEATH_EVENT ~ ., 
                            data = train_set, 
                            method = 'glm', 
                         family = 'binomial',
                         trControl = getCV()) 
        
        log_reg
        
    })
    
    trainRF <- eventReactive(input$fit, {
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Fitting Random Forest Model", value = 0)
        
        train_set <- getTrain() %>%
            select_at(c('DEATH_EVENT', input$var_rf))
        
        set.seed(42)
        random_forest <- train(DEATH_EVENT ~ .,
                               data = train_set,
                               method = "rf",
                               trControl = getCV(),
                               tuneGrid = expand.grid(mtry = input$mtry_range[1]:input$mtry_range[2]))
        
        random_forest
        
    })

    output$LR <- renderTable({
        
        trainLR()$results
        
        
        
    })
    
    output$RF <- renderTable({
        
        trainRF()$results
        
    })
    
   
    output$coeff_LR <- renderTable({
        
        summary(trainLR())$coefficients %>% 
            data.frame() %>% 
            rownames_to_column('Variable')
        
    })
    
    output$var_imp_RF <- renderTable({
        
        varImp(trainRF())$importance %>%
            arrange(-Overall) %>%
            rename('Overall Importance' = Overall) %>%
            rownames_to_column('Variable')
    })
    
    output$confMatRF <- renderPrint({
        
        test_pred <- predict(trainRF(), newdata = getTest())
        conf_mat <- confusionMatrix(test_pred, getTest()$DEATH_EVENT, positive = 'Yes')
        
        conf_mat
        
    })

    output$confMatLR <- renderPrint({
        
        test_pred <- predict(trainLR(), newdata = getTest())
        conf_mat <- confusionMatrix(test_pred, getTest()$DEATH_EVENT, positive = 'Yes')
     
        conf_mat
        
    })
    

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
    
    output$predictionLR <- renderText({
        
        test_pred <- predict(trainLR(), newdata = userValuesLR()) %>%
            levels(dat$DEATH_EVENT)[.]
        
        paste0('Predicted Death Event = ' , test_pred)
        
    })
    
    output$predictionRF <- renderText({
        
        test_pred <- predict(trainRF(), newdata = userValuesRF()) %>%
            levels(dat$DEATH_EVENT)[.]
        
        paste0('Predicted Death Event = ' , test_pred)
        
    })
    
    
      
}