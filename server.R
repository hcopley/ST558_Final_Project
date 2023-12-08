dat <- read_csv('heart_failure_clinical_records_dataset.csv')

server <- function(input, output, session) { 
    
    
    partition_data <- function(.data, model) {
        
        set.seed(42)
        partition <- createDataPartition(dat$DEATH_EVENT, p = input$split/100, list = FALSE) 
        
        train_set <- dat[partition,] 
        
        test_set <- dat[-partition,]
        
        variables <- switch(model, 
               'rf' = input$var_rf,
               'logit' = input$var_logit
               )
        
        list(train_set, test_set)
    }
    
    fit_dat <- observeEvent(input$fit, {
        set.seed(42)
        partition <- createDataPartition(dat$DEATH_EVENT, p = input$split/100, list = FALSE) 
        
        train_set <- dat[partition,] 
        
        test_set <- dat[-partition,]
        
        train_rf <- train_set %>%
            select(input$var_rf)
        
        train_logit <- train_set %>%
            select(input$var_logit)
        
        
        
    })
    
   
    #split data into train/test
    output$split_cols <- renderText({
        
        get_dat()
        
        paste0("There are ", nrow(train_set)," observations in the training set and ", nrow(test_set), " observations in the test set.")
        
    })
    
    
    
    output$train_rf_cols <- renderText({
        
        get_dat()
        
        train_set %>%
            colnames()
    
    })
    
    
    }
