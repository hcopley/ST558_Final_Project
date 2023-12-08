
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
    
    
    getData <- reactive({
        newData <- dat
    })
    
    
    
    
    observe({
        
        switch(input$plot_type,
               
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
        
    
    scatter <-   renderPlotly({
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
        
        
        if(input$x == 'none') {
            
            b <- ggplot(newData, aes(y = !!sym(input$y))) 
        } else {
            
            b <- ggplot(newData, aes(x = !!sym(input$x), y = !!sym(input$y))) 
            
        }
        
        #add color variable if included
        if(input$color == 'none') {
            
            b <- b + geom_boxplot()
            
        } else {
            
            b <- b + geom_boxplot(aes(fill = !!sym(input$color)))
            
        }
        
        #add facet variable if included
        if(input$facet != 'none') {
            
            b <- b + facet_wrap(input$facet, labeller = label_both)
            
        }
        b %>% ggplotly() %>% layout(boxmode = "group")
        
    })
    
    
    density <-   renderPlotly({
        #get filtered data
        newData <- getData()
        
            
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
    
    output$plot <- reactive({switch(input$plot_type,
        
        'scatter' = scatter(),
        'box' = boxplots(),
        'density' = density()
    )
    
    })
}




#partition_data <- function(.data, model) {
#    
#    set.seed(42)
#    partition <- createDataPartition(dat$DEATH_EVENT, p = input$split/100, list = FALSE) 
#    
#    train_set <- dat[partition,] 
#    
#    test_set <- dat[-partition,]
#    
#    variables <- switch(model, 
#                        'rf' = input$var_rf,
#                        'logit' = input$var_logit
#    )
#    
#    list(train_set, test_set)
#}
#
#fit_dat <- observeEvent(input$fit, {
#    set.seed(42)
#    partition <- createDataPartition(dat$DEATH_EVENT, p = input$split/100, list = FALSE) 
#    
#    train_set <- dat[partition,] 
#    
#    test_set <- dat[-partition,]
#    
#    train_rf <- train_set %>%
#        select(input$var_rf)
#    
#    train_logit <- train_set %>%
#        select(input$var_logit)
#    
#    
#    
#})
#
#
##split data into train/test
#output$split_cols <- renderText({
#    
#    get_dat()
#    
#    paste0("There are ", nrow(train_set)," observations in the training set and ", nrow(test_set), " observations in the test set.")
#    
#})
#
#
#
#output$train_rf_cols <- renderText({
#    
#    get_dat()
#    
#    train_set %>%
#        colnames()
#    
#})