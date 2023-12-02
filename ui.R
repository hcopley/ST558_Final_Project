library(shinydashboard)

dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("circle-info")),
            menuItem("Data Exploration", tabName = "data_exploration", icon = icon("compass")),
            menuItem("Modeling", tabName = "model", icon = icon("brain"),
                     startExpanded = FALSE,
                     menuSubItem("Modeling Info",tabName = "model_info"),
                     menuSubItem("Model Fitting",tabName = "model_fit"),
                     menuSubItem("Prediction",tabName = "model_predict")
                     
                     )
        )
    ),
    dashboardBody( dashboardBody(
        tabItems(
            tabItem(tabName = "about", h2("About")),
            tabItem(tabName = "data_exploration", h2("Data Exploration")),
            tabItem(tabName = "model", h2("Modeling")),
            tabItem(tabName = "model_info", h2("Modeling")),
            tabItem(tabName = "model_fit", h2("Modeling")),
            tabItem(tabName = "model_prediction", h2("Modeling"))
            )
        ))
)