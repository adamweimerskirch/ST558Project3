library(shiny)
library(shinydashboard)

# infoTabTest <- source("infoTabTest.R", local = TRUE)

# define UI
shinyUI(fluidPage(
    
    dashboardPage(
        dashboardHeader(title = "Climbing Data"),
        
        # sidebar
        dashboardSidebar(
            sidebarMenu(
                menuItem("Information", tabName = "info", icon = icon("info")),
                menuItem("Route List", tabName = "table", icon = icon("table")),
                menuItem("Data Exploration", tabName = "eda", icon = icon("calculator")),
                menuItem("Cluster Analysis", tabName = "cluster", icon = icon("chart-bar")),
                menuItem("Modeling", tabName = "model", icon = icon("chart-line"))
            )
        ),
        
        # body
        dashboardBody(
            tabItems(
                ########################################################
                #information tab
                tabItem(tabName = "info",
                        fluidRow(
                            box("information about dataset and abilities of the app, must have two dynamic UI elements, button to save plot to a file, option to save data currently being used for a plot to a csv, click / select region, math type, link to something, formatted text")
                            )
                        ),
                
                # source("../infoTabTest.R", local = TRUE),
                
                ########################################################
                # data tab
                tabItem(tabName = "table",
                        fluidRow(
                            box("data table with relevant subsetting", "allow multiselect")
                        ),
                        
                        fluidRow(
                            selectizeInput("tableCountry",
                                           "Select route country",
                                           choices = stri_trans_toupper(ascentDataSample$country),
                                           multiple = FALSE,
                                           selected = ),

                            selectizeInput("tableCrag",
                                           "Select route crag",
                                           choices = stri_trans_toupper(ascentDataSample$crag),
                                           multiple = FALSE),

                            selectizeInput("tableSector",
                                           "Select route sector",
                                           choices = stri_trans_toupper(ascentDataSample$sector),
                                           multiple = FALSE)

                        ),
                        
                        fluidRow(
                            box(tableOutput("table"))
                            )
                        ),
                
                ########################################################
                # data exploration tab
                tabItem(tabName = "eda",
                        fluidRow(
                            box("data exploration, user creates common numeric and graphical summaries", "create histogram for numeric, pareto for categorical")
                            ),

                        fluidRow(
                            selectizeInput("EDAVar",
                                           "Choose variable for EDA",
                                           choices = names(ascentDataSample))
                        ),

                        fluidRow(
                            box(plotOutput("EDAPlot"))
                        )
                ),
                
                ########################################################
                # cluster analysis tab
                tabItem(tabName = "cluster",
                        fluidRow(
                            box("cluster analysis")
                            )
                        ),
                
                ########################################################
                # modeling tab
                tabItem(tabName = "model",
                        fluidRow(
                            box("data models, at least two supervised learning models, user functionality to change user settings, and prediction")
                        )
                )
            )
        )
    )
))
