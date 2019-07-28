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
                menuItem("Country Summary", tabName = "country", icon = icon("globe-americas")),
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
                            box("Search for routes in various countries, crags, and sectors")
                        ),

                        
                        fluidRow(
                            box(dataTableOutput("routeTable"))
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
                                           choices = names(ascentData))
                        ),

                        fluidRow(
                            box(plotOutput("EDAPlot"))
                        )
                ),
                
                ########################################################
                # region summary tab
                tabItem(tabName = "country",
                        fluidRow(
                            box("route summary statistics by region")
                        ),

                        fluidRow(
                            box(
                                uiOutput("routeCountry")#,
                                # uiOutput("routeCrag"),
                                # uiOutput("routeSector")
                            )
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
                # Modeling Tab
                tabItem(tabName = "model",
                        fluidRow(
                            box("data models, at least two supervised learning models, user functionality to change user settings, and prediction")
                        ),
                        #row for prediction
                        fluidRow(
                            #user prediction inputs
                            box(
                                # selectizeInput("predictSex", "Sex", c("Male","Female")),
                                # uiOutput("predictHeight"),
                                # uiOutput("predictWeight"),
                                numericInput("predictSex", "Sex (0 = M, 1 = F)",
                                             value = 0,
                                             min = 0, max = 1),
                                numericInput("predictHeight", "Height",
                                             value = round(median(peakGrade$height),0),
                                             min = 150, max = 200, step = 5),
                                numericInput("predictWeight", "Weight",
                                             value = round(median(peakGrade$weight),0),
                                             min = 40, max = 100, step = 5),
                                numericInput("predictExp", "Years of Experience",
                                             value = round(median(peakGrade$exp),0),
                                             min = 1, max = 30),
                                actionButton("makePrediction", "Predict Peak Grade",
                                             icon = icon("eye"))
                            ),
                            #prediction result
                            box(
                                textOutput("treePredict"),
                                textOutput("rfPredict")
                            )
                        )
                )
            )
        )
    )
))
