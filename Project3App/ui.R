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
                menuItem("Data Exploration", tabName = "eda", icon = icon("chart-bar")),
                menuItem("Crag Summary", tabName = "crag", icon = icon("globe-americas")),
                menuItem("Peak Grade Model", tabName = "peak", icon = icon("chart-line"))
            )
        ),
        
        # body
        dashboardBody(
            tabItems(
                ########################################################
                ### Information Tab
                tabItem(tabName = "info",
                        fluidRow(
                            box("information about dataset and abilities of the app, must have two dynamic UI elements, button to save plot to a file, option to save data currently being used for a plot to a csv, click / select region, math type, link to something, formatted text")
                            )
                        ),
                
                # source("../infoTabTest.R", local = TRUE),
                
                ########################################################
                ### Route Data Tab
                tabItem(tabName = "table",
                        fluidRow(
                            box("Search for routes in various countries, crags, and sectors")
                        ),

                        
                        fluidRow(
                            box(dataTableOutput("routeTable"))
                            )
                        ),
                
                ########################################################
                ### EDA Tab
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
                ### Crag Summary Tab
                tabItem(tabName = "crag",
                        #row for page description
                        fluidRow(
                            box(h4("Use this tab to find other crags similar to your favorites!  This tab uses k-means clustering to group similar crags by route mix (bouldering vs. roped routes), difficulty, and rating."))
                        ),
                        #row for user input and cluster summary
                        fluidRow(
                            box(
                                sliderInput("nClust", "Number of Clusters",
                                            min = 1, max = 10, value = 5, step = 1),
                                actionButton("calcClust", "Go!")
                            ),
                            box(tableOutput("clustTable"))
                        ),
                        #row for cluster plot
                        fluidRow(
                            box(
                                selectizeInput("cragCountry", "Filter Plot by Country",
                                               choices = stri_sort(cragSummary$cragCountry),
                                               selected = "USA"),
                                uiOutput("clustFilter"),
                                plotOutput("clustPlot")
                            )
                        )
                ),
                
                ########################################################
                ### Peak Grade Model Tab
                tabItem(tabName = "peak",
                        fluidRow(
                            box("data models, at least two supervised learning models, user functionality to change user settings, and prediction")
                        ),
                        #row for modeling
                        fluidRow(
                          box(
                              selectizeInput("independent", "Specify Variables for Custom Model",
                                             choices = names(peakGrade[2:5]),
                                             multiple = TRUE),
                              #checkboxInput("interaction", "Include Interaction Terms"),
                              actionButton("fitUserModel", "Fit Custom Model"),
                              textOutput("userModelForm")
                          ),
                          box()
                        ),
                        #row for prediction
                        fluidRow(
                            #user prediction inputs
                            box(
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
                                textOutput("rfPredict"),
                                textOutput("userTreePredict")
                            )
                        )
                )
            )
        )
    )
))
