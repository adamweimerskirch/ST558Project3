library(shiny)
library(shinydashboard)
library(tidyverse)
library(randomForest)
library(tree)
library(caret)
library(rbenchmark)
library(stringi)

# infoTabTest <- source("infoTabTest.R", local = TRUE)

# define UI
shinyUI(fluidPage(
    
    dashboardPage(
        dashboardHeader(title = "Rock Climbing"),
        
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
                            box(width = 12,
    h3("This app allows the user to explore data about rock climbing routes around the world!"), br(),
    h2("Tabs"),
    h4("Route List", style = "color:blue"), "This tab allows the user to browse through the routes in the dataset, and to filter and subset by any of the provided variables.  Variables that would not be of interest to the typical user are not displayed for ease of viewing.", br(),
    h4("Data Exploration", style = "color:blue"), "This tab allows the user to create common numeric and graphical summaries of the variables in the dataset.", br(),
    h4("Crag Summary", style = "color:blue"), "This tab uses k-means clustering to group crags into similar clusters, and displays the results graphically by country.", br(),
    h4("Peak Grade Model", style = "color:blue"), "This tab uses supervised learning models to predict a climber's peak route difficulty grade, given a few predictor variables for the climber.", br(),
    h2("Data"), "The data in this set is from ",
        a("8a.nu", href="https://www.8a.nu/"),
        ", which is a website for Global Climbing News.  The dataset was compiled by David Cohen and posted to his ",
        a("Kaggle page", href="https://www.kaggle.com/dcohen21/8anu-climbing-logbook)"),
        ".  Each record in the dataset is a climb completed by an 8a.nu user and then logged on the site.  More detail about the variables in the dataset is provided on the Data Exploration tab."
                        )
                        )),
                
                ########################################################
                ### Route Data Tab
                tabItem(tabName = "table",
                        fluidRow(
                            box(width = 12,
                                h4("Search for routes in various countries, crags, and sectors."), style = "color:blue")
                        ),

                        
                        fluidRow(
                            box(width = 12,
                                dataTableOutput("routeTable"))
                            )
                        ),
                
                ########################################################
                ### EDA Tab
                tabItem(tabName = "eda",
                        fluidRow(
                            box(width = 12,
                                h4("Explore the data with numeric and graphical summaries for the variable of your choice.", style = "color:blue")
                                )
                            ),

                        fluidRow(
                            box(width = 4,
                                h4("Choose EDA Variable"),
                                uiOutput("EDAType"),
                                uiOutput("EDAVar")
                            ),
                            box(width = 8,
                                h4("Numeric Summary"),
                                tableOutput("EDASummary")
                            )
                        ),

                        fluidRow(
                            box(width = 6,
                                plotOutput("EDAPlot")),
                            box(width = 6,
                                h4("troubleshooting"),
                                verbatimTextOutput("EDAPrint"),
                                h5("I cannot for the life of me figure out why this is returning a list rather than a vector, and I cannot coerce.  This is preventing me from plotting.", style = "color:blue"))
                        )
                ),
                
                ########################################################
                ### Crag Summary Tab
                tabItem(tabName = "crag",
                        #row for page description
                        fluidRow(
                            box(width = 12,
                                h4("Use this tab to find other crags similar to your favorites!  This tab uses k-means clustering to group similar crags by route mix (bouldering vs. roped routes), difficulty, and rating.", style = "color:blue"))
                        ),
                        #row for user input and cluster summary
                        fluidRow(
                            box(width = 4,
                                h4("Select Number of Clusters"),
                                sliderInput("nClust", "Number of Clusters",
                                            min = 1, max = 10, value = 5, step = 1),
                                actionButton("calcClust", "Go!")
                            ),
                            box(width = 8,
                                h4("Cluster Summaries"),
                                tableOutput("clustTable"))
                        ),
                        #row for cluster plot
                        fluidRow(
                            box(width = 12,
                                h4("Visualize Clusters by Country"),
                                h5("Note: Try ESP and USA"),
                                selectizeInput("cragCountry", "Filter Plot by Country",
                                               choices = stri_sort(cragSummary$cragCountry),
                                               selected = "ESP"),
                                uiOutput("clustFilter"),
                                downloadButton("toCSV", "Download Plot Data to .csv"),
                                downloadButton("toPNG", "Download Plot Image to .png"),
                                plotOutput("clustPlot")
                            )
                        )
                ),
                
                ########################################################
                ### Peak Grade Model Tab
                tabItem(tabName = "peak",
                        fluidRow(
                            box(width = 12,
                                h4("Use this tab to predict your peak climbing grade at different years of experience!", style = "color:blue"))
                        ),
                        #row for modeling
                        fluidRow(
                          box(width = 6,
                              h4("Specify Custom Model Form"),
                              selectizeInput("independent", "Specify Variables for Custom Model",
                                             choices = names(peakGrade[2:5]),
                                             multiple = TRUE),
                              actionButton("fitUserModel", "Fit User Model"),
                              uiOutput("userModelFormMath")
                          ),
                          box(width = 6,
                              h4("Model Fit Comparison: RMSE"),
                              tableOutput("modelRMSE")
                              )
                        ),
                        #row for prediction
                        fluidRow(
                            #user prediction inputs
                            box(width = 6,
                                h4("Specify Prediction Inputs"),
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
                            box(width = 6,
                                h4("Peak Climbing Grade Predictions"),
                                tableOutput("modelPredict")
                            )
                        )
                )
            )
        )
    )
))
