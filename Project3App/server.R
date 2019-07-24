library(shiny)
library(tidyverse)

ascentData <- as.tbl(read_csv("../ascentDataSample.csv"))
routeData <- as.tbl(read_csv("../routeData.csv"))

# Define server logic
shinyServer(function(input, output) {
  output$table <- renderTable({
    routeData %>% select(name, climb_type, difficulty, crag, sector, country, rating)
  })
})
