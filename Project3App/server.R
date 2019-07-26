library(shiny)
library(tidyverse)

ascentData <- as.tbl(read_csv("../ascentDataSample.csv"))
routeData <- as.tbl(read_csv("../routeData.csv"))

g <- ggplot(ascentDataSample)

# define server logic
shinyServer(function(input, output) {
  
  # filter route data
  routeDataFilter <- reactive({
    routeData %>% filter(country == input$tableCountry)#, sector == input$tableSector, crag == input$tableCrag)
  })
  
  # generate route data table
  output$table <- renderTable({
    routeDataFilter() %>% select(name, climb_type, difficulty, crag, sector, country, rating)
  })

  # generate EDA plot
  observe({
    if(typeof(input$EDAVar) == "character"){
      output$EDAPlot <- renderPlot({
        g + geom_histogram(aes(x = ascentDataSample$grade_id))
      })
    } else {
      output$EDAPlot <- renderPlot({
        ggpareto(ascentDataSample$usa_routes)
      })
    }
  })
})
