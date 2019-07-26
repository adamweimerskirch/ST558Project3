library(shiny)
library(tidyverse)

ascentData <- as.tbl(read_csv("../ascentDataSample.csv"))
routeData <- as.tbl(read_csv("../routeData.csv"))

g <- ggplot(ascentDataSample)

# Define server logic
shinyServer(function(input, output) {
  
  output$table <- renderTable({
    routeData %>% select(name, climb_type, difficulty, crag, sector, country, rating)
  })

  if(typeof(ascentDataSample$grade_id) == "int"){
    output$EDAPlot <- renderPlot({
      g + geom_histogram(aes(x = ascentDataSample$grade_id))
    })
  } else {
    output$EDAPlot <- renderPlot({
      ggpareto(ascentDataSample$usa_routes)
    })
  }
  
})
