library(shiny)
library(shinydashboard)
library(tidyverse)
library(randomForest)
library(tree)
library(caret)
library(rbenchmark)
library(stringi)

ascentData <- read_csv("../ascentDataSample.csv")

########################################################
### Information Tab Support

# url <- a("Google Homepage", href="https://www.google.com/")
# output$tab <- renderUI({
#   tagList("URL link:", url)
########################################################
### Route Data Tab Support
#create route data frame
routeData <- ascentData %>% select(name, climb_type, crag, sector, country, rating, grade_usa, comment)
routeData$climb_type <- routeData$climb_type %>% recode(`0` = "Route", `1` = "Boulder")
routeData$country[routeData$country == ""] <- "Not Specified"
routeData$crag[routeData$crag == ""] <- "Not Specified"
routeData$sector[routeData$sector == ""] <- "Not Specified"

########################################################
### EDA Tab Support
g <- ggplot(ascentData)

# function to generate pareto chart courtesy Davide Passaretti on RPubs
# https://rpubs.com/dav1d00/ggpareto

ggpareto <- function(x) {
  
  title <- deparse(substitute(x))
  
  x <- data.frame(modality = na.omit(x))
  
  library(dplyr)
  
  Df <- x %>% group_by(modality) %>% summarise(frequency=n()) %>% 
    arrange(desc(frequency))
  
  Df$modality <- ordered(Df$modality, levels = unlist(Df$modality, use.names = F))
  
  Df <- Df %>% mutate(modality_int = as.integer(modality), 
                      cumfreq = cumsum(frequency), cumperc = cumfreq/nrow(x) * 100)
  nr <- nrow(Df)
  N <- sum(Df$frequency)
  
  Df_ticks <- data.frame(xtick0 = rep(nr +.55, 11), xtick1 = rep(nr +.59, 11), 
                         ytick = seq(0, N, N/10))
  
  y2 <- c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%")
  
  library(ggplot2)
  
  g <- ggplot(Df, aes(x=modality, y=frequency)) + 
    geom_bar(stat="identity", aes(fill = modality_int)) +
    geom_line(aes(x=modality_int, y = cumfreq, color = modality_int)) +
    geom_point(aes(x=modality_int, y = cumfreq, color = modality_int), pch = 19) +
    scale_y_continuous(breaks=seq(0, N, N/10), limits=c(-.02 * N, N * 1.02)) + 
    scale_x_discrete(breaks = Df$modality) +
    guides(fill = FALSE, color = FALSE) + 
    annotate("rect", xmin = nr + .55, xmax = nr + 1, 
             ymin = -.02 * N, ymax = N * 1.02, fill = "white") +
    annotate("text", x = nr + .8, y = seq(0, N, N/10), label = y2, size = 3.5) +
    geom_segment(x = nr + .55, xend = nr + .55, y = -.02 * N, yend = N * 1.02, color = "grey50") +
    geom_segment(data = Df_ticks, aes(x = xtick0, y = ytick, xend = xtick1, yend = ytick)) +
    labs(title = paste0("Pareto Chart of ", title), y = "absolute frequency") +
    theme_bw()
  
  return(list(graph = g, Df = Df[, c(3, 1, 2, 4, 5)]))
}


########################################################
### Crag Summary Support

#function to return the modal value in an object
modalValue <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#create crag summary dataset
cragSummary <- ascentData %>% group_by(crag) %>%
  summarize(cragCountry = modalValue(country),
            ascentCount = n(),
            routeMix = mean(climb_type),
            avgRating = mean(rating),
            medGrade = median(grade_id)) %>%
  filter(ascentCount >= 2)

########################################################
### Peak Grade Tab Support

##create dataset of peak climbing grades for each user
#improvement: find peak bouldering and route grades separately
peakGrade <- ascentData %>%
  group_by(user_id) %>%
  summarize("height" = max(height),
            "weight" = max(weight),
            "sex" = max(sex),
            "exp" = max(year) - min(started),
            "nClimbs" = n(),
            "maxGrade" = max(grade_id),
            "maxScore" = max(total_score)) %>%
  filter(height > 100, weight > 40, exp > 0, exp < 100, maxGrade > 0, maxScore > 0)

#split into train and test sets  
train <- sample(1:nrow(peakGrade), nrow(peakGrade)*0.8)
test <- setdiff(1:nrow(peakGrade), train)
peakGradeTrain <- peakGrade[train, ]
peakGradeTest <- peakGrade[test, ]

##fit default models
#capture the time it took to fit the model
treeBench <- benchmark(
  #fit regression tree model
  treeFitDefault <- tree(maxGrade ~ height + weight + sex + exp,
                         data = peakGradeTrain)
  , replications = 1)

treeBench$elapsed
treeFitDefault

#capture the time it took to fit the model
rfBench <- benchmark(
  rfFitDefault <- randomForest(maxGrade ~ height + weight + sex + exp,
                             data = peakGradeTrain,
                             mtry = 2,
                             ntree = 200,
                             importance = TRUE)
  , replications = 1)

rfBench$elapsed
rfFitDefault

########################################################
### Server

# define server logic
shinyServer(function(input, output, session) {
  
  ########################################################
  ### Route List Tab
  # render route data table
  output$routeTable <- renderDataTable({
    routeData %>% select(name, sector, crag, country, climb_type, grade_usa, rating, comment)
  })

  ########################################################
  ### EDA Tab
  # render EDA plot
  observe({
    if(typeof(input$EDAVar) == "character"){
      output$EDAPlot <- renderPlot({
        g + geom_histogram(aes(x = ascentData$grade_id))
      })
    } else {
      output$EDAPlot <- renderPlot({
        ggpareto(ascentData$usa_routes)
      })
    }
  })
  
  ########################################################
  ### Crag Summary Tab
  
  #group crags into user-specified number of clusters
  cragSummaryClust <- reactive({
    input$calcClust
    
    #isolate so kmeans algorithm only updates on button push
    isolate({
      clusters <- kmeans(scale(cragSummary[-1:-3]), centers = input$nClust,
                         iter.max = 5, algorithm = "MacQueen")
      cragSummaryClust <- cragSummary %>% mutate("cluster" = as.factor(clusters$cluster))
    })
  })
  
  #render summary table of clusters
  output$clustTable <- renderTable({
    cragSummaryClust() %>% group_by(cluster) %>%
      summarize("Number of Crags" = n(),
                "Avg. Ascents" = round(mean(ascentCount),0),
                "Avg. Route Mix" = round(mean(routeMix),2),
                "Avg. Rating" = round(mean(avgRating),2),
                "Avg. Grade" = round(mean(medGrade),1))
  })
  
  #render dynamic UI input to filter plot by cluster
  output$clustFilter <- renderUI({
    selectizeInput("clustFilter", "Filter Plot by Cluster",
                 choices = 1:input$nClust,
                 selected = 1:input$nClust,
                 multiple = TRUE)
  })
  
  #filter crag data to be plotted
  cragPlotData <- reactive({
    cragSummaryClust() %>% filter(cragCountry == input$cragCountry, cluster == input$clustFilter)
  })  
  
  #render jitter plot of clusters
  cragPlotFunc <- function(){
    ggplot(cragPlotData()) +
      geom_jitter(aes(x = medGrade, y = avgRating, size = ascentCount, color = cluster))
  }
  output$clustPlot <- renderPlot({
    cragPlotFunc()
    })
  
  #download plotted data to csv
  output$toCSV <- downloadHandler(
    filename = "cragData.csv",
    content = function(file){
      write.csv(cragPlotData(), file, row.names = FALSE)
    }
  )
  
  #download plot
  output$toPNG <- downloadHandler(
    filename = "cragPlot.png",
    content = function(file){
      ggsave(file, plot = cragPlotFunc(), device = "png")
    }
  )

  ########################################################
  ### Peak Grade Tab
  ### predicting peak climbing grade for each user w/ regression tree and random forest

  ##allow user to fit a custom model
  
    #fit regression tree model
    treeFitUser <- reactive({
      if(input$fitUserModel == 0) return()
      input$fitUserModel
      
      isolate({tree(as.formula(paste("maxGrade ~ ", paste(input$independent,collapse = " + "))),
                   data = peakGradeTrain)})
    })
  
    #fit random forest model
    rfFitUser <- reactive({
      if(input$fitUserModel == 0) return()
      input$fitUserModel
      
      isolate({randomForest(as.formula(paste("maxGrade ~ ", paste(input$independent,collapse = " + "))),
                            data = peakGradeTrain,
                            mtry = 2,
                            ntree = 100,
                            importance = TRUE)
        })
    })
  
  
  #render model form
  output$userModelFormMath <- renderUI({
    if(input$fitUserModel == 0) return()
    input$fitUserModel
    
    isolate({
      withMathJax(
        helpText(
          paste("$$maxGrade \\sim ",paste(input$independent,collapse=" + "), "$$")
          )
      )
      })
  })
  
  ##assess fit of active models
  #predict response for each method
  treeTestDefault <- predict(treeFitDefault, peakGradeTest)
  rfTestDefault <- predict(rfFitDefault, peakGradeTest)
  treeTestUser <- reactive({predict(treeFitUser(), peakGradeTest)})
  rfTestUser <- reactive({predict(rfFitUser(), peakGradeTest)})

  #evaluate performance for each method
  treeRMSEDefault <- sqrt(mean((treeTestDefault - peakGradeTest$maxGrade)^2))
  rfRMSEDefault <- sqrt(mean((rfTestDefault - peakGradeTest$maxGrade)^2))
  treeRMSEUser <- reactive({sqrt(mean((treeTestUser() - peakGradeTest$maxGrade)^2))})
  rfRMSEUser <- reactive({sqrt(mean((rfTestUser() - peakGradeTest$maxGrade)^2))})
  
  #render table to compare performance of each model
  output$modelRMSE <- renderTable({
    if(input$fitUserModel == 0) return()
    
    matrix(
      c(treeRMSEDefault, rfRMSEDefault, treeRMSEUser(), rfRMSEUser()),
      nrow = 2,
      dimnames = list(c("Regression Tree", "Random Forest"),
                      c("Default", "User")
                      )
      )
  }, rownames = TRUE, digits = 3)
  
  ##allow user to use model to predict peak grade of a new climber
  
  #improvement: dynamically update allowable height and weight ranges by sex to avoid regions with little data for fitting the model
  
  #update prediction inputs
  predData <- reactive({
    input$makePrediction

    #isolate so that new prediction is made only when button is pressed
    isolate(
      predData <- as.data.frame(cbind(height = input$predictHeight,
                                      weight = input$predictWeight,
                                      sex = input$predictSex,
                                      exp = input$predictExp))
    )
  })
  
  #output predictions
  #possible improvement: can I convert back to USA grade scale?
  treePredictDefault <- reactive({predict(treeFitDefault, predData())})
  rfPredictDefault <- reactive({predict(rfFitDefault, predData())})
  treePredictUser <- reactive({predict(treeFitUser(), predData())})
  rfPredictUser <- reactive({predict(rfFitUser(), predData())})
  
  output$modelPredict <- renderTable({
    if(input$makePrediction == 0) return()
    
    if(input$fitUserModel == 0){
      matrix(
        c(treePredictDefault(), rfPredictDefault()),
        nrow = 2,
        dimnames = list(c("Regression Tree", "Random Forest"),
                        c("Default")
                        )
      )
    } else {
      matrix(
        c(treePredictDefault(), rfPredictDefault(), treePredictUser(), rfPredictUser()),
        nrow = 2,
        dimnames = list(c("Regression Tree", "Random Forest"),
                        c("Default", "User")
                        )
      )
    }
  }, rownames = TRUE, digits = 0)
  ########################################################
})

