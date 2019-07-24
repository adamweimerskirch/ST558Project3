library(DBI)
library(tidyverse)

#query database
con <- dbConnect(RSQLite::SQLite(),"database.sqlite")

ascentData <- dbGetQuery(
  con,
  "SELECT a.user_id, a.grade_id, a.method_id, a.climb_type, a.total_score, a.year, a.name, a.crag_id, a.crag, a.sector_id, a.sector, a.country, a.comment, a.rating, a.user_recommended, a.chipped,
  m.score as method_score, m.shorthand,
  g.score as grade_score, g.usa_routes, g.usa_routes_input, g.usa_routes_selector, g.usa_boulders, g.usa_boulders_input, g.usa_boulders_selector,
  u.city, u.country, u.sex, u.height, u.weight, u.started, u.occupation, u.best_area, u.worst_area, u.interests, u.birth
  FROM ascent a
  LEFT JOIN method m ON a.method_id = m.id
  LEFT JOIN grade g ON a.grade_id = g.id
  LEFT JOIN user u ON a.user_id = u.id"
)

#create route data frame
routeData <- ascentDataSample %>% select(name, climb_type, crag, sector, country, rating, usa_routes, usa_boulders) %>% transform(difficulty = ifelse(climb_type == 1, usa_boulders, usa_routes))
routeData$climb_type <- routeData$climb_type %>% recode(`0` = "Route", `1` = "Boulder")

#write data to csv
write_csv(ascentData, "ascentData.csv")
write_csv(routeData, "routeData.csv")

#filter to random set of ascents
miniSample <- sample(1:nrow(ascentData), 100)

ascentDataSample <- ascentData[miniSample, ]
write_csv(ascentDataSample, "ascentDataSample.csv")





