library(DBI)
library(tidyverse)
library(tools)

#query database
con <- dbConnect(RSQLite::SQLite(),"database.sqlite")

ascentData <- dbGetQuery(
  con,
  "SELECT a.user_id, a.grade_id, a.method_id, a.climb_type, a.total_score, a.year, a.name, a.crag_id, a.crag, a.sector_id, a.sector, a.country, a.comment, a.rating, a.user_recommended, a.chipped,
  m.score as method_score, m.shorthand,
  g.score as grade_score, g.usa_routes, g.usa_boulders,
  u.city as user_city, u.country as user_country, u.sex, u.height, u.weight, u.started, u.occupation, u.best_area, u.worst_area, u.interests, u.birth
  FROM ascent a
  LEFT JOIN method m ON a.method_id = m.id
  LEFT JOIN grade g ON a.grade_id = g.id
  LEFT JOIN user u ON a.user_id = u.id"
)

#add contextual difficulty variable
ascentData <- ascentData %>% transform(grade_usa = ifelse(climb_type == 1, usa_boulders, usa_routes)) %>% within(rm(usa_routes, usa_boulders))

#convert appropriate variables to title case
ascentData$name <- stri_trans_totitle(ascentData$name)
ascentData$crag <- stri_trans_totitle(ascentData$crag)
ascentData$sector <- stri_trans_totitle(ascentData$sector)
ascentData$country <- stri_trans_toupper(ascentData$country)
ascentData$user_city <- stri_trans_totitle(ascentData$user_city)
ascentData$user_country <- stri_trans_toupper(ascentData$user_country)

#filter to random set of ascents
miniSample <- sample(1:nrow(ascentData), 1000)
ascentDataSample <- ascentData[miniSample, ]

#write data to csv
write_csv(ascentData, "ascentData.csv")
write_csv(ascentDataSample, "ascentDataSample.csv")
