?dbConnect
?dbGetQuery

library(DBI)
library(tidyverse)


#get data from SQLite database
con <- DBI::dbConnect(RSQLite::SQLite(),
                      "database.sqlite")

# methodData <- dbGetQuery(
#   con,
#   "SELECT id, score, shorthand
#   FROM method")
# 
# userData <- dbGetQuery(
#   con,
#   "SELECT id, city, country, sex, height, weight, started, occupation, best_area, worst_area, interests, birth
#   FROM user")
# 
# gradeData <- dbGetQuery(
#   con,
#   "SELECT id, score, usa_routes, usa_routes_input, usa_routes_selector, usa_boulders, usa_boulders_input, usa_boulders_selector
#   FROM grade")
# 
# ascentJoinData <- dbGetQuery(
#   con,
#   "SELECT user_id, grade_id, method_id, climb_type, total_score, year, name, crag_id, crag, sector_id, sector, country, comment, rating, user_recommended, chipped
#   FROM ascent"
#   )

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

#filter to random set of ascents
miniSample <- sample(1:nrow(ascentJoinData), 100)

ascentJoinDataSample <- ascentJoinData[miniSample, ]

#write data to csv
write_csv(ascentJoinData, "climbData.csv")
write_csv(ascentJoinDataSample, "climbDataSample.csv")
