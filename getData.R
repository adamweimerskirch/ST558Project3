?dbConnect
?dbGetQuery

library(DBI)

dbConnect()

my.data.frame <- dbGetQuery(My_conn, "SELECT * FROM My_Table")
write.csv(my.data.frame, file = "MyFileName.csv", ...)