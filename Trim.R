## Set up R
setwd("~/R/TeamRoulette")

## Import data
data <- read.csv('data.csv', encoding = 'UTF-8', stringsAsFactors = FALSE)
View(data[, 23:25])
t <- as.integer(readline("Row index to trim by (0 if no rows should be trimed): "))
data <- data[(t+1):nrow(data),]
write.csv(data, 'data.csv')
