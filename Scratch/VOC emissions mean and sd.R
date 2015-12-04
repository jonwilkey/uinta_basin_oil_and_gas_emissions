# Load data from main.R, then source this script

test <- NULL
# Get total median VOC production by year for (a) base and (b) reduced emissions
for(i in seq(from = 12, to = 60, by = 12)-11) {
  
  test <- cbind(test, rowSums(rVOC[, i:(i+11)]))
}
mean(apply(test, 2, mean))/1e3
mean(apply(test, 2, sd))/1e3

test <- NULL
# Get total median VOC production by year for (a) base and (b) reduced emissions
for(i in seq(from = 12, to = 60, by = 12)-11) {
  
  test <- cbind(test, rowSums(VOC[, i:(i+11)]))
}
mean(apply(test, 2, mean))/1e3
mean(apply(test, 2, sd))/1e3