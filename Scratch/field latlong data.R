load(file.path(path$data, "welldata.rda"))

test <- subset(welldata, subset = ((w_county == "DUCHENSE" | w_county == "UINTAH") & w_well_type == "OW"),
               select = c("w_field_num", "w_lat_surf", "w_long_surf"))
test <- na.omit(test)
test[,2] <- as.numeric(test[,2])
test[,3] <- as.numeric(test[,3])

field <- sort(unique(test$w_field_num))
result <- matrix(0, nrow = length(field), ncol = 3)

for (i in 1:length(field)) {
  temp <- subset(test, w_field_num == field[i])
  result[i,2] <- mean(temp$w_lat_surf, na.rm = TRUE)
  result[i,3] <- mean(temp$w_long_surf, na.rm = TRUE)
  result[i,1] <- nrow(temp)
}

result <- data.frame(field, result)

clipboard(result, col.names = FALSE)