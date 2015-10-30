
# Load data
test <- read.csv(file.path(path$raw, "EIA retrospective prices.csv"))
names(test) <- c("type", "label", as.character(1999:2015))

# Reshape function
rshdata <- function(type) {
  
  r <- NULL
  for (i in 1999:2000) {
    
    temp <- test[which(test$type == type & test$label == as.character(i)),3:ncol(test)]
    temp <- temp[temp != 0]
    
    temp <- data.frame(type = type,
                       label = i,
                       year = i:(i+length(temp)-1),
                       cost = temp)
    r <- rbind(r, temp)
  }
  for (i in 2001:2002) {
    
    temp <- test[which(test$type == type & test$label == as.character(i)),3:ncol(test)]
    temp <- temp[temp != 0]
    
    temp <- data.frame(type = type,
                       label = i,
                       year = (i-2):(i+length(temp)-3),
                       cost = temp)
    r <- rbind(r, temp)
  }
  for (i in 2003:2014) {
    
    temp <- test[which(test$type == type & test$label == as.character(i)),3:ncol(test)]
    temp <- temp[temp != 0]
    
    temp <- data.frame(type = type,
                       label = i,
                       year = (i-3):(i+length(temp)-4),
                       cost = temp)
    r <- rbind(r, temp)
  }
  r <- rbind(r,
             data.frame(type  = type,
                        label = 999,
                        year  = 1999:2015,
                        cost  = as.numeric(test[which(test$type == type & test$label == "Actual"),3:ncol(test)])))
}

# Reshape oil data
results <- rshdata("oil")
results <- rbind(results, rshdata("gas"))

# Plot oil
linecolor <- rainbow(length(1999:2014))

pdf(file.path(path$plot, "EIA Forecast Retrospective wellhead.pdf"))
par(mar=c(5.1, 4.1, 4.1, 6.5))

plot(cost~year, results[(results$type == "oil" & results$label == 999),],
     type = "l",
     lwd = 2,
     ylim = c(10, 140),
     xlim = c(1999, 2015),
     xlab = "Year",
     ylab = "Oil Price (2014 $ / bbl)")
abline(v = (1999:2015), h = seq(0, 150, 10), col = "lightgrey")
for (i in 1999:2014) {
  lines(cost~year, results[(results$type == "oil" & results$label == i),], lty = 1, col = linecolor[i-1998])
}
lines(cost~year, results[(results$type == "oil" & results$label == 999),], lwd = 2, col = "black")
legend("right",
       c("Actual", 1999:2014),
       ncol = 1,
       xpd = NA,
       inset = c(-0.25, 0),
       lty = c(1, rep(1, length(linecolor))),
       lwd = c(2, rep(1, length(linecolor))),
       col = c("black", linecolor),
       bg = "white")

plot(cost~year, results[(results$type == "gas" & results$label == 999),],
     type = "l",
     lwd = 2,
     ylim = c(2, 10),
     xlim = c(1999, 2015),
     xlab = "Year",
     ylab = "Natural Gas Price (2014 $ / MCF)")
abline(v = (1999:2015), h = seq(0, 12, 1), col = "lightgrey")
for (i in 1994:2014) {
  lines(cost~year, results[(results$type == "gas" & results$label == i),], lty = 1, col = linecolor[i-1998])
}
lines(cost~year, results[(results$type == "gas" & results$label == 999),], lwd = 2, col = "black")
legend("right",
       c("Actual", 1999:2014),
       ncol = 1,
       xpd = NA,
       inset = c(-0.25, 0),
       lty = c(1, rep(1, length(linecolor))),
       lwd = c(2, rep(1, length(linecolor))),
       col = c("black", linecolor),
       bg = "white")
dev.off()
