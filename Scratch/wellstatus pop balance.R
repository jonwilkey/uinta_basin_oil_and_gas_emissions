# Load histdata
load(file.path(path$data, "histdata.rda"))

# Get list of unique api #s and what counties they are located in
test <- sqldf("select distinct p_api, w_county, h_well_type from p")
names(test) <- c("p_api", "w_county", "hwt")

# Merge with histdata
test <- merge(x = histdata, y = test, by.x = "h_api", by.y = "p_api", all.x = T)

# Drop any wells not in Uintah or Duchesne county (i.e. w_county == NA) and
# select only desired columns
test <- subset(test,
               subset = (!is.na(w_county) &
                         !is.na(h_wellstatus)),
               select = c("h_api", "h_work_type", "h_compl_date", "h_work_compl",
                          "h_well_type", "h_first_prod", "h_wellstatus", "hwt"))

# Get population of wells with completion dates and work type == DRILLING
drill <- subset(test,
                subset = (h_work_type == "DRILL" &
                            h_compl_date >= as.Date("1984-01-01") &
                            h_compl_date < as.Date("2010-01-01") &
                            h_wellstatus == "P" &
                            (hwt == "OW" |
                               hwt == "GW")),
                select = c("h_api", "h_compl_date", "hwt"))

# Get subset of oil wells which were abandoned (i.e. plugged or converted)
aband.ow <- subset(test,
                   subset = ((h_work_type == "CONVERT" |
                                h_work_type == "PLUG") &
                               (h_wellstatus == "A" |
                                  h_wellstatus == "I" |
                                  h_wellstatus == "PA") &
                               hwt == "OW"),
                   select = c("h_api", "h_work_compl"))

# Get subset of gas wells which were abandoned (i.e. plugged or converted)
aband.gw <- subset(test,
                   subset = ((h_work_type == "CONVERT" |
                                h_work_type == "PLUG") &
                               (h_wellstatus == "A" |
                                  h_wellstatus == "I" |
                                  h_wellstatus == "PA") &
                               hwt == "GW"),
                   select = c("h_api", "h_work_compl"))

# Get subset of oil wells which were reworked
re.ow <- subset(test,
                subset = ((h_work_type == "RECOMP" |
                             h_work_type == "REPERF") &
                            h_wellstatus == "P" &
                            hwt == "OW"),
                select = c("h_api", "h_work_compl"))

# Get subset of oil wells which were reworked
re.gw <- subset(test,
                subset = ((h_work_type == "RECOMP" |
                             h_work_type == "REPERF") &
                            h_wellstatus == "P" &
                            hwt == "GW"),
                select = c("h_api", "h_work_compl"))

# Round to nearest month
aband.ow[,2] <- as.Date(as.yearmon(aband.ow[,2]))
aband.gw[,2] <- as.Date(as.yearmon(aband.gw[,2]))
re.ow[,2] <- as.Date(as.yearmon(re.ow[,2]))
re.gw[,2] <- as.Date(as.yearmon(re.gw[,2]))
drill[,2] <- as.Date(as.yearmon(drill[,2]))

# Merge
aband.ow <- na.omit(merge(x = drill, y = aband.ow, all.y = T))
aband.gw <- na.omit(merge(x = drill, y = aband.gw, all.y = T))
re.ow <- na.omit(merge(x = drill, y = re.ow, all.y = T))
re.gw <- na.omit(merge(x = drill, y = re.gw, all.y = T))


# As f(prod) analysis -----------------------------------------------------

# Function to find the percentile of an observation y in a vector x
ptile <- function(x,y) {
  max(which(sort(x, na.last = NA) == y), na.rm = T)/(length(x)+1)
}

# Get unique dates
td <- unique(re.ow$h_work_compl)

# predefine percentile and probability results vectors
pres <- NULL
pbres <- pres

for (i in 1:length(td)) {
  
  # Get data about wells to match
  test <- re.ow[which(re.ow$h_work_compl == td[i]),]
  
  # Get production records
  temp <- subset(p,
                 subset = c(p_rpt_period == as.Date(as.yearmon(td[i])-1/12) &
                              w_well_type == "OW" &
                              p_days_prod >= 28),
                 select = c("p_api", "p_oil_prod"))
  
  if (length(temp$p_api) > 0) {
    
    # Sort in terms of production
    temp <- temp[order(temp$p_oil_prod, na.last = NA),]
    
    # Predefine tempP
    tempP <- NULL
    
    # For each well
    for (j in 1:length(test)) {
      
      # Get percentiles
      tempP <- c(tempP, ptile(temp$p_oil_prod,
                              p$p_oil_prod[which(p$p_api == test$h_api[j] &
                                                   p$p_rpt_period == as.Date(as.yearmon(td[i])-1/12))]))
    }
    
    # Save percentile results
    pres <- c(pres, tempP)
    
    # Calculate rework probability
    pbres <- c(pbres, length(test$h_api)/(nrow(temp)*max(tempP)))
  }
}




# As time series ----------------------------------------------------------
# Calculate time dependence
aband.ow$diff <- with(aband.ow, round(as.numeric(difftime(h_work_compl, h_compl_date, units = "days"))*(12/365.25)))
aband.gw$diff <- with(aband.gw, round(as.numeric(difftime(h_work_compl, h_compl_date, units = "days"))*(12/365.25)))
re.ow$diff <- with(re.ow, round(as.numeric(difftime(h_work_compl, h_compl_date, units = "days"))*(12/365.25)))
re.gw$diff <- with(re.gw, round(as.numeric(difftime(h_work_compl, h_compl_date, units = "days"))*(12/365.25)))


dft <- seq(from = as.Date("1984-01-01"), to = as.Date("2015-05-01"), by = "months")

# Get count as timeseries
result <- data.frame(aband.ow = rep(0, 376),
                     aband.gw = rep(0, 376),
                     re.ow = rep(0, 376),
                     re.gw = rep(0, 376),
                     dft.ow = rep(0, 376),
                     dft.gw = rep(0, 376))
for (i in 0:375) {
  result$aband.ow[i+1] <- length(which(aband.ow$diff == i))
  result$aband.gw[i+1] <- length(which(aband.gw$diff == i))
  result$re.ow[i+1] <- length(which(re.ow$diff == i))
  result$re.gw[i+1] <- length(which(re.gw$diff == i))
  result$dft.ow[i+1] <- length(unique(drill$h_api[which(drill$hwt == "OW" &
                                                          drill$h_compl_date < dft[377-i])]))
  result$dft.gw[i+1] <- length(unique(drill$h_api[which(drill$hwt == "GW" &
                                                          drill$h_compl_date < dft[377-i])]))
}
result <- result[-1,]
result$aband.ow <- cumsum(result$aband.ow)
result$aband.gw <- cumsum(result$aband.gw)
result$re.ow <- cumsum(result$re.ow)
result$re.gw <- cumsum(result$re.gw)
result$pr.ow <- length(unique(drill$h_api[drill$hwt == "OW"]))-(result$aband.ow)#+result$re.ow)
result$pr.gw <- length(unique(drill$h_api[drill$hwt == "GW"]))-(result$aband.gw)#+result$re.gw)
result$time <- 1:375

pdf(file.path(path$plot, "Well population balance as timeseries.pdf"))

# Plot results

hist(aband.ow$diff,
     breaks = 20,
     xlim = c(0, 400),
     xlab = "Time Since First Production (months)",
     main = "Time First Prod. to Abandonment - Oil Wells")

hist(aband.gw$diff,
     breaks = 20,
     xlim = c(0, 400),
     xlab = "Time Since First Production (months)",
     main = "Time First Prod. to Abandonment - Gas Wells")

hist(re.ow$diff,
     breaks = 20,
     xlim = c(0, 400),
     xlab = "Time Since First Production (months)",
     main = "Time First Prod. to Rework - Oil Wells")

hist(re.gw$diff,
     breaks = 20,
     xlim = c(0, 400),
     xlab = "Time Since First Production (months)",
     main = "Time First Prod. to Rework - Gas Wells")

plot(pr.ow ~ time, result,
     type = "l",
     ylim = c(0, 3e3),
     xlab = "Time Since First Production (months)",
     ylab = "Well Count",
     main = "Oil Well Pop. Bal. for Wells Drilled 1984-2010")
lines((pr.ow-re.ow)~time, result, col = "black", lty = 2)
lines(re.ow~time, result, col = "red")
lines(aband.ow~time, result, col = "blue")
lines(dft.ow~time, result, col = "green")
legend("topright",
       c("Prior Wells - Abandoned",
         "Prior Wells - (Abandoned + Rework)",
         "Rework",
         "Abandoned",
         "# of Wells in Sample"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))

plot(pr.gw ~ time, result,
     type = "l",
     ylim = c(0, 4.5e3),
     xlab = "Time Since First Production (months)",
     ylab = "Well Count",
     main = "Gas Well Pop. Bal. for Wells Drilled 1984-2010")
lines((pr.gw-re.gw)~time, result, col = "black", lty = 2)
lines(re.gw~time, result, col = "red")
lines(aband.gw~time, result, col = "blue")
lines(dft.gw~time, result, col = "green")
legend("right",
       c("Prior Wells - Abandoned",
         "Prior Wells - (Abandoned + Rework)",
         "Rework",
         "Abandoned",
         "# of Wells in Sample"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))

dev.off()

# As function of production -----------------------------------------------

# Function
prodfun <- function(x) {
  
  # Add columns for prod and cumprod
  x$prod <- rep(NA, times = nrow(x))
  x$cprod <- x$prod
  
  for (i in 1:nrow(x)) {
    x$prod[i] <- tail(p$p_oil_prod[which(p$p_api == x$h_api[i] &
                                    p$p_rpt_period <= x$h_work_compl[i])])[6]
    x$cprod[i] <- sum(p$p_oil_prod[which(p$p_api == x$h_api[i] & p$p_rpt_period < x$h_work_compl[i])], na.rm = T)
  }
  return(x)
}

# Get production levels (warning: takes awhile)
aband.ow <- prodfun(aband.ow)
aband.gw <- prodfun(aband.gw)
re.ow <- prodfun(re.ow)
re.gw <- prodfun(re.gw)

# plots

pdf(file.path(path$plot, "Well events as function of production.pdf"))

hist(aband.ow$prod,
     breaks = 20,
     xlab = "Oil Production (bbl)",
     main = "Oil Production at Abandonment")
cutoff <- quantile(aband.ow$prod, 0.9, na.rm = T)
abline(v = cutoff, lwd = 2, col = "blue")
legend("topright", paste("90% =", cutoff), lwd = 2, col = "blue")

hist(aband.ow$cprod,
     breaks = 20,
     xlab = "Cumulative Oil Production (bbl)",
     main = "Cumulative Oil Production at Abandonment")
cutoff <- quantile(aband.ow$cprod, 0.9, na.rm = T)
abline(v = cutoff, lwd = 2, col = "blue")
legend("topright", paste("90% =", cutoff), lwd = 2, col = "blue")

hist(aband.gw$prod,
     breaks = 20,
     xlab = "Gas Production (MCF)",
     main = "Gas Production at Abandonment")
cutoff <- quantile(aband.gw$prod, 0.9, na.rm = T)
abline(v = quantile(aband.gw$prod, 0.9, na.rm = T), lwd = 2, col = "blue")
legend("topright", paste("90% =", cutoff), lwd = 2, col = "blue")

hist(aband.gw$cprod,
     breaks = 20,
     xlab = "Cumulative Gas Production (MCF)",
     main = "Cumulative Gas Production at Abandonment")
cutoff <- quantile(aband.gw$cprod, 0.9, na.rm = T)
abline(v = cutoff, lwd = 2, col = "blue")
legend("topright", paste("90% =", cutoff), lwd = 2, col = "blue")

hist(re.ow$prod,
     breaks = 20,
     xlab = "Oil Production (bbl)",
     main = "Oil Production at Rework")
cutoff <- quantile(re.ow$prod, 0.9, na.rm = T)
abline(v = cutoff, lwd = 2, col = "blue")
legend("topright", paste("90% =", cutoff), lwd = 2, col = "blue")

hist(re.ow$cprod,
     breaks = 20,
     xlab = "Cumulative Oil Production (bbl)",
     main = "Cumulative Oil Production at Rework")
cutoff <- quantile(re.ow$cprod, 0.9, na.rm = T)
abline(v = cutoff, lwd = 2, col = "blue")
legend("topright", paste("90% =", cutoff), lwd = 2, col = "blue")

hist(re.gw$prod,
     breaks = 20,
     xlab = "Gas Production (MCF)",
     main = "Gas Production at Rework")
cutoff <- quantile(re.gw$prod, 0.9, na.rm = T)
abline(v = quantile(re.gw$prod, 0.9, na.rm = T), lwd = 2, col = "blue")
legend("topright", paste("90% =", cutoff), lwd = 2, col = "blue")

hist(re.gw$cprod,
     breaks = 20,
     xlab = "Cumulative Gas Production (MCF)",
     main = "Cumulative Gas Production at Rework")
cutoff <- quantile(re.gw$cprod, 0.9, na.rm = T)
abline(v = cutoff, lwd = 2, col = "blue")
legend("topright", paste("90% =", cutoff), lwd = 2, col = "blue")

test1 <- (p$p_oil_prod[which(p$p_rpt_period == as.Date("2014-10-01") & p$w_well_type == "OW")])
test1 <- test1[test1>=0]
hist(test1,
     breaks = 1e3,
     xlim = c(0, 5e3),
     xlab = "Oil Production (bbl)",
     main = "Oil Production from OW in Oct. 2014")
abline(v = quantile(aband.ow$prod, 0.9, na.rm = T), lwd = 2, col = "blue")
abline(v = quantile(re.ow$prod, 0.9, na.rm = T), lwd = 2, col = "red")
mtext("Note: x-axis clipped to 5e3 bbl, max rate ~50e3 bbl")
legend("right", c("90% Abandoned", "90% Rework"), lwd = 2, col = c("blue", "red"))

test2 <- (p$p_gas_prod[which(p$p_rpt_period == as.Date("2014-10-01") & p$w_well_type == "GW")])
test2 <- test2[test2>=0]
hist(test2,
     breaks = 1e3,
     xlim = c(0, 10e3),
     xlab = "Gas Production (bbl)",
     main = "Gas Production from GW in Oct. 2014")
abline(v = quantile(aband.gw$prod, 0.9, na.rm = T), lwd = 2, col = "blue")
abline(v = quantile(re.gw$prod, 0.9, na.rm = T), lwd = 2, col = "red")
mtext("Note: x-axis clipped to 10e3 MCF, max rate ~150e3 MCF")
legend("right", c("90% Abandoned", "90% Rework"), lwd = 2, col = c("blue", "red"))

dev.off()
