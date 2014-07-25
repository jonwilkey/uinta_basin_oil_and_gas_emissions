#-------------------------------------------------------------------------------
# declineCurve_DAQ_v1.R (DOGM Decline Curve Fitting for DAQ)
# Version 1
# 01/10/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Change this to the folder where you keep your *.rda files
# For Mac:
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data"
# For Windows:
data_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(data_root)

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------

# Copy/Paste function
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------

# 'minpack.lm' for nonlinear least-squares regression using Levenberg-Marquardt
# algorithm
library(minpack.lm)
# 'sqldf' for SQL queries
library(sqldf)

#-------------------------------------------------------------------------------
# Load DOGM data tables and DAQ csv list of API#'s to keep
#-------------------------------------------------------------------------------

# Load the dataframe named 'DAQ.rda' which is merged proddata and histdata databases from DOGM
load(file.path(data_root, "DAQ.rda"))
# Rename dataframe for brevity
p <- DAQ
# Find production time ptime for each row of dataframe by subtracting the
# reporting period of that row p_rpt_period by the date of first production
# h_first_prod. Covert difftime to numeric value of weeks, divide by 4 to get
# months, round, and attach to p
p$ptime <- round(as.numeric(p$p_rpt_period - p$h_first_prod, units = "weeks")/4)
# Merged dataframe contains many NA values in ptime, drop all rows that have
# ptime = NA by creating vector of row indices that are not NA and then redefine
# p as only containing those rows. Also drop any records for which ptime < 0
keeps <- which(p$ptime != "NA" & p$ptime >=0)
p <- p[keeps,]
# Drop first month of production since it can be partial month
p <- subset(p, subset = (ptime != 0))

# Import DAQ csv files. Note that Both csv files are list of oil wells on state
# jurisdiction in Uinta and Duchesne counties.
# Load 03-12_wells.csv, which is record of wells in aforementioned subset that
# produced between 2003-2012
DAQ.0312.wells <- read.csv("03-12_wells.csv", header = TRUE)
# Load 2012_wells.csv, which is record of wells in aforementioned subset that
# produced in 2012
DAQ.2012.wells <- read.csv("2012_wells.csv", header = TRUE)

# The next two for-loops select the API# in the j-th element of the DAQ API
# listing and selects all rows of p that have an API# matching the current API#
# value. These subsetted rows are appended to p.DAQ.xxxx, creating two new
# dataframes containing all records related to the imported csv's. Note that
# these two loops take a LONG time ( > 30 minutes)
p.DAQ.0312 <- NULL
for (j in 1:length(DAQ.0312.wells[,1])) {
  temp <- subset(p, subset = (p_api == DAQ.0312.wells[j,1]))
  p.DAQ.0312 <- rbind(p.DAQ.0312, temp)
}
p.DAQ.2012 <- NULL
for (j in 1:length(DAQ.2012.wells[,1])) {
  temp <- subset(p, subset = (p_api == DAQ.2012.wells[j,1]))
  p.DAQ.2012 <- rbind(p.DAQ.2012, temp)
}

# Finally, create one dataframe which is combination of both 0312 and 2012
p.DAQ.all <- rbind(p.DAQ.0312, p.DAQ.2012)

#-------------------------------------------------------------------------------
# Regression to All Wells in p.DAQ.0312, p.DAQ.2012, and p.DAQ.all
#-------------------------------------------------------------------------------

# Fit the data for each fluid for each type of well
# Oil from p.DAQ.0312 wells
o.0312.exp <- nlsLM(p_oil_prod ~ alpha * exp(-ptime / theta),
                 data = p.DAQ.0312,
                 start = list(alpha = 1053, theta = 32),
                 control = list(maxiter=1000))
o.0312.hyp <- nlsLM(p_oil_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                 data = p.DAQ.0312,
                 start = list(alpha = 9000, theta = 3.0, delta = 4.5),
                 control = list(maxiter=1000))
o.0312.har <- nlsLM(p_oil_prod ~ alpha / (1 + theta * ptime),
                 data = p.DAQ.0312,
                 start = list(alpha = 1402, theta = 0.09),
                 control = list(maxiter=1000))
# Gas from p.DAQ.0312 wells
g.0312.exp <- nlsLM(p_gas_prod ~ alpha * exp(-ptime / theta),
                data = p.DAQ.0312,
                start = list(alpha = 2165, theta = 70),
                control = list(maxiter=1000))
g.0312.hyp <- nlsLM(p_gas_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                data = p.DAQ.0312,
                start = list(alpha = 2461, theta = 1.0, delta = 0.03),
                control = list(maxiter=1000))
g.0312.har <- nlsLM(p_gas_prod ~ alpha / (1 + theta * ptime),
                data = p.DAQ.0312,
                start = list(alpha = 2459, theta = 0.03),
                control = list(maxiter=1000))

# Oil from p.DAQ.2012 wells
o.2012.exp <- nlsLM(p_oil_prod ~ alpha * exp(-ptime / theta),
                    data = p.DAQ.2012,
                    start = list(alpha = 1053, theta = 32),
                    control = list(maxiter=1000))
o.2012.hyp <- nlsLM(p_oil_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                    data = p.DAQ.2012,
                    start = list(alpha = 9000, theta = 3.0, delta = 4.5),
                    control = list(maxiter=1000))
o.2012.har <- nlsLM(p_oil_prod ~ alpha / (1 + theta * ptime),
                    data = p.DAQ.2012,
                    start = list(alpha = 1402, theta = 0.09),
                    control = list(maxiter=1000))
# Gas from p.DAQ.2012 wells
g.2012.exp <- nlsLM(p_gas_prod ~ alpha * exp(-ptime / theta),
                    data = p.DAQ.2012,
                    start = list(alpha = 2165, theta = 70),
                    control = list(maxiter=1000))
g.2012.hyp <- nlsLM(p_gas_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                    data = p.DAQ.2012,
                    start = list(alpha = 2461, theta = 1.0, delta = 0.03),
                    control = list(maxiter=1000))
g.2012.har <- nlsLM(p_gas_prod ~ alpha / (1 + theta * ptime),
                    data = p.DAQ.2012,
                    start = list(alpha = 2459, theta = 0.03),
                    control = list(maxiter=1000))

# Oil from p.DAQ.all wells
o.all.exp <- nlsLM(p_oil_prod ~ alpha * exp(-ptime / theta),
                    data = p.DAQ.all,
                    start = list(alpha = 1053, theta = 32),
                    control = list(maxiter=1000))
o.all.hyp <- nlsLM(p_oil_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                    data = p.DAQ.all,
                    start = list(alpha = 9000, theta = 3.0, delta = 4.5),
                    control = list(maxiter=1000))
o.all.har <- nlsLM(p_oil_prod ~ alpha / (1 + theta * ptime),
                    data = p.DAQ.all,
                    start = list(alpha = 1402, theta = 0.09),
                    control = list(maxiter=1000))
# Gas from p.DAQ.all wells
g.all.exp <- nlsLM(p_gas_prod ~ alpha * exp(-ptime / theta),
                    data = p.DAQ.all,
                    start = list(alpha = 2165, theta = 70),
                    control = list(maxiter=1000))
g.all.hyp <- nlsLM(p_gas_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                    data = p.DAQ.all,
                    start = list(alpha = 2461, theta = 1.0, delta = 0.03),
                    control = list(maxiter=1000))
g.all.har <- nlsLM(p_gas_prod ~ alpha / (1 + theta * ptime),
                    data = p.DAQ.all,
                    start = list(alpha = 2459, theta = 0.03),
                    control = list(maxiter=1000))

# Determine median and mean oil and gas production from p.DAQ.0312 wells
median.oil.0312 <- NULL
median.gas.0312 <- NULL
mean.oil.0312 <- NULL
mean.gas.0312 <- NULL
for (j in 1:max(p.DAQ.0312$ptime)){
  temp <- subset(p.DAQ.0312, subset = (ptime == j))
  median.oil.0312 <- rbind(median.oil.0312, median(temp$p_oil_prod))
  median.gas.0312 <- rbind(median.gas.0312, median(temp$p_gas_prod))
  mean.oil.0312 <- rbind(mean.oil.0312, mean(temp$p_oil_prod))
  mean.gas.0312 <- rbind(mean.gas.0312, mean(temp$p_gas_prod))
}
# Determine median and mean oil and gas production from p.DAQ.0312 wells
median.oil.2012 <- NULL
median.gas.2012 <- NULL
mean.oil.2012 <- NULL
mean.gas.2012 <- NULL
for (j in 1:max(p.DAQ.2012$ptime)){
  temp <- subset(p.DAQ.2012, subset = (ptime == j))
  median.oil.2012 <- rbind(median.oil.2012, median(temp$p_oil_prod))
  median.gas.2012 <- rbind(median.gas.2012, median(temp$p_gas_prod))
  mean.oil.2012 <- rbind(mean.oil.2012, mean(temp$p_oil_prod))
  mean.gas.2012 <- rbind(mean.gas.2012, mean(temp$p_gas_prod))
}
# Determine median and mean oil and gas production from p.DAQ.all wells
median.oil.all <- NULL
median.gas.all <- NULL
mean.oil.all <- NULL
mean.gas.all <- NULL
for (j in 1:max(p.DAQ.all$ptime)){
  temp <- subset(p.DAQ.all, subset = (ptime == j))
  median.oil.all <- rbind(median.oil.all, median(temp$p_oil_prod))
  median.gas.all <- rbind(median.gas.all, median(temp$p_gas_prod))
  mean.oil.all <- rbind(mean.oil.all, mean(temp$p_oil_prod))
  mean.gas.all <- rbind(mean.gas.all, mean(temp$p_gas_prod))
}

# Print results to PDF
pdf(file = "DAQ Fit Results.pdf")
# Oil from p.DAQ.0312 plot
x <- seq(from = 1, to = max(p.DAQ.0312$ptime))
plot(x, mean.oil.0312,
     type="l",
     ylab="Oil Production (bbl/month)",
     xlab="Time Since First Production (months)")
title("Oil Production from all Oil Wells in 03-12_wells.csv")
lines(x, median.oil.0312, col="black", lty=2)
y.exp <- coef(o.0312.exp)[1] * exp(-x / coef(o.0312.exp)[2])
y.hyp <- coef(o.0312.hyp)[1] * (1 + coef(o.0312.hyp)[3] * coef(o.0312.hyp)[2] * x)^(-1/coef(o.0312.hyp)[2])
y.har <- coef(o.0312.har)[1] / (1 + coef(o.0312.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Mean", "Median", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
# Gas from p.DAQ.0312 plot
plot(x, mean.gas.0312,
     type="l",
     ylab="Gas Production (MCF/month)",
     xlab="Time Since First Production (months)")
title("Gas Production from all Oil Wells in 03-12_wells.csv")
lines(x, median.gas.0312, col="black", lty=2)
y.exp <- coef(g.0312.exp)[1] * exp(-x / coef(g.0312.exp)[2])
y.hyp <- coef(g.0312.hyp)[1] * (1 + coef(g.0312.hyp)[3] * coef(g.0312.hyp)[2] * x)^(-1/coef(g.0312.hyp)[2])
y.har <- coef(g.0312.har)[1] / (1 + coef(g.0312.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
# Oil from p.DAQ.2012 plot
x <- seq(from = 1, to = max(p.DAQ.2012$ptime))
plot(x, mean.oil.2012,
     type="l",
     ylab="Oil Production (bbl/month)",
     xlab="Time Since First Production (months)")
title("Oil Production from all Oil Wells in 2012_wells.csv")
lines(x, median.oil.2012, col="black", lty=2)
y.exp <- coef(o.2012.exp)[1] * exp(-x / coef(o.2012.exp)[2])
y.hyp <- coef(o.2012.hyp)[1] * (1 + coef(o.2012.hyp)[3] * coef(o.2012.hyp)[2] * x)^(-1/coef(o.2012.hyp)[2])
y.har <- coef(o.2012.har)[1] / (1 + coef(o.2012.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
# Gas from p.DAQ.2012 plot
plot(x, mean.gas.2012,
     type="l",
     ylab="Gas Production (MCF/month)",
     xlab="Time Since First Production (months)")
title("Gas Production from all Oil Wells in 2012_wells.csv")
lines(x, median.gas.2012, col="black", lty=2)
y.exp <- coef(g.2012.exp)[1] * exp(-x / coef(g.2012.exp)[2])
y.hyp <- coef(g.2012.hyp)[1] * (1 + coef(g.2012.hyp)[3] * coef(g.2012.hyp)[2] * x)^(-1/coef(g.2012.hyp)[2])
y.har <- coef(g.2012.har)[1] / (1 + coef(g.2012.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
# Oil from p.DAQ.all plot
x <- seq(from = 1, to = max(p.DAQ.all$ptime))
plot(x, mean.oil.all,
     type="l",
     ylab="Oil Production (bbl/month)",
     xlab="Time Since First Production (months)")
title("Oil Production from all Oil Wells in both 03-12_wells.csv and 2012_wells.csv")
lines(x, median.oil.all, col="black", lty=2)
y.exp <- coef(o.all.exp)[1] * exp(-x / coef(o.all.exp)[2])
y.hyp <- coef(o.all.hyp)[1] * (1 + coef(o.all.hyp)[3] * coef(o.all.hyp)[2] * x)^(-1/coef(o.all.hyp)[2])
y.har <- coef(o.all.har)[1] / (1 + coef(o.all.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
# Gas from p.DAQ.all plot
plot(x, mean.gas.all,
     type="l",
     ylab="Gas Production (MCF/month)",
     xlab="Time Since First Production (months)")
title("Gas Production from all Oil Wells in both 03-12_wells.csv and 2012_wells.csv")
lines(x, median.gas.all, col="black", lty=2)
y.exp <- coef(g.all.exp)[1] * exp(-x / coef(g.all.exp)[2])
y.hyp <- coef(g.all.hyp)[1] * (1 + coef(g.all.hyp)[3] * coef(g.all.hyp)[2] * x)^(-1/coef(g.all.hyp)[2])
y.har <- coef(g.all.har)[1] / (1 + coef(g.all.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
dev.off()

#-------------------------------------------------------------------------------
# Regression to Individual Wells for p.DAQ.0312
#-------------------------------------------------------------------------------

# Define dataframes that will hold fit coefficients
coef.exp.o <- NULL
coef.hyp.o <- NULL
# Get list of unique API#'s in dataframe
api.list <- unique(p.DAQ.0312[,"p_api",drop=TRUE])
# Enter indices here of wells that fail to converge to skip them in subsequent
# iterations of for-loop
api.list <- api.list[c(-21, -75, -261)]
# Also make a copy since p.DAQ.2012 uses same variable name
api.list.o <- api.list
# Save plot output to PDF
pdf(file = "DAQ Individual Fits for Wells in 0312_wells_csv.pdf")
# Begin for-loop
for (a in 1:length(api.list)) {
  # Creates a subset 'w' from dataframe 'p.DAQ.0312' whose API# is = to the API#
  # in step 'a' of the for-loop
  w <- subset(p.DAQ.0312, subset = (p_api == api.list[a]))
  # Smooth the dataset and redefine smooth spline as dataframe
  sm <- smooth.spline(w$ptime, w$p_oil_prod)
  sm <- data.frame(sm$x, sm$y)
  # Perform regression for oil from well
  fit.exp.o <- nlsLM(sm.y ~ alpha * exp(-sm.x / theta),
                     data = sm,
                     start = list(alpha = sm$sm.y[1], theta = coef(o.0312.exp)[2]),
                     control = list(maxiter=1000))
  fit.hyp.o <- nlsLM(sm.y ~ alpha*(1+theta*delta*sm.x)^(-1/theta),
                     data = sm,
                     start = list(alpha = sm$sm.y[1], theta = coef(o.0312.hyp)[2], delta = coef(o.0312.hyp)[3]),
                     control = list(maxiter=1000))
  coef.exp.o <- rbind(coef.exp.o, coef(fit.exp.o))
  coef.hyp.o <- rbind(coef.hyp.o, coef(fit.hyp.o))
  plot(w$ptime, w$p_oil_prod,
       type="p",
       pch = 20,
       ylab="Oil Production (bbl/month)",
       xlab="Time Since First Production (months)")
  title(paste("Oil from API# ", api.list[a], sep = ""))
  lines(sm$sm.x, sm$sm.y, col="black", lty=2)
  lines(sm$sm.x, y=predict(fit.exp.o), col="red")
  lines(sm$sm.x, y=predict(fit.hyp.o), col="blue")
  legend("topright",
         c("Actual", "Smooth", "Exponential", "Hyperbolic"),
         lty = c(0,2,1,1),
         pch = c(20,NA,NA,NA),
         col = c("black", "black", "red", "blue")) 
}
dev.off()

#-------------------------------------------------------------------------------
# Regression to Individual Wells for p.DAQ.2012
#-------------------------------------------------------------------------------

# Define dataframes that will hold fit coefficients
coef.exp <- NULL
coef.hyp <- NULL
# Get list of unique API#'s in dataframe
api.list <- unique(p.DAQ.2012[,"p_api",drop=TRUE])
# Enter indices here of wells that fail to converge to skip them in subsequent
# iterations of for-loop
api.list <- api.list[c(-5, -14, -36, -42, -55, -62, -151, -163, -169, -306,
                       -309, -400, -557, -735, -897, -992, -1124, -1157, -1171,
                       -1281, -1409, -1417, -1544, -1644, -1662)]
# Save plot output to PDF
pdf(file = "DAQ Individual Fits for Wells in 2012_wells_csv.pdf")
# Begin for-loop
for (a in 1:length(api.list)) {
  # Creates a subset 'w' from dataframe 'p.DAQ.2012' whose API# is = to the API#
  # in step 'a' of the for-loop
  w <- subset(p.DAQ.2012, subset = (p_api == api.list[a]))
  # Smooth the dataset and redefine smooth spline as dataframe
  sm <- smooth.spline(w$ptime, w$p_oil_prod)
  sm <- data.frame(sm$x, sm$y)
  # Perform regression for oil from well
  fit.exp.o <- nlsLM(sm.y ~ alpha * exp(-sm.x / theta),
                     data = sm,
                     start = list(alpha = sm$sm.y[1], theta = coef(o.2012.exp)[2]),
                     control = list(maxiter=1000))
  fit.hyp.o <- nlsLM(sm.y ~ alpha*(1+theta*delta*sm.x)^(-1/theta),
                     data = sm,
                     start = list(alpha = sm$sm.y[1], theta = coef(o.2012.hyp)[2], delta = coef(o.2012.hyp)[3]),
                     control = list(maxiter=1000))
  coef.exp <- rbind(coef.exp, coef(fit.exp.o))
  coef.hyp <- rbind(coef.hyp, coef(fit.hyp.o))
  plot(w$ptime, w$p_oil_prod,
       type="p",
       pch = 20,
       ylab="Oil Production (bbl/month)",
       xlab="Time Since First Production (months)")
  title(paste("Oil from API# ", api.list[a], sep = ""))
  lines(sm$sm.x, sm$sm.y, col="black", lty=2)
  lines(sm$sm.x, y=predict(fit.exp.o), col="red")
  lines(sm$sm.x, y=predict(fit.hyp.o), col="blue")
  legend("topright",
         c("Actual", "Smooth", "Exponential", "Hyperbolic"),
         lty = c(0,2,1,1),
         pch = c(20,NA,NA,NA),
         col = c("black", "black", "red", "blue")) 
}
dev.off()

#-------------------------------------------------------------------------------
# Pasting to Clipboard to Copy to Excel Table
#-------------------------------------------------------------------------------
# Uncomment and run one line at a time and to copy & paste into Excel

write.excel(coef(o.0312.exp), col.names = FALSE)
write.excel(coef(o.0312.hyp), col.names = FALSE)
write.excel(coef(o.0312.har), col.names = FALSE)
write.excel(coef(o.2012.exp), col.names = FALSE)
write.excel(coef(o.2012.hyp), col.names = FALSE)
write.excel(coef(o.2012.har), col.names = FALSE)
write.excel(api.list, col.names= FALSE)
write.excel(api.list.o, col.names= FALSE)
write.excel(coef.exp.o, col.names = FALSE)
write.excel(coef.hyp.o, col.names = FALSE)
# Coefficients for 2012.csv are too large to fit on clipboard, have to save to
# csv first, then import manually into Excel
write.csv(coef.exp, file = "coef.exp.csv", row.names = FALSE)
write.csv(coef.hyp, file = "coef.hyp.csv", row.names = FALSE)