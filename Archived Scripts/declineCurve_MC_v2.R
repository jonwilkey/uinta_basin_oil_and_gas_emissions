#-------------------------------------------------------------------------------
# declineCurve_MC_v2.R (Monte-Carlo Decline Curve Comparisons)
# Version 2
# 04/21/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Version History
#-------------------------------------------------------------------------------
# --- Version 1 ---
# 1. Wrote script that determines 5th, 50th, 95th quantile for actual and
#    Monte-Carlo decline curve production of oil from oil wells.
# --- Version 2 ---
# 1. Dropped code for OGW
# 2. Added code for simulating and plotting GGW

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(work_root)

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
# 'sqldf' for SQL queries
library(sqldf)

#-------------------------------------------------------------------------------
# Load DOGM data tables
#-------------------------------------------------------------------------------
# Load *.rda files
load(file.path(data_root, "pdf_oow.rda"))
load(file.path(data_root, "cdf_oow.rda"))
load(file.path(data_root, "decline_field_ow.rda"))
load(file.path(data_root, "pdf_ggw.rda"))
load(file.path(data_root, "cdf_ggw.rda"))
# load(file.path(data_root, "pdf_gow.rda"))
# load(file.path(data_root, "cdf_gow.rda"))
# load(file.path(data_root, "pdf_ogw.rda"))
# load(file.path(data_root, "cdf_ogw.rda"))
load(file.path(data_root, "production.rda"))
# Rename dataframe for brevity
p <- production
remove(production)

#-------------------------------------------------------------------------------
# Sort actual data
#-------------------------------------------------------------------------------
# Drop first month of production since it can be partial month
p <- subset(p, time != 0)

# Only keep desired rows in p
p <- p[, c("p_api",
           "w_well_type",
           "p_oil_prod",
           "p_gas_prod",
           "w_field_num",
           "time")]

# Create subset dataframe which contains only oil wells
p.ow <- subset(p, w_well_type == "OW")
# Same for gas wells
p.gw <- subset(p, w_well_type == "GW")

# Oil & Gas fields to pull quantile stats from
field.ow <- c(105, 72, 590, 117, 718, 55, 60, 65, 665, 710, 80, 630, 605, 560,
              101, 610, 692, 695)
field.gw <- c(630, 710, 600, 617, 640, 547, 670, 635, 618, 622, 610, 590, 791,
              72, 105, 55)

# Define list to hold quantile stats for each field and list of max field times
quantile.oow <- vector("list", length(field.ow))
quantile.ggw <- vector("list", length(field.gw))
field.ow.maxtime <- matrix (0, nrow = length(field.ow), ncol = 1)
field.gw.maxtime <- matrix (0, nrow = length(field.gw), ncol = 1)

# Pull 5th, 50th, and 95th quantiles of production from wells in each field
# For OOW
for (j in 1:length(field.gw)) {
  # Note: length of temp.oow (nrow) will lead to issues with x-axis on plot of
  # field 101 (which has a max time of 97 compared to max time in whole set of
  # 248)
  temp.oow <- matrix(0, nrow = max(p.ow$time), ncol = 3)  
  temp <- subset(p.ow, w_field_num == field.ow[j])
  for (i in 1:max(temp$time)){
    temp.step <- subset(temp, time == i)
    temp.ow <- quantile(temp.step[,c("p_oil_prod")], c(0.05, 0.50, 0.95))    
    temp.oow[i,1] <- temp.ow[1]
    temp.oow[i,2] <- temp.ow[2]
    temp.oow[i,3] <- temp.ow[3]
  }
  quantile.oow[[j]] <- temp.oow
  # Find max time for each field
  field.ow.maxtime[j] <- max(temp$time)
}
# For GGW
for (j in 1:length(field.gw)) {
  temp.ggw <- matrix(0, nrow = max(p.gw$time), ncol = 3)
  temp <- subset(p.gw, w_field_num == field.gw[j])
  for (i in 1:max(temp$time)){
    temp.step <- subset(temp, time == i)
    temp.gw <- quantile(temp.step[,c("p_gas_prod")], c(0.05, 0.50, 0.95))
    temp.ggw[i,1] <- temp.gw[1]
    temp.ggw[i,2] <- temp.gw[2]
    temp.ggw[i,3] <- temp.gw[3]
  }
  quantile.ggw[[j]] <- temp.ggw
  # Find max time for each field
  field.gw.maxtime[j] <- max(temp$time)
}

#-------------------------------------------------------------------------------
# Fit results for each field
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Monte-Carlo Simulation for OOW for each field
#-------------------------------------------------------------------------------
# Number of simulation runs for each field
nrun <- 10^5
# Define list of Monte-Carlo simulation results for each field
mc.oow <- vector("list", length(field.ow))

for (i in 1:length(field.ow)) {
  # Define timestep sequence
  t <- seq(from = 1, to = field.ow.maxtime[i])
  # Define temporary matrix for holding simulation run results for field i
  temp <- matrix(0, nrow = length(t), ncol = nrun)
  for (h in 1:nrun) {
    # Generate random numbers for each coefficient for this well
    rn <- runif(3, min = 0, max = 1)
    # Use findInterval to pick index and assign value to each variable
    a <- pdf.oow[findInterval(rn[1], c(0, cdf.oow[,(i-1)*3+1])), (i-1)*3+1]
    b <- pdf.oow[findInterval(rn[2], c(0, cdf.oow[,(i-1)*3+2])), (i-1)*3+2]
    c <- pdf.oow[findInterval(rn[3], c(0, cdf.oow[,(i-1)*3+3])), (i-1)*3+3]
    # Determine oil production over time from this well
    oil <- a*(1+b*c*t)^(-1/b)
    for (j in 1:length(oil)) {
      if (oil[j] < 0 | oil[j] == "NaN") {
        oil[j] <- 0
      }
      temp[j,h] <- oil[j]
    }
  }
  mc.oow[[i]] <- temp
}

# Define list to hold quantile stats for each field
quantile.mc.oow <- vector("list", length(field.ow))

# Pull stats on MC runs
for (j in 1:length(mc.oow)) {
  temp <- mc.oow[[j]]
  temp.oow <- matrix(0, nrow = field.ow.maxtime[j], ncol = 3)
  for (i in 1:field.ow.maxtime[j]) {
    temp.ow <- quantile(temp[i,], c(0.05, 0.50, 0.95))
    temp.oow[i,1] <- temp.ow[1]
    temp.oow[i,2] <- temp.ow[2]
    temp.oow[i,3] <- temp.ow[3]
  }
  quantile.mc.oow[[j]] <- temp.oow
}

#-------------------------------------------------------------------------------
# Monte-Carlo Simulation for GGW for each field
#-------------------------------------------------------------------------------
# Number of simulation runs for each field
nrun <- 10^5
# Define list of Monte-Carlo simulation results for each field
mc.ggw <- vector("list", length(field.gw))

for (i in 1:length(field.gw)) {
  # Define timestep sequence
  t <- seq(from = 1, to = field.gw.maxtime[i])
  # Define temporary matrix for holding simulation run results for field i
  temp <- matrix(0, nrow = length(t), ncol = nrun)
  for (h in 1:nrun) {
    # Generate random numbers for each coefficient for this well
    rn <- runif(3, min = 0, max = 1)
    # Use findInterval to pick index and assign value to each variable
    a <- pdf.ggw[findInterval(rn[1], c(0, cdf.ggw[,(i-1)*3+1])), (i-1)*3+1]
    b <- pdf.ggw[findInterval(rn[2], c(0, cdf.ggw[,(i-1)*3+2])), (i-1)*3+2]
    c <- pdf.ggw[findInterval(rn[3], c(0, cdf.ggw[,(i-1)*3+3])), (i-1)*3+3]
    # Determine oil production over time from this well
    gas <- a*(1+b*c*t)^(-1/b)
    for (j in 1:length(gas)) {
      if (gas[j] < 0 | gas[j] == "NaN") {
        gas[j] <- 0
      }
      temp[j,h] <- gas[j]
    }
  }
  mc.ggw[[i]] <- temp
}

# Define list to hold quantile stats for each field
quantile.mc.ggw <- vector("list", length(field.gw))

# Pull stats on MC runs
for (j in 1:length(mc.ggw)) {
  temp <- mc.ggw[[j]]
  temp.ggw <- matrix(0, nrow = field.gw.maxtime[j], ncol = 3)
  for (i in 1:field.gw.maxtime[j]) {
    temp.gw <- quantile(temp[i,], c(0.05, 0.50, 0.95))
    temp.ggw[i,1] <- temp.gw[1]
    temp.ggw[i,2] <- temp.gw[2]
    temp.ggw[i,3] <- temp.gw[3]
  }
  quantile.mc.ggw[[j]] <- temp.ggw
}

#-------------------------------------------------------------------------------
# Plot output - OOW
#-------------------------------------------------------------------------------
# Save results to PDF
pdf(file = "declineCurve MC v1.pdf")
for (i in 1:length(field.ow)) {
  # Pull quantile data from lists
  actual <- quantile.oow[[i]]
  mc <- quantile.mc.oow[[i]]
  # Pull Field aggregated decline curve fit coefficients
  a <- decline.field.ow[which(field.ow[i] == decline.field.ow[,1]),4]
  b <- decline.field.ow[which(field.ow[i] == decline.field.ow[,1]),5]
  c <- decline.field.ow[which(field.ow[i] == decline.field.ow[,1]),6]
  # Calculate field aggregated decline curve
  t <- 1:field.ow.maxtime[i]
  p = a*(1+b*c*t)^(-1/b)
  plot(actual[,3],
       type = "l",
       lty = 1,
       col = "blue",
       xlab = "Time (months)",
       ylab = "Oil Production (bbl per month)",
       main = paste("Actual vs. Monte-Carlo Decline Curve for Field", field.ow[i], sep = " "))
  lines(actual[,2], col = "black")
  lines(actual[,1], col = "red")
  lines(mc[,3], lty = 2, col = "blue")
  lines(mc[,2], lty = 2, col = "black")
  lines(mc[,1], lty = 2, col = "red")
  lines(p, lty = 6, col = "green")
  legend("topright",
         c("Actual 95%", "Actual 50%", "Actual 5%", "MC 95%", "MC 50%", "MC 5%", "Fit"),
         lty = c(1,1,1,2,2,2,6),
         col = c("blue", "black", "red", "blue", "black", "red", "green"))
}
dev.off()

# Save results to PDF (log scale)
pdf(file = "declineCurve MC v1 log scale.pdf")
for (i in 1:length(field.ow)) {
  # Pull quantile data from lists
  actual <- quantile.oow[[i]]
  mc <- quantile.mc.oow[[i]]
  # Pull Field aggregated decline curve fit coefficients
  a <- decline.field.ow[which(field.ow[i] == decline.field.ow[,1]),4]
  b <- decline.field.ow[which(field.ow[i] == decline.field.ow[,1]),5]
  c <- decline.field.ow[which(field.ow[i] == decline.field.ow[,1]),6]
  # Calculate field aggregated decline curve
  t <- 1:field.ow.maxtime[i]
  p = a*(1+b*c*t)^(-1/b)
  plot(actual[,3],
       type = "l",
       lty = 1,
       col = "blue",
       xlab = "Time (months)",
       ylab = "Oil Production (bbl per month)",
       main = paste("Actual vs. Monte-Carlo Decline Curve for Field", field.ow[i], sep = " "),
       log = "y",
       ylim = c(0.1, 10000))
  lines(actual[,2], col = "black")
  lines(actual[,1], col = "red")
  lines(mc[,3], lty = 2, col = "blue")
  lines(mc[,2], lty = 2, col = "black")
  lines(mc[,1], lty = 2, col = "red")
  lines(p, lty = 6, col = "green")
  legend("topright",
         c("Actual 95%", "Actual 50%", "Actual 5%", "MC 95%", "MC 50%", "MC 5%", "Fit"),
         lty = c(1,1,1,2,2,2,6),
         col = c("blue", "black", "red", "blue", "black", "red", "green"))
}
dev.off()

#-------------------------------------------------------------------------------
# Plot output - GGW
#-------------------------------------------------------------------------------
# Save results to PDF
pdf(file = "GGW declineCurve MC v2.pdf")
for (i in 1:length(field.gw)) {
  # Pull quantile data from lists
  actual <- quantile.ggw[[i]]
  mc <- quantile.mc.ggw[[i]]
  # Pull Field aggregated decline curve fit coefficients
  a <- decline.field.gw[which(field.gw[i] == decline.field.gw[,1]),7]
  b <- decline.field.gw[which(field.gw[i] == decline.field.gw[,1]),8]
  c <- decline.field.gw[which(field.gw[i] == decline.field.gw[,1]),9]
  # Calculate field aggregated decline curve
  t <- 1:field.gw.maxtime[i]
  p = a*(1+b*c*t)^(-1/b)
  plot(actual[,3],
       type = "l",
       lty = 1,
       col = "blue",
       xlab = "Time (months)",
       ylab = "Gas Production (MCF per month)",
       main = paste("Actual vs. Monte-Carlo Decline Curve for Field", field.gw[i], sep = " "))
  lines(actual[,2], col = "black")
  lines(actual[,1], col = "red")
  lines(mc[,3], lty = 2, col = "blue")
  lines(mc[,2], lty = 2, col = "black")
  lines(mc[,1], lty = 2, col = "red")
  lines(p, lty = 6, col = "green")
  legend("topright",
         c("Actual 95%", "Actual 50%", "Actual 5%", "MC 95%", "MC 50%", "MC 5%", "Fit"),
         lty = c(1,1,1,2,2,2,6),
         col = c("blue", "black", "red", "blue", "black", "red", "green"))
}
dev.off()

# Save results to PDF (log scale)
pdf(file = "GGW declineCurve MC v2 log scale.pdf")
for (i in 1:length(field.gw)) {
  # Pull quantile data from lists
  actual <- quantile.ggw[[i]]
  mc <- quantile.mc.ggw[[i]]
  # Pull Field aggregated decline curve fit coefficients
  a <- decline.field.gw[which(field.gw[i] == decline.field.gw[,1]),7]
  b <- decline.field.gw[which(field.gw[i] == decline.field.gw[,1]),8]
  c <- decline.field.gw[which(field.gw[i] == decline.field.gw[,1]),9]
  # Calculate field aggregated decline curve
  t <- 1:field.gw.maxtime[i]
  p = a*(1+b*c*t)^(-1/b)
  plot(actual[,3],
       type = "l",
       lty = 1,
       col = "blue",
       xlab = "Time (months)",
       ylab = "Gas Production (MCF per month)",
       main = paste("Actual vs. Monte-Carlo Decline Curve for Field", field.ow[i], sep = " "),
       log = "y",
       ylim = c(0.1, 10000))
  lines(actual[,2], col = "black")
  lines(actual[,1], col = "red")
  lines(mc[,3], lty = 2, col = "blue")
  lines(mc[,2], lty = 2, col = "black")
  lines(mc[,1], lty = 2, col = "red")
  lines(p, lty = 6, col = "green")
  legend("topright",
         c("Actual 95%", "Actual 50%", "Actual 5%", "MC 95%", "MC 50%", "MC 5%", "Fit"),
         lty = c(1,1,1,2,2,2,6),
         col = c("blue", "black", "red", "blue", "black", "red", "green"))
}
dev.off()