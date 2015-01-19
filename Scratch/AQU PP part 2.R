
# Save to PDF? (yes or no)
pdfFlag <- "yes"

# If saving to PDF, name pdf as:
pdfName <- "Revised Plots for AQU part 2.pdf"

# Mac
raw_root  <- "/Users/john/Dropbox/CLEAR/DOGM Data/Raw Data"
data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"
plot_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"
fin <-       "/Users/john/Documents/ub_oilandgas/ub_oilandgas/Functions"
work_root <- "/Users/john/Documents/ub_oilandgas/ub_oilandgas"


# Load required data files ------------------------------------------------

# Simulation results from simDriver.R
load(file.path(data_root, "simResults.rda"))

# Actual well and production data
load(file.path(data_root, "psim_actual_v1.rda"))
load(file.path(data_root, "wsim_actual_v1.rda"))


# Format Simulation Results -----------------------------------------------

# Pull water balance terms out of water list
prod.water      <- water.balance[[1]]
disp.water      <- water.balance[[2]]
evap.water      <- water.balance[[3]]
rec.water       <- water.balance[[4]]
drill.water     <- water.balance[[5]]
frac.water      <- water.balance[[6]]
flood.water     <- water.balance[[7]]
water.in        <- water.balance[[8]]
water.intensity <- water.balance[[9]]

# Pull GHG cO2 data.frame out of emissions list
ghg <- emissions[[1]]
ch4 <- emissions[[2]]


# Queries and Calculations ------------------------------------------------

# Number of wells drilled and oil Production as timeseries simulated vs actual
pmt <- proc.time()
wellcount.sim <- matrix(0, nrow = max(wsim$runID), ncol = max(wsim$tDrill))
ptot.sim.oil <- wellcount.sim
for (i in 1:max(wsim$runID)) {
  for (j in 1:max(wsim$tDrill)) {
    wellcount.sim[i,j] <- length(which(wsim$runID == i & wsim$tDrill == j))
    ptot.sim.oil[i,j] <- sum(psim[which(wsim$runID == i & wsim$wellType == "OW"),j])
  }
}
wellcount.act <- sqldf("select count(wellID) from 'wsim.actual' group by tDrill")
ptot.act.oil <- colSums(psim.actual[which(wsim.actual$wellType == "OW"),], na.rm = TRUE)
proc.time() - pmt

# Quantiles
quant <- c(0.9, 0.7, 0.5, 0.3, 0.1)
wellcount.sim.quant <- matrix(0, nrow = length(quant), ncol = max(wsim$tDrill))
ptot.sim.oil.quant <- wellcount.sim.quant
ghg.quant <- wellcount.sim.quant
ch4.quant <- wellcount.sim.quant
water.quant <- wellcount.sim.quant
for (i in 1:max(wsim$tDrill)) {
  wellcount.sim.quant[,i] <- quantile(wellcount.sim[,i], quant)
  ptot.sim.oil.quant[,i] <- quantile(ptot.sim.oil[,i], quant)
  ghg.quant[,i] <- quantile(ghg[,i], quant)
  ch4.quant[,i] <- quantile(ch4[,i], quant)
  water.quant[,i] <- quantile(water.intensity[,i], quant)
}

jobs.RIMS.quant <- matrix(0, nrow = length(quant), ncol = max(wsim$tDrill)/12)
jobs.work.quant <- jobs.RIMS.quant
for (i in 1:(max(wsim$tDrill)/12)) {
  jobs.RIMS.quant[,i] <- quantile(jobs.RIMS[,i], quant)
  jobs.work.quant[,i] <- quantile(jobs.workload[,i], quant)
}

# Define vector of timesteps
timesteps <- c(1:168)


# Plotting ----------------------------------------------------------------

# Save plots to PDF as pdfName if pdfFLag == "yes"
if(pdfFlag == "yes") pdf(file.path(plot_root, file = pdfName))

# Set line colors for quantiles used in quant
linecolor <- rainbow(length(quant))

# Production timeseries for oil vs actual
plot(timesteps, ptot.sim.oil.quant[3,],
     type = "l",
     cex.axis = axisSize,
     cex.lab = labSize,
     lwd = lineSize,
     col = "blue",
     xlab = "Time (months)",
     ylab = "Oil Production (bbl)",
     main = "Total Oil Production - Simulated vs. Actual")

lines(timesteps, ptot.act.oil, lwd = lineSize, col = "black")
legend("topleft",
       c("Actual", "50%"),
       col = c("black", "blue"), lty = 1)

# GHG CO2e timeseries
plot(timesteps, ghg.quant[1,],
     type = "l",
     cex.axis = axisSize,
     cex.lab = labSize,
     lwd = lineSize,
     col = linecolor[1],
     xlab = "Time (months)",
     ylab = "CO2e Emissions (1e3 kg)",
     main = "Total CO2 Equivalent Emissions Estimate")
for (i in 2:length(quant)) {
  lines(timesteps,ghg.quant[i,], col = linecolor[i], lwd = lineSize)
}
legend("topleft",
       c("90%", "70%", "50%", "30%", "10%"),
       ncol = 2, col = c(linecolor), lty = 1)


# GHG CH4 timeseries
plot(timesteps, ch4.quant[1,],
     type = "l",
     col = linecolor[1],
     cex.axis = axisSize,
     cex.lab = labSize,
     lwd = lineSize,
     xlab = "Time (months)",
     ylab = "CH4 Emissions (MCF CH4)",
     main = "Total CH4 Emissions Estimate from Gas Wells")
for (i in 2:length(quant)) {
  lines(timesteps,ch4.quant[i,], col = linecolor[i], lwd = lineSize)
}
legend("topleft",
       c("90%", "70%", "50%", "30%", "10%"),
       ncol = 2, col = c(linecolor), lty = 1)

# End PDF
if(pdfFlag == "yes") dev.off()