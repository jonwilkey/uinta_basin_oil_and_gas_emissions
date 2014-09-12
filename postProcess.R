# Post-processing Results from simDriver.R

# Inputs ------------------------------------------------------------------

# simData.rda - collection of all the resulting data.tables and data.frames from
# executing the Monte-Carlo simulation in simDriver.R


# Outputs -----------------------------------------------------------------

# Various plots of Monte-Carlo Simulation results (depending on options set
# below)


# Description -------------------------------------------------------------

# This script generates plots the results from the Monte-Carlo simulation
# performed in simDriver.R in various ways. The user sets options in the
# "Options" segment of the code below. The script then queries the simulation
# data and executes the desired plotting functions, saving the plots as PDFs.


# Options -----------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

# Save to PDF? (yes or no)
pdfFlag <- "no"

# If saving to PDF, name pdf as:
pdfName <- "Total Oil Example Plot.pdf"


# Paths -------------------------------------------------------------------

# Windows
raw_root  <- "D:/Dropbox/CLEAR/DOGM Data/Raw Data"             # Raw data
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"        # Prepared data
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"                # Plots
fin <-       "C:/Users/Jon/Documents/R/ub_oilandgas/Functions" # Functions
work_root <- "C:/Users/Jon/Documents/R/ub_oilandgas/"          # Working dir.

# # Mac
# raw_root  <- "/Users/john/Dropbox/CLEAR/DOGM Data/Raw Data"
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"
# plot_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"
# fin <-       "/Users/john/Documents/ub_oilandgas/ub_oilandgas/Functions"
# work_root <- "/Users/john/Documents/ub_oilandgas/ub_oilandgas"

# Set working directory
setwd(work_root)


# Functions ---------------------------------------------------------------
# List of functions used in this script to be loaded here
flst <- file.path(fin,c("write_excel.R"))

# Load each function in list
for (f in flst) source(f)


# Libraries ---------------------------------------------------------------
library(data.table)  # For wsim data.table (maybe others in the future)
library(sqldf)
library(rggobi)      # For interactive plotting through GGobi 

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


# GGobi -------------------------------------------------------------------

# Make data.frame for calling with GGobi with all of the results you wish to
# include
results <- data.frame(timesteps,
                      t(wellcount.sim.quant),
                      t(ptot.sim.oil.quant),
                      t(ghg.quant),
                      t(ch4.quant),
                      t(water.quant))

# Rename for review
names(results) <- c("Time Step",
                    "Wells Drilled - 90%",
                    "Wells Drilled - 70%",
                    "Wells Drilled - 50%",
                    "Wells Drilled - 30%",
                    "Wells Drilled - 10%",
                    "Oil Production (bbl) - 90%",
                    "Oil Production (bbl) - 70%",
                    "Oil Production (bbl) - 50%",
                    "Oil Production (bbl) - 30%",
                    "Oil Production (bbl) - 10%",
                    "GHG Emissions (metric tons CO2e) - 90%",
                    "GHG Emissions (metric tons CO2e) - 70%",
                    "GHG Emissions (metric tons CO2e) - 50%",
                    "GHG Emissions (metric tons CO2e) - 30%",
                    "GHG Emissions (metric tons CO2e) - 10%",
                    "CH4 Emissions (MCF CH4) - 90%",
                    "CH4 Emissions (MCF CH4) - 70%",
                    "CH4 Emissions (MCF CH4) - 50%",
                    "CH4 Emissions (MCF CH4) - 30%",
                    "CH4 Emissions (MCF CH4) - 10%",
                    "Water In Ratio (water)/(oil) - 90%",
                    "Water In Ratio (water)/(oil) - 70%",
                    "Water In Ratio (water)/(oil) - 50%",
                    "Water In Ratio (water)/(oil) - 30%",
                    "Water In Ratio (water)/(oil) - 10%")

# Call GGobi
ggobi(results)

# Plotting ----------------------------------------------------------------

# # Save plots to PDF as pdfName if pdfFLag == "yes"
# if(pdfFlag == "yes") pdf(file.path(plot_root, file = pdfName))
# 
# # Set line colors for quantiles used in quant
# linecolor <- rainbow(length(quant))
# 
# # # Number of wells drilled as timeseries simulated vs actual
# # plot(timesteps, wellcount.sim.quant[1,],
# #      type = "l",
# #      col = linecolor[1],
# #      xlab = "Time",
# #      ylab = "Wells Drilled",
# #      ylim = c(0, max(wellcount.sim.quant[1,])),
# #      main = "Drilling Schedule - Simulation vs. Actual")
# # for (i in 2:length(quant)) {
# #   lines(timesteps,wellcount.sim.quant[i,], col = linecolor[i])
# # }
# # lines(timesteps, wellcount.act[[1]], lwd = 1)
# # legend("topleft",
# #        c("90%", "70%", "50%", "30%", "10%", "Actual"),
# #        ncol = 2, col = c(linecolor,"black"), lty = 1)
# 
# 
# # Boxplots of Decline curve coefficients by field
# 
# # Production timeseries for oil vs actual
# plot(timesteps, ptot.sim.oil.quant[1,],
#      type = "l",
#      col = linecolor[1],
#      xlab = "Time (months)",
#      ylab = "Oil Production (bbl)",
#      main = "Total Oil Production - Simulated vs. Actual")
# for (i in 2:length(quant)) {
#   lines(timesteps,ptot.sim.oil.quant[i,], col = linecolor[i])
# }
# lines(timesteps, ptot.act.oil, lwd = 1)
# legend("topleft",
#        c("90%", "70%", "50%", "30%", "10%", "Actual"),
#        ncol = 2, col = c(linecolor,"black"), lty = 1)
# 
# 
# # Production time series for gas vs actual
# 
# # Production from individual well by field vs actual
# 
# # Royalties, severance taxes, property taxes, corporate income taxes (federal
# # and state) as timeseries
# 
# # RIMS vs. BEA mining jobs data
# 
# 
# # Workload vs. DWFS oil and gas jobs data
# 
# 
# # GHG CO2e timeseries
# plot(timesteps, ghg.quant[1,],
#      type = "l",
#      col = linecolor[1],
#      xlab = "Time (months)",
#      ylab = "CO2e Emissions (1e3 kg)",
#      main = "Total CO2 Equivalent Emissions Estimate")
# for (i in 2:length(quant)) {
#   lines(timesteps,ghg.quant[i,], col = linecolor[i])
# }
# legend("topleft",
#        c("90%", "70%", "50%", "30%", "10%"),
#        ncol = 2, col = c(linecolor), lty = 1)
# 
# 
# # GHG CH4 timeseries
# plot(timesteps, ch4.quant[1,],
#      type = "l",
#      col = linecolor[1],
#      xlab = "Time (months)",
#      ylab = "CH4 Emissions (MCF CH4)",
#      main = "Total CH4 Emissions Estimate from Gas Wells")
# for (i in 2:length(quant)) {
#   lines(timesteps,ch4.quant[i,], col = linecolor[i])
# }
# legend("topleft",
#        c("90%", "70%", "50%", "30%", "10%"),
#        ncol = 2, col = c(linecolor), lty = 1)
# 
# # End PDF
# if(pdfFlag == "yes") dev.off()