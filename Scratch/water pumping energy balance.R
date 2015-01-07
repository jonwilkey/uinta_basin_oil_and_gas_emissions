# energybalance.R

# Inputs ------------------------------------------------------------------

# disposal.rda - DOGM data on water disposed of via injeciton wells

# production.rda - DOGM data on production volumes (and other well data)

# timesteps - listing of months over which to model energy balance


# Outputs -----------------------------------------------------------------

# Plot of energy out vs. energy for pumping water into deep injection wells


# Description -------------------------------------------------------------

# This script compares the energy from produced oil and gas vs. the energy used 
# to pump water into deep injection wells in the Uinta Basin. The key
# assumptions here are:

# (1) The work for pumping is W = (volume produced water) * (max tubing press.)
# (2) That the pump is 100% efficient
# (3) That the energy content of oil is 5.8 GJ/bbl and gas is 1.06 GJ/MCF


# Options -----------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------

raw_root <-  "D:/Dropbox/CLEAR/DOGM Data/Raw Data"      # Raw data
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data" # Prepared data
work_root <- "C:/Users/Jon/Documents/R/ub_oilandgas/"   # Working directory
fin <-       "D:/Dropbox/CLEAR/DOGM Data/Functions"     # Functions
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"         # Plots

# Set working directory
setwd(work_root)


# Functions ---------------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(fin,c("write_excel.R"))

# Load each function in list
for (f in flst) source(f)


# Libraries ---------------------------------------------------------------

library(sqldf)
library(zoo)


# Load required data files ------------------------------------------------

# Load *.rda files
load(file.path(raw_root, "disposal.rda"))
load(file.path(data_root, "production.rda"))

# Rename some dataframes for brevity
d <- disposal
p <- production
remove(disposal, production)


# Inputs ------------------------------------------------------------------

# Create a sequence of dates from start of modeling period to final date in
# dataframe v (2013-11-01)
timesteps <- seq(from = as.Date("1999-01-01"), to = as.Date("2012-12-01"),
            by = "months")


# Calculate pumping energy for produced water disposal --------------------

# Determine pumping energy and join to d
# Equation: (Work) = (Volume Pumped)*(Pressure change)
# Units: conversion factors for bbl to m^3 and psi to Pa, result is in GJ
# Assumptions: Pump efficiency = 1, max tubing pressure = pump pressure
d$energy <- d$Volume.Liquid*d$Prs..Tubing.Max*(42/264.172*6894.75729)/1e9

# Put reporting dates in dataframe d into YYYY-MM-DD format
d$Report.Date <- as.Date(d$Report.Date, "%m/%d/%Y")

# Round reporting dates to nearest month
d$Report.Date <- as.Date(as.yearmon(d[,"Report.Date"]))

# Pull county portion out of well API # and add as additional column to d
d$county <- substr(x = d$API.Well.Number, start = 4, stop = 6)

# Rename columns
names(d) <- c("api", "well_name", "location", "section", "date", "inject_days",
              "volume_liquid", "volume_gas", "tube_press", "energy", "county")

# Get sum of all energy from wells in Uinta and Duchesne counties as timeseries
energy.inj <- sqldf("select date, sum(energy) from d where county = '013' or county = '047' group by date")

# Drop dates prior to start of modeling period
energy.inj <- energy.inj[which(energy.inj$date >= min(timesteps) &
                               energy.inj$date <= max(timesteps)),]


# Determine energy out ----------------------------------------------------

psub <- sqldf("select p_rpt_period, sum(p_oil_prod), sum(p_gas_prod)
              from p
              where w_county = 'UINTAH' or w_county = 'DUCHESNE'
              group by p_rpt_period")

# Rename columns
names(psub) <- c("date", "sum_oil", "sum_gas")

# Retype summations to numerical values (result from SQL query is character
# string)
psub$sum_oil <- as.numeric(psub$sum_oil)
psub$sum_gas <- as.numeric(psub$sum_gas)

# Extract data from modeling period only
psub <- psub[which(psub$date >= min(timesteps) & psub$date <= max(timesteps)),]

# Calculate total energy of gas and oil production volumes
# Unit conversion factors: bbl of oil = 5.8 GJ, MCF of gas = 1.06 GJ
energy.out <- 5.8*psub$sum_oil+1.06*psub$sum_gas


# Plot output -------------------------------------------------------------
# Save to pdf
pdf(file.path(plot_root, file = "energybalance_v1 Results.pdf"))

plot(timesteps, energy.out,
     type = "l",
     col = "blue",
     lwd = 2,
     log = "y",
     xlab = "Date",
     ylab = "Energy (GJ)",
     ylim = c(1e3, 1e8),
     main = "Energy Out vs. Pumping Energy for Water Injection")
lines(timesteps, test, col = "red", lwd = 2)
abline(v=seq(from = as.Date("1999-01-01"), to = as.Date("2012-12-01"),
             by = "year"),
       h=c(seq(1e3, 1e4, 1e3),
           seq(1e4, 1e5, 1e4),
           seq(1e5, 1e6, 1e5),
           seq(1e6, 1e7, 1e6),
           seq(1e7, 1e8, 1e7)),
       col="grey10")
legend("topleft",
       bg = "white",
       c("Energy Out", "Energy Inj."),
       lty = c(1,1),
       lwd = c(2,2),
       col = c("blue", "red"),
       ncol = 2)

# Close PDF
dev.off()