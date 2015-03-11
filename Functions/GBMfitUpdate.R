# Function Info -----------------------------------------------------------
# Name:      GBMfitUpdate.R (Geometric Brownian Motion Price Path Fitting)
# Author(s): Jon Wilkey, Michael Hogue
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# eia.hp - data.frame of EIA historical energy prices with observations "month"
# for time index, "OP" for first purchase price (FFP) of oil in Utah ($/bbl,
# inflation adjusted to the date associated with "cpi" option input), and "GP"
# for FFP of gas in Utah ($/MMBtu, also inflation adjusted)

# tstart - start date of prices to include in analysis

# tstop - stop date of prices to include in analysis

# ver - Version number for file naming of exported data.frames


# Outputs -----------------------------------------------------------------

# GBMfitOP & GBMfitGP - data.frames for OP and GP containing GBM fit parameters
# "v" (volatility) and "mu" (drift)


# Description -------------------------------------------------------------

# This function uses Maximum Likelihood Estimation to fit the parameters "v" and
# "mu" to the price paths in eia.hp within the tstart to tstop time period.
# These parameters can then subsequently be used to generate random price paths
# according to GBM.


# Function ----------------------------------------------------------------

GBMfitUpdate <- function(path, eia.hp, tstart, tstop, ver) {
  
  # Define vectors from eia.hp columns
  dates <- eia.hp$month
  OP <- eia.hp$OP
  GP <- eia.hp$GP
  
  # Create an ordered time series object out of OP and GP so that we can use time
  # series functions such as diff().
  OP.z <- zoo(OP, order.by=dates)
  GP.z <- zoo(GP, order.by=dates)
  
  # Subset to specified time range
  OP.z <- window(OP.z, start = as.yearmon(tstart), end = as.yearmon(tstop))
  GP.z <- window(GP.z, start = as.yearmon(tstart), end = as.yearmon(tstop))
  
  
  # Fit GBM parameters ------------------------------------------------------
  
  # Calculate the mean of the first differences of the log prices.
  m.OP <- mean(diff(log(OP.z)))
  m.GP <- mean(diff(log(GP.z)))
  
  # Following are the Maximum Likelihood Estimation (MLE) for GBM parameters
  
  # [1] 'v' is the squared volatility
  v.OP <- mean((diff(log(OP.z))-m.OP)**2)
  v.GP <- mean((diff(log(GP.z))-m.GP)**2)
  
  # [2] 'mu' is drift
  mu.OP <- m.OP + v.OP/2
  mu.GP <- m.GP + v.GP/2
  
  # Create data.frame with fit results for export
  GBMfitOP <- data.frame(v.OP, mu.OP); names(GBMfitOP) <- c("v", "mu")
  GBMfitGP <- data.frame(v.GP, mu.GP); names(GBMfitGP) <- c("v", "mu")
  
  
  # Export results ----------------------------------------------------------
  
  save(file = file.path(path$data, paste("GBMfit_", ver, ".rda", sep = "")),
       list = c("GBMfitOP",
                "GBMfitGP"))
}