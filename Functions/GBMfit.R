# Function Info -----------------------------------------------------------
# Name:      GBMfit.R (Geometric Brownian Motion Price Path Fitting)
# Author(s): Jon Wilkey, Michael Hogue
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# EP.CPI.basis - EIA Historical Energy Prices CPI Basis (i.e. the CPI index
# value for the year to which all oil/gas prices in the EIA_HistPrice.csv file
# have been adjusted to).

# cpi - CPI basis for model

# version - Version number for file naming of exported data.frames


# Outputs -----------------------------------------------------------------

# GBMfitOP & GBMfitGP - data.frames for OP and GP containing GBM fit parameters
# "v" (volatility) and "mu" (drift)


# Description -------------------------------------------------------------

# This function loads a *.csv file containing oil and gas prices and uses 
# Maximum Likelihood Estimation to fit the parameters "v" and "mu" to those 
# price paths, which can then subsequently be used to generate random price
# paths according to GBM.


# Function ----------------------------------------------------------------

GBMfit <- function(path, EP.CPI.basis, cpi, version) {
  # Load historical wellhead price data -------------------------------------
  
  # Make a *.csv file with the following format: [month, oilprice, gasprice].
  # The first column is the month associated with each row of the price data,
  # the second column is the oil price at the wellhead from the following source
  # [1] listed below in EP.CPI.basis real dollars per bbl, and the last column
  # is the natural gas wellhead price in the same real dollars per MMBtu from
  # source [2]. Save the file as "EIA_HistPrices.csv" and place it in your raw
  # data file path.
  
  # Sources:
  # [1] http://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=f004049__3&f=a
  # [2] http://www.eia.gov/dnav/ng/hist/na1140_sut_3a.htm
  
  # Load the EIA_HistPrices.csv file and rename columns
  eia.hp <- read.csv(file.path(path$raw, "EIA_HistPrices.csv"))
  names(eia.hp) <- c("month", "OP", "GP")
  
  # Drop years with incomplete price information (safe guard, source *.csv file
  # should be complete).
  eia.hp <- na.omit(eia.hp)
  
  # Create vectors of OP and GP in inflation adjusted CPI-year real dollars
  OP <- inf_adj(price = eia.hp$OP, index = EP.CPI.basis, basis = cpi)
  GP <- inf_adj(price = eia.hp$GP, index = EP.CPI.basis, basis = cpi)
  
  # Create vector of dates for eia.hp$month that are truncated to nearest month
  dates <- as.yearmon(eia.hp$month)
  
  # Create an ordered time series object out of OP and GP so that we can use time
  # series functions such as diff().
  OP.z <- zoo(OP, order.by=dates)
  GP.z <- zoo(GP, order.by=dates)
  
  
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
  
  save(file = file.path(path$data,
                        paste("GBMfit_", version, ".rda", sep = "")),
       list = c("GBMfitOP",
                "GBMfitGP"))
}