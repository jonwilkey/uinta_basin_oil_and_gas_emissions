# Function Info -----------------------------------------------------------
# Name:      EIAforecastUpdate.R (EIA Price Forecast Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# forecast - data.frame with EIA AEO reference forecasts for oil and gas on an
# annual basis

# basis - CPI value used as basis for dollar values in simulation

# EIAbasis - CPI value used as basis for EIA reference forecasts and prices in
# forecast data.frame

# tsteps - vector of dates for simulation period

# oil.fpp.init - Initial first purchase price (FPP) of oil in basis $/bbl in
# timestep immediately preceeding start of simulation period

# gas.fpp.init - Same as above but for gas, and in $/MCF

# FPPdate - Date associated with oil/gas FPPs (e.g. "2004-12-01")

# ver - File version number

# path - List object containing directory paths for file I/O

# type - character switch indicating what kind of forecast is being generated.
# Valid options are "ref", "high", or "low".


# Outputs -----------------------------------------------------------------

# op.FC/gp.FC - the interpolated and inflation adjusted vector of oil prices
# (op.FC, in $/bbl) and gas prices (gp.FC, in $/MCF) according to EIA's
# reference forecast.


# Description -------------------------------------------------------------

# This function adjusts the EIA AEO forecasts contained in the 
# data.frame forecast for (1) inflation and then (2) interpolates monthly 
# oil/gas prices from the annual forecast values, assuming linear interpolation
# and that the EIA AEO forecast prices occur during the middle of each calendar
# year (i.e. June).


# Function ----------------------------------------------------------------

EIAforecastUpdate <- function(forecast, basis, EIAbasis, tsteps, oil.fpp.init,
                              gas.fpp.init, FPPdate, ver, path, type) {
  
  # Internal values - uncomment to debug ------------------------------------
  
#   forecast <-     opt$forecast
#   basis <-        opt$cpi
#   EIAbasis <-     opt$EIAbasis
#   tsteps <-       opt$tsteps
#   oil.fpp.init <- opt$oil.fpp.init
#   gas.fpp.init <- opt$gas.fpp.init
#   FPPdate <-      opt$FPPdate
#   ver <-          opt$file_ver
  
  
  # Adjust EIA AEO forecasts ------------------------------------------------
  
  # Adjust for inflation
  forecast$oil <- forecast$oil*(basis/EIAbasis)
  forecast$gas <- forecast$gas*(basis/EIAbasis)
  
  # The next section of code uses the zoo() library to covert from EIA's annual
  # prices to monthly prices using linear interpolation.
  
  # Make zoo object for FPPs
  opFPP <- zoo(oil.fpp.init, FPPdate)
  gpFPP <- zoo(gas.fpp.init, FPPdate)
  
  # Create zoo object for monthly oil and gas prices with length = tsteps and
  # initial element of each zoo object = FPP of oil or gas.
  op <- c(opFPP, zoo(x = rep(NA, times = length(tsteps)), order.by = tsteps))
  gp <- c(gpFPP, zoo(x = rep(NA, times = length(tsteps)), order.by = tsteps))
  
  # Next, make zoo object for from forecast matrix for both oil and gas forecasts
  foil <- zoo(x = forecast$oil, order.by = forecast$year)
  fgas <- zoo(x = forecast$gas, order.by = forecast$year)
  
  # Merge the two sets of zoo objects together by moving along the op/gp vector.
  # If the time index of op/gp is equal to the time index of foil/fgas, then 
  # replace the NA value of that position in the op/gp vector with the value in 
  # foil/fgas and increment the counter j by one (j is counter for vector
  # position along foil/fgas vector).
  
  # Initial value for counter
  j <- 1
  
  # For each time index of op/gp
  for (i in 1:length(op)) {
    
    # Check - do we need to continue merging?
    if (j < length(foil)+1) {
      
      # Check - is the current index i have the same time value as index on
      # foil/fgas?
      if (index(op[i]) == index(foil[j])) {
        
        # If so, replace value of op/gp at i with value of foil/fgas at j
        op[i] <- foil[j]
        gp[i] <- fgas[j]
        
        # Increment counter
        j <- j+1
      }
    }
  }
  
  # Fill in the NAs by linear approximation with na.approx. By using rule = 2,
  # any dates which are outside of the last data point will be estimated by
  # extrapolating from the nearest datapoint.
  op <- coredata(na.approx(op, rule = 2))
  gp <- coredata(na.approx(gp, rule = 2))
  
  # Finally, since first element of op/gp is initial FPP, drop value the first
  # element of both vectors and rename as *.FC
  op.FC <- op[-1]
  gp.FC <- gp[-1]
  
  
  # Save results ----------------------------------------------------------
  
  # Depending on update type, change forecast object name and save instructions
  switch(type,
         
         # Reference case
         "ref" = {
           
           # Forecast
           op.FC.ref <- op.FC
           gp.FC.ref <- gp.FC
           flist <-     c("op.FC.ref", "gp.FC.ref")
           
           # File name
           fn <- paste("EIAforecast_ref_", ver, ".rda", sep = "")
         },
         
         # Low oil price
         "low" = {
           
           # Forecast
           op.FC.low <- op.FC
           gp.FC.low <- gp.FC
           flist <-     c("op.FC.low", "gp.FC.low")
           
           # File name
           fn <- paste("EIAforecast_low_", ver, ".rda", sep = "")
         },
         
         # High oil price
         "high" = {
           
           # Forecast
           op.FC.high <- op.FC
           gp.FC.high <- gp.FC
           flist <-     c("op.FC.high", "gp.FC.high")
           
           # File name
           fn <- paste("EIAforecast_high_", ver, ".rda", sep = "")
         }
         )
  
  save(file = file.path(path$data, fn), list = flist)
}