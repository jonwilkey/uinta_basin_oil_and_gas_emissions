# Function Info -----------------------------------------------------------
# Name:      EIAerrorUpdateLT.R (EIA Annual Energy Outlook Long-term Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

#op/gp.FC.high/ref/low - EIA high/ref/low oil and gas forecasts interpolated on
#a monthly basis and in constant model basis dollars

# path - List object containing directory paths for file I/O

# xq - Vector of probabilities at which to estimate CDF for % forecasting errors

# ver - File version number


# Outputs -----------------------------------------------------------------

# Eoil/Egas.LT - matrix of oil/gas prices with rows = quantiles in xq and 
# columns = months into the future that forcast is predicting (same length as
# input EIA forecasts).


# Description -------------------------------------------------------------

# This function fits a log-normal distribution to the high/ref/low EIA forecasts
# for oil and gas in each monthly time step and then returns the quantiles of 
# the fitted distribution at each values specified in the vector xq. The result
# is saved out as Eoil/Egas.LT.


# Function ----------------------------------------------------------------

EIAerrorUpdateLT <- function(op.FC.high, op.FC.ref, op.FC.low,
                             gp.FC.high, gp.FC.ref, gp.FC.low,
                             path, xq, ver) {
  
  # Generate error matrices -------------------------------------------------
  
  # Combined input forecasts into data.frames
  OPF <- data.frame(high = op.FC.high, ref = op.FC.ref, low = op.FC.low)
  GPF <- data.frame(high = gp.FC.high, ref = gp.FC.ref, low = gp.FC.low)
  
  # Predefine matrices
  Eoil.LT <- matrix(0, nrow = length(xq), ncol = nrow(OPF))
  Egas.LT <- Eoil.LT
  
  # For each time step
  for (i in 1:nrow(OPF)) {
    
    otemp <- coef(fitdist(as.numeric(OPF[i, ]), "lnorm"))
    gtemp <- coef(fitdist(as.numeric(GPF[i, ]), "lnorm"))
    
    # Get log-normal quantiles
    Eoil.LT[, i] <- qlnorm(p = xq, meanlog = otemp[1], sdlog = otemp[2])
    Egas.LT[, i] <- qlnorm(p = xq, meanlog = gtemp[1], sdlog = gtemp[2])
  }
  
  
  # Save results ------------------------------------------------------------
  
  # Save
  save(file = file.path(path$data, paste("EIAerrorLT_", ver, ".rda", sep = "")),
       list = c("Eoil.LT",
                "Egas.LT"))
}
