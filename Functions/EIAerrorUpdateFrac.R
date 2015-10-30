# Function Info -----------------------------------------------------------
# Name:      EIAerrorUpdate.R (EIA Annual Energy Outlook Error Analysis Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# xq - Vector of probabilities at which to estimate CDF for % forecasting errors

# tsteps - Number of time steps to analyze % error (i.e. how far into the future
# the forecast is predicting)

# ver - File version number

# type - character string specifying relative error quantification method. Valid
# options are "RE Direct" for relative error with directionality (for RE = (FP -
# AP) / FP data where RE = relative error, FP = forecasted price, and AP = 
# actual price) or "RE Frac" for relative fractional error (for RE = FP / AP IFF
# AP > FP | RE = AP / FP IFF FP > AP data).


# Outputs -----------------------------------------------------------------

# Eoil/Egas - matrix of % relative error in with rows = probabilities and 
# columns = years into the future that forcast is predicting. Probabilities /
# number of rows will match up with xq, years into the future / number of
# columns will match up with tsteps.


# Description -------------------------------------------------------------

# This function loads *.csv files containing the percent relative error in EIA's
# AEO oil and gas reference forecasts to actual oil and gas prices. These files
# are converted into a data.frame, and qnorm() is used to generate the
# cumulative distribution function for the error % for each column (the number
# of years into the future being predicted) under the assumption that the %
# errors can be modeled as a normal distribution. The resulting CDF matrices
# Eoil and Egas are then saved.


# Function ----------------------------------------------------------------

EIAerrorUpdateFrac <- function(path, xq, tsteps, ver) {
  
  # Load EIA error *.csv files ----------------------------------------------
  
  # Read in source data from *.csv files. The *.csv files should be structured as
  # follows: columns = AEO reports, rows = time steps, values = % error between
  # actual price and predicted price. Lower right of corner of export matrix
  # should all be NA.
  egas <- read.csv(file.path(path$raw, "EIA AEO frac error gp export.csv"))
  eoil <- read.csv(file.path(path$raw, "EIA AEO frac error op export.csv"))
  
  
  # Fit beta distribution to annual errors ----------------------------------
  
  # Predefine shape matrix, add an extra row for tstep 0 where relative error = 1.
  # First column is shape1 (alpha), second column is shape2 (beta)
  gshape <- matrix(0, nrow = tsteps+1, ncol = 2)
  gshape[1, ] <- c(1, 0)
  gshape <- as.data.frame(gshape)
  names(gshape) <- c("shape1", "shape2")
  
  # Copy gshape for oshape
  oshape <- gshape
  
  # Get fitted coefficients
  for (i in 1:tsteps) {
    
    # For each year
    gshape[i+1,] <- coef(fitdist(egas$r[egas$year == i], "beta"))
    oshape[i+1,] <- coef(fitdist(eoil$r[eoil$year == i], "beta"))
  }
  
  # Add year
  gshape$year <- 0:(tsteps)
  oshape$year <- 0:(tsteps)
  
  
  # Generate error matrices -------------------------------------------------
  
  # Predefine matrices
  EgasFrac <- matrix(0, nrow = length(xq), ncol = 12 * tsteps)
  EoilFrac <- EgasFrac
  
  # For each annual time step
  for (i in 1:tsteps) {
    
    # Get beta distribution quantiles
    EgasFrac[, i * 12] <- qbeta(p = xq, shape1 = gshape$shape1[i + 1], shape2 = gshape$shape2[i + 1])
    EoilFrac[, i * 12] <- qbeta(p = xq, shape1 = oshape$shape1[i + 1], shape2 = oshape$shape2[i + 1])
  }
  
  
  # Interpolate to monthly basis --------------------------------------------
  
  # For each quantile
  for (i in 1:nrow(EgasFrac)) {
    
    # Values
    EgasFrac[i, 1:12] <- approx(x = c(0, 12), y = c(1, EgasFrac[i, 12]), n = 13)$y[2:13]
    EoilFrac[i, 1:12] <- approx(x = c(0, 12), y = c(1, EoilFrac[i, 12]), n = 13)$y[2:13]
  }
  
  # For each year
  for (j in 2:tsteps) {
    
    # For each quantile
    for (i in 1:nrow(EgasFrac)) {
      
      # Values
      EgasFrac[i, (12*j-11):(12*j)] <- approx(x = c(12*(j-1), 12*j), y = c(EgasFrac[i, 12*(j-1)], EgasFrac[i, 12*j]), n = 13)$y[2:13]
      EoilFrac[i, (12*j-11):(12*j)] <- approx(x = c(12*(j-1), 12*j), y = c(EoilFrac[i, 12*(j-1)], EoilFrac[i, 12*j]), n = 13)$y[2:13]
    }
  }
  
  
  # Save results ------------------------------------------------------------
  
  # Save
  save(file = file.path(path$data, paste("EIAerrorFrac_", ver, ".rda", sep = "")),
       list = c("EoilFrac",
                "EgasFrac"))
}
