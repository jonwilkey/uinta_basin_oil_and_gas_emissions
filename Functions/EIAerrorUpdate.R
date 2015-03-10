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

EIAerrorUpdate <- function(path, xq, tsteps, ver) {
  
  # Load EIA error *.csv files ----------------------------------------------
  
  # Read in source data from *.csv files. The *.csv files should be structured as
  # follows: columns = AEO reports, rows = time steps, values = % error between
  # actual price and predicted price. Lower right of corner of export matrix
  # should all be NA.
  egas <- read.csv(file.path(path$raw, "EIA_gp_error_export.csv"), header = FALSE)
  eoil <- read.csv(file.path(path$raw, "EIA_op_error_export.csv"), header = FALSE)
  
  
  # Expand errors from annual to monthly basis ------------------------------
  
  # Define monthly error matrices
  moil <- matrix(0, nrow = nrow(eoil), ncol = 12*ncol(eoil))
  mgas <- moil
  
  # For each set of AEO forecast error observations (i.e. rows) in eoil/egas
  for (i in 1:nrow(eoil)) {
    
    # Get sequence of error observations. Zero is concatonated in front of first
    # error observation because its assumed that zero error occurs in forecast
    # at start of time period.
    obs.oil <- c(0, eoil[i,])
    obs.gas <- c(0, egas[i,])
    
    # Define sequence of months into future from start of forecast (t = 0) to
    # end of AEO forecast error observation period
    m <- seq(from = 0, to = 12*(length(obs.oil)-1), by = 12)
    
    # Define temporary results vector
    roil <- NULL
    rgas <- roil
    
    # For each annual observation, interpolate using approx() function
    for (j in 1:(length(obs.oil)-1)) {
      
      # Check: is error observation at end of time period NA? If yes, skip.
      if (!is.na(obs.oil[j+1])) {
        
        # Call approx()
        int.oil <- approx(x = m[j:(j+1)], y = obs.oil[j:(j+1)], n = 13)$y
        int.gas <- approx(x = m[j:(j+1)], y = obs.gas[j:(j+1)], n = 13)$y
        
        # Write out interpolation results to temporary vector
        roil <- c(roil, int.oil[2:13])
        rgas <- c(rgas, int.gas[2:13])
      } else {
        
        # Fill in the rest of the monthly observation periods with NAs
        roil <- c(roil, rep(NA, times = (ncol(moil)-length(roil))))
        rgas <- c(rgas, rep(NA, times = (ncol(moil)-length(rgas))))
      }
    }
    
    # Write out result to monthly error matrix
    moil[i,] <- roil
    mgas[i,] <- rgas
  }
  
  # Finally, rename as eoil/egas
  eoil <- moil
  egas <- mgas
  
  
  # Get CDF for each prediction year ----------------------------------------
  
  # Assume that % relative error in each year is drawn from a normal distribution 
  # with mean and sd given equal to that of the sample set in each year. We can
  # then use qnorm() to get CDF
  
  # Preallocate space for results
  Eoil <- matrix(0, nrow = length(xq), ncol = 12*tsteps)
  Egas <- Eoil
  
  # For each year into forecast, determine CDF
  for (i in 1:(12*tsteps)){
    
    # Subset data to select only current year, omit any NAs
    otemp <- eoil[,i]
    gtemp <- egas[,i]
    otemp <- otemp[which(!is.na(otemp))]
    gtemp <- gtemp[which(!is.na(gtemp))]
    
    # Calculate CDF with qnorm
    Eoil[,i] <- qnorm(p = xq, mean = mean(otemp), sd = sd(otemp))
    Egas[,i] <- qnorm(p = xq, mean = mean(gtemp), sd = sd(gtemp))
  }
  
  
  # Save results ------------------------------------------------------------
  
  save(file = file.path(path$data, paste("EIAerror_", ver, ".rda", sep = "")),
       list = c("Eoil",
                "Egas"))
  
  
  # Plots - uncomment to plot if desired ------------------------------------
  
#   pdf(file.path(path$plot, "EIA error plots.pdf"))
#   
#   # Generate test runs
#   otest <- Eoil[round(runif(10e3, min = 1, max = nrow(Eoil))),]
#   gtest <- Egas[round(runif(10e3, min = 1, max = nrow(Egas))),]
#   
#   # Space for results
#   o.err.q <- matrix(0, nrow = 9, ncol = tsteps)
#   g.err.q <- o.err.q
#   
#   # Get quantiles of results
#   quant <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
#   for (i in 1:tsteps) {
#     o.err.q[,i] <- quantile(otest[,i], probs = quant)
#     g.err.q[,i] <- quantile(gtest[,i], probs = quant)
#   }
#   
#   linecolor <- rainbow(length(quant))
#   
#   # Main plot with largest quantile result for oil
#   plot(o.err.q[1,],
#        type = "l",
#        col = linecolor[1],
#        ylim = c(1.1*min(o.err.q), 1.1*max(o.err.q)),
#        xlab = "Time Steps (years)",
#        ylab = "% Relative Error",
#        main = "% Relative Error EIA Forecast vs Actual Prices for Oil")
#   
#   # All the other quantile lines
#   for (i in 2:length(quant)) {lines(o.err.q[i,], col = linecolor[i])}
#   
#   # Add legend
#   legend("bottomleft", c("90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%"),
#          ncol = 2, col = c(linecolor), lty = 1)
#   
#   # Main plot with largest quantile result for gas
#   plot(g.err.q[1,],
#        type = "l",
#        col = linecolor[1],
#        ylim = c(1.1*min(g.err.q), 1.1*max(g.err.q)),
#        xlab = "Time Steps (years)",
#        ylab = "% Relative Error",
#        main = "% Relative Error EIA Forecast vs Actual Prices for Gas")
#   
#   # All the other quantile lines
#   for (i in 2:length(quant)) {lines(g.err.q[i,], col = linecolor[i])}
#   
#   # Add legend
#   legend("bottomleft", c("90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%"),
#          ncol = 2, col = c(linecolor), lty = 1)
#   
#   dev.off()
}