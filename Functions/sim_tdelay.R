# Function Info -----------------------------------------------------------
# Name:      sim_tdelay.R (Simulated Time Delay Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# times - number of wells for which to generate DCC

# field - vector of field numbers to be analyzed individually

# fieldnum - vector of field numbers indicating what field each well is in

# DCA.cdf.coef.oil - CDFs for each coefficient in hyperbolic decline curve eq.
# for each field for oil production, used here for CDFs on time delays

# DCA.cdf.coef.gas - same as above but for gas


# Outputs -----------------------------------------------------------------

# DCC - data frame time delay for each well for both oil and gas production


# Description -------------------------------------------------------------

# This function randomly picks the time delay for each well based on 
# the CDFs contained in DCA.cdf.coef.oil/gas


# Function ----------------------------------------------------------------

sim_tdelay <- function(times, field, fieldnum, DCA.cdf.coef.oil, DCA.cdf.coef.gas) {
  
  # Define time delay vectors
  td <- data.frame(td.oil = rep(0, times),
                   td.gas = rep(0, times))
  
  # For each field
  for (i in 1:length(field)) {
    
    # Get indices of wells located in field i
    ind <- which(fieldnum == field[i])
    
    # Pull coefficient CDFs for field i
    cdf.td.oil <- DCA.cdf.coef.oil[[(i-1)*4+4]]
    cdf.td.gas <- DCA.cdf.coef.gas[[(i-1)*4+4]]
    
    # Pick time delay values
    td$td.oil[ind] <- cdf.td.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.td.oil$CDF), all.inside = T)]
    td$td.gas[ind] <- cdf.td.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.td.gas$CDF), all.inside = T)]
  }
  
  # Round time delay values to nearest month
  td$td.oil <- round(td$td.oil)
  td$td.gas <- round(td$td.gas)
  
  # Return the wellType vector
  return(td)
}