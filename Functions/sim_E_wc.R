# Function Info -----------------------------------------------------------
# Name:      sim_E_wc.R (Simulated emissions for well completion)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wc.fuel.CDF - CDF describing the probability for diesel fuel usage for
# completing a well

# wc.ctrl.prob - cumulative probability for different completion control (i.e.
# flaring) types

# wc.EF - emission factors for well completion

# times - number of random field numbers to pick


# Outputs -----------------------------------------------------------------

# E.wc - Emissions for well completion


# Description -------------------------------------------------------------

# This function randomly picks the fuel usage for completing a well, as well as 
# whether or not the emissions from completion are controlled (and to what 
# extent). Based on the selected fuel and control types, the total well
# completion emissions are calculated.


# Function ----------------------------------------------------------------

sim_E_wc <- function(wc.fuel.CDF, wc.ctrl.prob, wc.EF, times) {
  
  # Pick diesel fuel usage (gal)
  fuel <- wc.fuel.CDF$PDF.x[findInterval(runif(times), c(0, wc.fuel.CDF$CDF))]
  
  # Pick flaring control type (i.e. reduction efficiency)
  red <- wc.ctrl.prob$red[findInterval(runif(times), c(0, wc.ctrl.prob$cprob))]
  
  # Calculate completion emissions
  E.wc <- data.frame(em.pm10 = fuel * wc.EF$EF.pm10 * (1 - red) / 2000,
                     em.pm25 = fuel * wc.EF$EF.pm25 * (1 - red) / 2000,
                     em.nox  = fuel * wc.EF$EF.nox  * (1 - red) / 2000,
                     em.voc  = fuel * wc.EF$EF.voc  * (1 - red) / 2000,
                     em.co   = fuel * wc.EF$EF.co   * (1 - red) / 2000)
  
  # Return the tDrill vector
  return(E.wc)
}
