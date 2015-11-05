# Function Info -----------------------------------------------------------
# Name:      sim_rework.R (Simulated Well Rework Time Step Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# type - character string used as switch for adjusting to timescale used in 
# simulation, valid options are "new" for new wells, "prior" for prior wells,
# and "none" for no reworks

# wellType - oil or gas (assuming dry wells are gas wells)

# tDrill - vector of timesteps at which a a well was drilled (new wells only)

# td.oil/td.gas - vector of time delays for oil or gas production (new wells
# only)

# cdf.rework - data frame with CDFs for well reworks as function of time for
# both oil and gas wells

# timesteps - number of simulated time steps

# firstprod - date vector for dates of first production (prior wells only)

# tstart - simulation start date (prior wells only)


# Outputs -----------------------------------------------------------------

# rework - vector of time steps at which well will be reworked


# Description -------------------------------------------------------------

# This function randomly picks the time step at which a well will be reworked.
# If the value returned is zero, the well is never reworked.


# Function ----------------------------------------------------------------

sim_rework <- function(type, wellType, tDrill, td.oil, td.gas, cdf.rework,
                       timesteps, firstprod, tstart) {
  
  # Predefine results vector
  rework <- rep(0, times = length(wellType))
  
  # Find interval of rework time step based on well type
  rework[wellType == "OW"] <- findInterval(runif(length(which(wellType == "OW"))),
                                           c(0, cdf.rework$oil, 1))
  rework[which(wellType == "GW" |
                 wellType == "D")] <- findInterval(runif(length(which(wellType == "GW" |
                                                                        wellType == "D"))),
                                                   c(0, cdf.rework$gas, 1))
  
  # Replace intervals with time step values. Any interval value that is
  # length(wellType)+1 means that well is never reworked, leave as 0
  rework <- c(cdf.rework$month, 0)[rework]
  
  # Check - which type of wells are we calculating reworks for?
  switch(type,
         
         # For new wells
         new = {
           
           # Get row index of reworked wells
           ind <- which(rework > 0)
           
           # Since rework time steps are relative to how long well has been in operation,
           # move to absolute scale of simulation by adding rework to tDrill and
           # td.oil/td.gas
           rework[ind] <- (rework[ind]+
                           tDrill[ind]+
                           apply(data.frame(td.oil, td.gas)[ind,], 1, max))
         },
         
         # For prior wells
         prior = {
           
           # Using firstprod, calculate relative time difference between tstart
           # (start of simulation period) and firstprod (date when well was
           # first came online)
           trel <- round(as.numeric(difftime(firstprod, tstart, units = "days"))*(12/365.25))
           
           # Any well with a postive time difference value was drilled after the
           # start of the simulation period, and is therefore not a "prior"
           # well. Zero these wells out
           rework[trel >= 0] <- 0
           
           # All other prior wells are just the value of rework + trel on time
           # scale of simulation
           rework <- rework+trel
         },
         
         # No reworks
         none = {
           
           # Set everything back to zero
           rework <- rep(0, times = length(wellType))
         })
  
  # Finally, since any well reworked before/after the simulation period is
  # effectively not reworked, rewrite as 0
  rework[which(rework > timesteps | rework < 0)] <- 0
  
  # Return the rework vector
  return(rework)
}