# Function Info -----------------------------------------------------------
# Name:      calTstep.R (Calendar date to model time step converter)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# cal - calendar date object in YYYY-MM-DD format

# tstart - start date of Monte-Carlo simulation (also in YYYY-MM-DD format), set
# by default to opt$tstart


# Outputs -----------------------------------------------------------------

# time step (relative to model)


# Description -------------------------------------------------------------

# This function translates calendar dates into the equivalent model time step


# Function ----------------------------------------------------------------

calTstep <- function (cal, tstart = opt$tstart) {
  
  # Calculate monthly time step based on time difference
  1+round(as.numeric(difftime(cal, tstart, units = "days"))*(12/365.25))
}