# Function Info -----------------------------------------------------------
# Name:      sim_fieldnum.R (Simulated Field Number Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# cdf.ff - CDFs for probability of a well being located in a given field with
# two columns, first column being list of field numbers and second column being
# the CDF for being located in said field

# times - number of random field numbers to pick


# Outputs -----------------------------------------------------------------

# fieldnum - vector of randomly selected field numbers


# Description -------------------------------------------------------------

# This function randomly picks the field number for a well "times" times based
# on the CDFs for the number of wells lcoated in a given field contained in
# cdf.ff.


# Function ----------------------------------------------------------------

sim_fieldnum <- function(cdf.ff, times) {
  
  # Pick field number
  fieldnum <- cdf.ff[findInterval(runif(times), c(0, cdf.ff[,2])), 1]
  
  # Return the tDrill vector
  return(fieldnum)
}
