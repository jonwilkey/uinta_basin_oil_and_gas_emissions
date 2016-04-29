# Function Info -----------------------------------------------------------
# Name:      calc_E_wc.R (Well completion emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# fuel - quantity of oil used (gallons)

# red - emissions reduction fraction (from flare control). OGEI database values
# are given as a %, so divide values by 100

# wc.EF - emission factors for well completion


# Outputs -----------------------------------------------------------------

# E.wc - Emissions for well completion (in tons)


# Description -------------------------------------------------------------

# This function calculates the total well completion emissions based on diesel
# fuel usage and control types.


# Function ----------------------------------------------------------------

calc_E_wc <- function(fuel, red, wc.EF) {
  
  # Calculate well completion emissions (in tons)
  E.wc <- data.frame(em.wc.pm10 = fuel * wc.EF$pm10 * (1 - red / 100) / 2000,
                     em.wc.pm25 = fuel * wc.EF$pm25 * (1 - red / 100) / 2000,
                     em.wc.nox  = fuel * wc.EF$nox  * (1 - red / 100) / 2000,
                     em.wc.voc  = fuel * wc.EF$voc  * (1 - red / 100) / 2000,
                     em.wc.co   = fuel * wc.EF$co   * (1 - red / 100) / 2000)
  
  # Return emissions result
  return(E.wc)
}
