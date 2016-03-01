# Function Info -----------------------------------------------------------
# Name:      calc_E_truck.R (Truck emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# S - S factor for truck loading rack mode of operation

# P.vap - vapor pressure (psia)

# MW - molecular weight (lb/lbmol)

# Temp - temperature in deg. R

# load - quantity of oil loaded (bbl/yr)

# red - control reduction fraction


# Outputs -----------------------------------------------------------------

# E.truck - Emissions for truck loading (in tons)


# Description -------------------------------------------------------------

# This function calculates truck loading emissions


# Function ----------------------------------------------------------------

calc_E_truck <- function(S, P.vap, MW, Temp, load, red) {
  
  # Calculate VOC emissions factor
  ef <- 12.46 * S * P.vap * MW / Temp
  
  # Calculate VOC emissions (ton/yr)
  VOC <- load * 42 / 1e3 * ef * (1 - red) / 2000
  
  # Return emissions result
  return(E.truck)
}
