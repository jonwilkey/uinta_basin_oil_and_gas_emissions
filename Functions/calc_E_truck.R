# Function Info -----------------------------------------------------------
# Name:      calc_E_truck.R (Truck emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# S - S factor for truck loading rack mode of operation

# P.vap - vapor pressure (psia)

# MW - molecular weight (lb/lbmol)

# Temp - temperature in deg. R

# red - control reduction fraction


# Outputs -----------------------------------------------------------------

# ef - Emissions factor for truck loading (ton VOC/bbl)


# Description -------------------------------------------------------------

# This function calculates the truck loading VOC emissions factor


# Function ----------------------------------------------------------------

calc_E_truck <- function(S, P.vap, MW, Temp, red) {
  
  # Calculate VOC emissions factor (ton VOC/bbl)
  ef <- (12.46 * S * P.vap * MW / Temp) * (1 - red) * (42 / 1e3 / 2000)
  
  # Return emissions result
  return(ef)
}
