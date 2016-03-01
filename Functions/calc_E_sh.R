# Function Info -----------------------------------------------------------
# Name:      calc_E_sh.R (Separators and heaters emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# heat.duty - heat duty rating (MMBtu/hr)

# op - hours of operation per year (hr)

# heat.value - heating value for fuel (Btu/SCF)

# sh.EF - emission factors for separators and heaters (lb/MMCF)

# low.nox - T/F indicating whether or not low-NOx burners are used

# nox.red - NOx reduction fraction for using low-NOx burners


# Outputs -----------------------------------------------------------------

# E.sh - Emissions for separators and heaters (in tons)


# Description -------------------------------------------------------------

# This function calculates the separator and heater emissions based the volume
# of fuel combusted and the specified emission factors.


# Function ----------------------------------------------------------------

calc_E_sh <- function(heat.duty, op, heat.value, sh.EF, low.nox, nox.red) {
  
  # Calculate separator and heater emissions (in tons)
  E.sh <- data.frame(em.sh.pm10 = heat.duty * op / heat.value * sh.EF$pm10 / 2000, # CHANGE ME TO MONTHLY BASIS
                     em.sh.pm25 = heat.duty * op / heat.value * sh.EF$pm25 / 2000,
                     em.sh.sox  = heat.duty * op / heat.value * sh.EF$sox  / 2000,
                     em.sh.nox  = heat.duty * op / heat.value * sh.EF$nox  / 2000,
                     em.sh.voc  = heat.duty * op / heat.value * sh.EF$voc  / 2000,
                     em.sh.co   = heat.duty * op / heat.value * sh.EF$co   / 2000)
  
  # If low-NOx burner is in use
  if (low.nox == TRUE) {
    
    # Reduce NOx emissions
    E.sh$em.sh.nox <- E.sh$em.sh.nox * (1 - nox.red)
  }
  
  # Return emissions result
  return(E.sh)
}
