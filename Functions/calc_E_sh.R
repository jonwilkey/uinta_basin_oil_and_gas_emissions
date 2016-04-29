# Function Info -----------------------------------------------------------
# Name:      calc_E_sh.R (Separators and heaters emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# heat.duty - heat duty rating (MMBtu/hr)

# op - hours of operation per year (hr)

# heat.value - heating value for fuel (Btu/SCF)

# sh.EF - emission factors for separators and heaters (lb/MMCF)

# nox.red - NOx reduction fraction for using low-NOx burners

# wfrac - how much each individual well contributes to emissions at full site


# Outputs -----------------------------------------------------------------

# E.sh - Emissions for separators and heaters (in tons/yr)


# Description -------------------------------------------------------------

# This function calculates the separator and heater emissions based the volume
# of fuel combusted and the specified emission factors.


# Function ----------------------------------------------------------------

calc_E_sh <- function(heat.duty, op, heat.value, sh.EF, nox.red, wfrac) {
  
  # Calculate separator and heater emissions (in tons/yr)
  E.sh <- data.frame(em.sh.pm10 = heat.duty * op / heat.value * sh.EF$pm10 / 2000 * wfrac,
                     em.sh.pm25 = heat.duty * op / heat.value * sh.EF$pm25 / 2000 * wfrac,
                     em.sh.sox  = heat.duty * op / heat.value * sh.EF$sox  / 2000 * wfrac,
                     em.sh.nox  = heat.duty * op / heat.value * sh.EF$nox * (1 - nox.red) / 2000 * wfrac,
                     em.sh.voc  = heat.duty * op / heat.value * sh.EF$voc  / 2000 * wfrac,
                     em.sh.co   = heat.duty * op / heat.value * sh.EF$co   / 2000 * wfrac)
  
  # Return emissions result, omitting any NAs
  return(E.sh)
}
