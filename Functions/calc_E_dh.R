# Function Info -----------------------------------------------------------
# Name:      calc_E_dh.R (Dehydrator emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# heat.duty - heat duty rating (MMBtu/hr)

# op - hours of operation per year (hr)

# vgas.pilot - volume of gas for combustor pilot (SCF/yr)

# dh.EF - emission factors for dehydrator (lb/MMCF), note that the combustor
# pilot has several emission factors as well


# Outputs -----------------------------------------------------------------

# E.dh - Emissions for separators and heaters (in tons)


# Description -------------------------------------------------------------

# This function calculates dehydrator emissions based the volume of fuel
# combusted and the specified emission factors.


# Function ----------------------------------------------------------------

calc_E_dh <- function(heat.duty, op, vgas.pilot, dh.EF) {
  
  # Calculate separator and heater emissions (in tons)
  E.dh <- data.frame(em.dh.voc = (vgas.pilot / 1e6 * dh.EF$voc.pilot + dh.EF$voc * op)/2000,
                     em.dh.nox = (heat.duty * 8760 * dh.EF$nox       + vgas.pilot / 1e6 * dh.EF$nox.pilot) / 2000,
                     em.dh.co =  (heat.duty * 8760 * dh.EF$co        + vgas.pilot / 1e6 * dh.EF$co.pilot)  / 2000)
  
  # Need to build something to handle range of dehydrator VOCs
  
  # Return emissions result
  return(E.dh)
}
