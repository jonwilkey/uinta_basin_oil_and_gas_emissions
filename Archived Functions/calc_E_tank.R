# Function Info -----------------------------------------------------------
# Name:      calc_E_tank.R (Tank emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# heat.duty - heat duty rating of combustor (MMBtu/hr)

# vgas.pilot - volume of gas for combustor pilot (SCF/yr)

# tank.EF - emission factors for tanks (lb/MMCF), note that the combustor
# pilot has several emission factors as well

# wfrac - how much each individual well contributes to emissions at full site

# tank.voc - total tank VOC emissions rate (ton/yr) from eci$tank CPT


# Outputs -----------------------------------------------------------------

# E.tank - Emissions for tanks (in ton/yr)


# Description -------------------------------------------------------------

# This function calculates tank emissions, assuming that tank VOC emissions are
# not a function of oil production rates and do not change over time.


# Function ----------------------------------------------------------------

calc_E_tank <- function(heat.duty, vgas.pilot, tank.EF, wfrac, tank.voc) {
  
  # Calculate combustor and combustor pilot emissions for tank (in ton/yr)
  E.tank <- data.frame(em.tank.voc = (tank.voc                       + vgas.pilot / 1e6 * tank.EF$voc.pilot) / 2000 * wfrac,
                       em.tank.nox = (heat.duty * 8760 * tank.EF$nox + vgas.pilot / 1e6 * tank.EF$nox.pilot) / 2000 * wfrac,
                       em.tank.co =  (heat.duty * 8760 * tank.EF$co  + vgas.pilot / 1e6 * tank.EF$co.pilot)  / 2000 * wfrac)
  
  # Return emissions result
  return(E.tank)
}
