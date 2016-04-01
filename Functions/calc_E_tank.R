# Function Info -----------------------------------------------------------
# Name:      calc_E_tank.R (Tank emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# heat.duty - heat duty rating of combustor (MMBtu/hr)

# vgas.pilot - volume of gas for combustor pilot (SCF/yr)

# tank.EF - emission factors for tanks (lb/MMCF), note that the combustor
# pilot has several emission factors as well


# Outputs -----------------------------------------------------------------

# E.tank - Emissions for tanks (in tons)


# Description -------------------------------------------------------------

# This function calculates tank emissions


# Function ----------------------------------------------------------------

calc_E_tank <- function(heat.duty, vgas.pilot, tank.EF) {
  
  # Calculate combustor and combustor pilot emissions for tank (in tons)
  E.tank <- data.frame(em.tank.voc = (vgas.pilot / 1e6 * tank.EF$voc.pilot) / 2000,
                       em.tank.nox = (heat.duty * 8760 * tank.EF$nox       + vgas.pilot / 1e6 * tank.EF$nox.pilot) / 2000,
                       em.tank.co =  (heat.duty * 8760 * tank.EF$co        + vgas.pilot / 1e6 * tank.EF$co.pilot)  / 2000)
  
  # Need to build something for tank VOC emissions
  # Go with tank emissions being (oil + condensate) * (voc rate)
  
  # Return emissions result
  return(E.tank)
}
