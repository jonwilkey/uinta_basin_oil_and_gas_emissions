# Function Info -----------------------------------------------------------
# Name:      calc_E_tank.R (Tank emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# heat.duty - heat duty rating of combustor (MMBtu/hr)

# vgas.pilot - volume of gas for combustor pilot (SCF/yr)

# tank.EF - emission factors for tanks (lb/MMCF), note that the combustor
# pilot has several emission factors as well

# red - Tank VOC reduction fraction from either running combustor or VRU

# osim - oil production matrix

# wfrac - how much each individual well contributes to emissions at full site

# Eid - emissions identity matrix. Values are either 1 (if well is producing oil
# or gas) or 0 (if not producing). Same dimensions as osim.

# ratio - ton VOC / bbl of oil emitted by tank

# max.voc - max value of total_voc from CPT table for tanks


# Outputs -----------------------------------------------------------------

# E.tank - Emissions for tanks (in tons/month)


# Description -------------------------------------------------------------

# This function calculates tank emissions

# Note - Some tanks with very low production volumes have high emissions rates.
# If you allow R to randomly pick the ratio  (emissions / oil) for those tanks,
# its possible that they'll be matched up with large production wells, resulting
# in emissions > 10e3 ton VOC/yr. As a result, any tank with voc emissions
# larger than the highest reported rate in the tank CPT table is replaced with
# that maximum value.


# Function ----------------------------------------------------------------

calc_E_tank <- function(heat.duty, vgas.pilot, tank.EF, osim, wfrac, red, Eid,
                        ratio, max.voc) {
  
  # Calculate combustor and combustor pilot emissions for tank (in tons/month)
  temp <- data.frame(em.tank.voc = (                                 vgas.pilot / 1e6 * tank.EF$voc.pilot) / 2000 / 12 * wfrac,
                     em.tank.nox = (heat.duty * 8760 * tank.EF$nox + vgas.pilot / 1e6 * tank.EF$nox.pilot) / 2000 / 12 * wfrac,
                     em.tank.co =  (heat.duty * 8760 * tank.EF$co  + vgas.pilot / 1e6 * tank.EF$co.pilot)  / 2000 / 12 * wfrac)
  
  # Turn into matrices
  em.tank.voc <- matrix(temp$em.tank.voc, nrow = nrow(osim), ncol = ncol(osim)) * Eid
  em.tank.nox <- matrix(temp$em.tank.nox, nrow = nrow(osim), ncol = ncol(osim)) * Eid
  em.tank.co  <- matrix(temp$em.tank.co,  nrow = nrow(osim), ncol = ncol(osim)) * Eid
  
  # Add VOC emissions from tank
  em.tank.voc <- em.tank.voc + (ratio * osim) * (1 - red)
  
  # Replace any VOC values > max.voc with max.voc
  em.tank.voc[which(em.tank.voc > max.voc)] <- max.voc
  
  # Return emissions result
  return(list(voc = em.tank.voc,
              nox = em.tank.nox,
              co  = em.tank.co))
}
