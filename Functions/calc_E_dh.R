# Function Info -----------------------------------------------------------
# Name:      calc_E_dh.R (Dehydrator emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# voc.factor - reported VOC emissions factor (lb/hr)

# heat.duty - heat duty rating (MMBtu/hr)

# op - hours of operation per year (hr)

# vgas.pilot - volume of gas for combustor pilot (SCF/yr)

# dh.EF - emission factors for dehydrator (lb/MMCF), note that the combustor
# pilot has several emission factors as well

# wfrac - how much each individual well contributes to emissions at full site


# Outputs -----------------------------------------------------------------

# E.dh - Emissions for separators and heaters (in tons/yr)


# Description -------------------------------------------------------------

# This function calculates dehydrator emissions based the volume of fuel
# combusted and the specified emission factors.


# Function ----------------------------------------------------------------

calc_E_dh <- function(voc.factor, op, vgas.pilot, dh.EF, wfrac, heat.duty) {
  
  # Calculate dehydrator emissions (in tons/yr). Note that the first term in the
  # em.dh.voc calculation is for the VOC emissions from the dehydrator. All
  # other terms are either from the combustor or pilot.
  E.dh <- data.frame(em.dh.voc = (voc.factor * op              + vgas.pilot / 1e6 * dh.EF$voc.pilot) / 2000 * wfrac,
                     em.dh.nox = (heat.duty * 8760 * dh.EF$nox + vgas.pilot / 1e6 * dh.EF$nox.pilot) / 2000 * wfrac,
                     em.dh.co =  (heat.duty * 8760 * dh.EF$co  + vgas.pilot / 1e6 * dh.EF$co.pilot)  / 2000 * wfrac)
  
  # Return emissions result
  return(E.dh)
}
