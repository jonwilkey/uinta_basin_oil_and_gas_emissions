# Function Info -----------------------------------------------------------
# Name:      calc_E_ppump.R (Pneumatic pumps emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# op - average annual hours of operation per pump (?)

# vent.rate - average vent rate for pump(s) (SCF/min)

# MW - gas molecular weight (lb/lbmol)

# m.voc - VOC weight fraction

# wfrac - how much each individual well contributes to emissions at full site


# Outputs -----------------------------------------------------------------

# VOC - VOC emissions total from pneumatic pumps


# Description -------------------------------------------------------------

# This function calculates pneumatic pump emissions


# Function ----------------------------------------------------------------

calc_E_ppump <- function(vent.rate, MW, op, m.voc, wfrac) {
  
  # Calculate emissions
  VOC <- vent.rate * 60 / 379 * MW * op * m.voc / 2000 * wfrac
  
  # Return emissions result
  return(VOC)
}
