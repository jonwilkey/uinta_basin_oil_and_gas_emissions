# Function Info -----------------------------------------------------------
# Name:      calc_E_pctrl.R (Pneumatic controllers emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# op - average operating hours per device

# nHB/nIB/nLB - number of high/intermittent/low bleed devices

# pctrl.ef - emission factors for each type of pneumatic control bleed rate

# wfrac - how much each individual well contributes to emissions at full site


# Outputs -----------------------------------------------------------------

# VOC - VOC emissions total from pneumatic controllers


# Description -------------------------------------------------------------

# This function calculates pneumatic controller emissions


# Function ----------------------------------------------------------------

calc_E_pctrl <- function(op, nHB, nIB, nLB, pctrl.ef, wfrac) {
  
  # Calculate VOC emissions
  VOC <- (nHB * pctrl.ef$HB.voc +
          nIB * pctrl.ef$IB.voc +
          nLB * pctrl.ef$LB.voc) * op * wfrac
  
  # Return emissions result
  return(VOC)
}
