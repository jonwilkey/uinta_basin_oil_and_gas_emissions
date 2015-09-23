# Function Info -----------------------------------------------------------
# Name:      LECcalc.R (Lease equipment cost calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wsim - well information data.frame, columns used here are:
# $prior - indicateds if a well is a prior well or not
# $rework - indicates when (and if) a well has been reworked
# $wellType - type of well
# $tDrill - time step in which well was drilled
# $depth - total measured well depth in ft

# LOC.oil/gas.equip - lm fit objects describing LEC as f(oil price, depth) for
# oil wells and f(gas price, depth, gas production rate (MCFD)) for gas wells

# op/gp - oil/gas price

# osim/gsim - oil/gas production matrix


# Outputs -----------------------------------------------------------------

# lec - lease equipment capital cost vector


# Description -------------------------------------------------------------

# This function applies the lease equipment cost fits LOC.oil/gas.equip to find 
# the capital cost for lease equipment for each well based on well type and 
# energy prices. The gas fits directly include gas production rate in the fit 
# equation. Oil fits are all for a well producing up to 200 BPD, and must
# subsequently be scaled to the actual maximum production rate of each well.


# Function ----------------------------------------------------------------

LECcalc <- function(wsim, LOC.oil.equip, LOC.gas.equip, op, gp, osim, gsim) {
  
  # Predefine LEC cost vector
  LEC <- rep(0, nrow(wsim))
  
  # Get indices of new wells (prior == F & rework != NA) for oil (OW) and gas (GW)
  # wells
  ind.ow <- which(wsim$prior == FALSE & !is.na(wsim$rework) & wsim$wellType == "OW")
  ind.gw <- which(wsim$prior == FALSE & !is.na(wsim$rework) & wsim$wellType == "GW")
  
  # Calculate LEC for wells in ind.ow
  LEC[ind.ow] <- predict(object =  LOC.oil.equip,
                         newdata = data.frame(op =    op[wsim$tDrill[ind.ow]],
                                              depth = wsim$depth[ind.ow]))
  
  # Scale by production in first month (base is 200 BPD well)
  LEC[ind.ow] <- LEC[ind.ow]*(apply(osim[ind.ow,], 1, max)/30/200)^0.6
  
  # Calculate LEC for wells in ind.gw
  LEC[ind.gw] <- predict(object =  LOC.gas.equip,
                         newdata = data.frame(gp =      gp[wsim$tDrill[ind.gw]],
                                              depth =   wsim$depth[ind.gw],
                                              gasRate = apply(gsim[ind.gw,], 1, max)/30))
  
  # Return result
  return(LEC)
}
