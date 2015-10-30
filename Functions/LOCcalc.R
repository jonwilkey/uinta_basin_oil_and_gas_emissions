# Function Info -----------------------------------------------------------
# Name:      LOCcalc.R (Lease operating costs calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wsim - well information data.frame, columns used here are:
# $wellType - type of well
# $depth - total measured well depth in ft

# LOC.oil/gas.op - lm fit objects describing LOC as f(oil price, depth) for
# oil wells and f(gas price, depth, gas production rate (MCFD)) for gas wells

# op/gp - oil/gas price

# osim/gsim - oil/gas production matrix


# Outputs -----------------------------------------------------------------

# LOC - lease operating cost matrix


# Description -------------------------------------------------------------

# This function applies the lease equipment cost fits LOC.oil/gas.op to find the
# operating cost for each well based on well type and energy prices. The gas
# fits directly include gas production rate in the fit equation. Oil fits are
# all for a well producing 100 BPD, and must subsequently be scaled to the
# actual production rate of each well.


# Function ----------------------------------------------------------------

LOCcalc <- function(wsim, LOC.oil.op, LOC.gas.op, op, gp, osim, gsim) {
  
  # Predefine LEC cost vector
  LOC <- matrix(0, nrow = nrow(wsim), ncol = ncol(osim))
  
  # Get indices of oil and gas wells
  ind.ow <- which(wsim$wellType == "OW")
  ind.gw <- which(wsim$wellType == "GW")
  
  # For each time step
  for (i in 1:length(op)) {
    
    # Calculate LOC for wells in ind.ow
    LOC[ind.ow,i] <- predict(object =  LOC.oil.op,
                             newdata = data.frame(op =    op[i],
                                                  depth = wsim$depth[ind.ow]))
    
    # Calculate LOC for wells in ind.gw
    LOC[ind.gw,i] <- ifelse(test = gsim[ind.gw,i] == 0,
                            yes = 0,
                            no = predict(object =  LOC.gas.op,
                                         newdata = data.frame(gp =      gp[i],
                                                              depth =   wsim$depth[ind.gw],
                                                              gasRate = gsim[ind.gw,i]/30)))
    
  }
  
  # Scale by production in first month (base is 100 BPD well operating cost per month)
  LOC[ind.ow,] <- LOC[ind.ow,]*(osim[ind.ow,]/(100*30))
  
  # Return result
  return(LOC)
}
