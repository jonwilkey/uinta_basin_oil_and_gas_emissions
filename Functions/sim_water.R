# Function Info -----------------------------------------------------------
# Name:      sim_water.R (Simulated Water Factor Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wellType - character vector of well types (OW, GW)

# cdf.water - data frame of CDFs for various water related activities (produced
# water, evaporation, fracking, etc.)


# Outputs -----------------------------------------------------------------

# result - data frame of water factors


# Description -------------------------------------------------------------

# This function randomly picks water factors for use in the water balance
# calculation according to the CDFs in the cdf.water data.frame


# Function ----------------------------------------------------------------
sim_water <- function(wellType, cdf.water) {
  
  # Pick produced water ratio (vol produced water / vol oil/gas) based on well
  # wellType
  pw <- ifelse(test = (wellType == "OW"),
               yes =  cdf.water$pw.oil[findInterval(runif(length(wellType)),
                                                    c(0, cdf.water$cdf), all.inside = T)],
               no =   cdf.water$pw.gas[findInterval(runif(length(wellType)),
                                                    c(0, cdf.water$cdf), all.inside = T)])
  
  # Pick disposal water ratio (vol disposal / vol produced)
  disp <- cdf.water$disp[findInterval(runif(length(wellType)),
                                      c(0, cdf.water$cdf), all.inside = T)]
  
  # Pick fraction of evaporation water to produced water
  evap <- cdf.water$evap[findInterval(runif(length(wellType)),
                                      c(0, cdf.water$cdf), all.inside = T)]
  
  # Pick amount of water used for fracking each well (bbl water)
  frack <- ifelse(test = wellType == "OW",
                  yes = cdf.water$fw.ow[findInterval(runif(length(wellType)),
                                                     c(0, cdf.water$cdf), all.inside = T)],
                  no =  cdf.water$fw.gw[findInterval(runif(length(wellType)),
                                                     c(0, cdf.water$cdf), all.inside = T)])
  
  # Pick water injection for water flooding ratio (vol water injected / vol oil produced)
  inj <- cdf.water$inj[findInterval(runif(length(wellType)),
                                    c(0, cdf.water$cdf), all.inside = T)]
  
  # Create results data.frame
  result <- data.frame(pw =    pw,
                       disp =  disp,
                       evap =  evap,
                       frack = frack,
                       inj =   inj)
  
  # Return the results
  return(result)
}