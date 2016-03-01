# Function Info -----------------------------------------------------------
# Name:      calc_E_rt.R (RICE & Turbines emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# fuel - quantity of oil used (gallons)

# red - emissions reduction fraction (from flare control)

# wc.EF - emission factors for well completion


# Outputs -----------------------------------------------------------------

# E.rt - Emissions for RICE and turbines (in tons/month)


# Description -------------------------------------------------------------

# This function calculates the total emissions from reciprocating internal 
# combustion engines (RICE) and turbines. Emissions are calculated using either
# (a) AP-42 Chapter 3, (b) NSPS Subpart JJJJ, or (c) a user specified input


# Function ----------------------------------------------------------------

calc_E_rt <- function(heat, type, rt.EF, red, hp, hr) {
  
  # Predefine results data.frame
  result <- matrix(0, nrow = length(type), ncol = ncol(rt.EF))
  
  # Calculate AP-42 emissions
  
  # Get index of engine type numbers for which AP-42 applies
  ind <- which(type <= 9)
  
  # If AP-42 engines exist
  if (length(ind) > 0) {
    
    # For each element in ind
    for (i in ind) {
      
      # Calculate emissions
      result[i, ] <- as.numeric(heat[i] * rt.EF[type[i], ] * (1 - red[i]) / 2000)
    }
  }
  
  # Repeat for NSPS Subpart JJJJ engines
  
  # Get index of engine type numbers for which AP-42 applies
  ind <- which(type > 9)
  
  # If JJJJ engines exist
  if (length(ind) > 0) {
    
    # For each element in ind
    for (i in ind) {
      
      # Calculate emissions
      result[i, ] <- as.numeric((((hp[i] * hr[i] * rt.EF[type[i], ]) / 453.592) * (1 - red[i])) / 2000)
    }
  }
  
  # Reformat result into data.frame
  E.rt <- data.frame(em.rt.pm10 = result[,1],
                     em.rt.pm25 = result[,2],
                     em.rt.sox  = result[,3],
                     em.rt.nox  = result[,4],
                     em.rt.voc  = result[,5],
                     em.rt.co   = result[,6],
                     em.rt.ch2o = result[,7])
  
  # Return emissions result
  return(E.rt)
}
