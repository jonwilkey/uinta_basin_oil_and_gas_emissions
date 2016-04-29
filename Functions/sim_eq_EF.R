# Function Info -----------------------------------------------------------
# Name:      sim__eq_EF.R (Simulated equipment-based emission factors row selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# times - number of wells for which to pick emission factors

# eci - list containing the cumlative probability tables (CPT) from the
# emissionUpdate function


# Outputs -----------------------------------------------------------------

# result - data frame of randomly selected CPT rows for each well


# Description -------------------------------------------------------------

# This function randomly picks the rows in each CPT table which will later be 
# used to calculate equipment-based emissions for each well. The row selection 
# is saved here so that calculations for individual wells can be double checked
# later.


# Function ----------------------------------------------------------------

sim_eq_EF <- function(times, eci) {
  
  # Pick rows from each CPT table for each equipment type for each well
  
  wc <-       findInterval(runif(times), c(0, eci$wc$cprob))       # Well Completion
  rework <-   findInterval(runif(times), c(0, eci$wc$cprob))       # Rework Well Completion
  rt <-       findInterval(runif(times), c(0, eci$rt$cprob))       # RICE and Turbines
  sh <-       findInterval(runif(times), c(0, eci$sh$cprob))       # Separators and Heaters
  dh <-       findInterval(runif(times), c(0, eci$dh$cprob))       # Dehydrators
  tank <-     findInterval(runif(times), c(0, eci$tank$cprob))     # Tanks
  truck <-    findInterval(runif(times), c(0, eci$truck$cprob))    # Trucks
  pctrl <-    findInterval(runif(times), c(0, eci$pctrl$cprob))    # Pneumatic Controllers
  ppump <-    findInterval(runif(times), c(0, eci$ppump$cprob))    # Pneumatic Pumps
  fug.gas <-  findInterval(runif(times), c(0, eci$fug$gas$cprob))  # Fugitives - gas component
  fug.hoil <- findInterval(runif(times), c(0, eci$fug$hoil$cprob)) # Fugitives - heavy oil component
  fug.loil <- findInterval(runif(times), c(0, eci$fug$loil$cprob)) # Fugitives - light oil component
  fug.woil <- findInterval(runif(times), c(0, eci$fug$woil$cprob)) # Fugitives - water/oil component
  
  # Return result as data.frame
  return(data.frame(eqEF.wc =       wc,
                    eqEF.rework =   rework,
                    eqEF.rt =       rt,
                    eqEF.sh =       sh,
                    eqEF.dh =       dh,
                    eqEF.tank =     tank,
                    eqEF.truck =    truck,
                    eqEF.pctrl =    pctrl,
                    eqEF.ppump =    ppump,
                    eqEF.fug.gas =  fug.gas,
                    eqEF.fug.hoil = fug.hoil,
                    eqEF.fug.loil = fug.loil,
                    eqEF.fug.woil = fug.woil))
}
