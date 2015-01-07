# Function Info -----------------------------------------------------------
# Name:      productionsim.R (Production Simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# nrun - Number of overall iterations in simulation

# timesteps - Number of months to be simulated

# production.type - Character switch indicating method for determining prodution
# volumes (simulated or actual)

# ver - File version number (used to differentiate various version of
# preprocessing data files)


# Outputs -----------------------------------------------------------------

# osim/gsim - Matrix with rows = individual wells and columns = timesteps that
# gives production volume timeseries (of oil/gas) for each well


# Description -------------------------------------------------------------

# This function calculates the production volumes of oil or gas (depending on
# well type) for each well in wsim according to the hyperbolic decline curve
# equation:

# q = qo*(1+b*Di*t)^(-1/b)

# where q is prodcution (in bbl oil or MCF gas), qo is the initial production 
# rate, b is the decline exponent, Di is the initial decline rate, and t is time.

# If the value of production.type == "a" then the model calculates the 
# production volumes using the values of a, b, and c stored in wsim. The
# calculation is peformed using the apply() function on each row of wsim.

# If the value of production.type == "a" then the function call is for a 
# validation run and the function loads and returns the actual production 
# histories stored in the prepared data.frame osim.actual/gsim.actual and these 
# matrices are returned. If nrun > 1 then matrices are concatonated "nrun"
# times.


# Function ---------------------------------------------------------------- 
productionsim <- function(path, nrun, timesteps, production.type, ver) {
  
  # Switch for production simulation type. Options are "a" for calculating 
  # production from decline curve coefficients in wsim, or "b" for loading the 
  # actual production schedule from DOGM database records processed and saved in
  # scheduleUpdate() function.
  switch(production.type,
         
         # Calculated from decline curve coefficients
         a = {
           
           # Extract required coefficients from wsim. These columns are lated
           # called (in the order specified here) by the function prodfun.
           coef.oil <- with(wsim, matrix(c(tDrill, td.oil, qo.oil, b.oil, Di.oil), ncol = 5))
           coef.gas <- with(wsim, matrix(c(tDrill, td.gas, qo.gas, b.gas, Di.gas), ncol = 5))
           
           # Function for calculating production schedule for individual well
           prodfun <- function(x) {
             
             # Get number of zeros to include (from months before well was
             # drilled and from delay between well drilling and start of first
             # decline curve)
             TDzeros <- (x[1]-1)+x[2]
             
             # If equal to or greater than timesteps
             if (TDzeros >= timesteps) {
               
               # Then all production values for this well are zero
               TD <- rep(0, times = timesteps)
               
               # And the production volume vector Q is null
               Q <- NULL
               
             } else {
               
               # The number of zero values is equal to TDzeros
               TD <- rep(0, times = TDzeros)
               
               # Create vector of months for which production will be calculated
               tvec <- c(1:(timesteps-TDzeros))
               
               # Calculate production for each time step in tvec
               Q <- x[3]*(1+x[4]*x[5]*tvec)^(-1/x[4])
             }
             
             # Return concatonated result
             return(c(TD, Q))
           }
           
           # Apply prodfun on each row of coefficient matrices to calculate oil
           # and gas production schedule
           osim <- t(apply(coef.oil, MARGIN = 1, FUN = prodfun))
           gsim <- t(apply(coef.gas, MARGIN = 1, FUN = prodfun))
         },
         
         # Actual production schedule from DOGM database
         b = {
           
           # Load actual DOGM data on production from scheduleUpdate() function
           load(file.path(path$data, paste("psim_actual_", ver, ".rda", sep = "")))
           
           # Concatonate actual production matrix "nrun" times
           otemp <- osim.actual
           gtemp <- gsim.actual
           if (nrun > 1) {
             for (i in 1:(nrun-1)) {
               otemp <- rbind(otemp, osim.actual)
               gtemp <- rbind(gtemp, gsim.actual)
             }
           }
           
           # Redefine as osim/gsim
           osim <- otemp
           gsim <- gtemp
         })
  
  # Return results
  return(list(osim, gsim))
}