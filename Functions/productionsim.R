# Function Info -----------------------------------------------------------
# Name:      productionsim.R (Production Simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# wsim - data.table with information about each well

# timesteps - Number of months to be simulated

# production.type - Character switch indicating method for determining prodution
# volumes (simulated or actual)

# decline.type - character string switch for determining what decline/production
# curve equation to use (hyperbolic DC or cumulative production)

# osim.actual - Actual oil production matrix with rows = individual wells and
# columns = timesteps

# gsim.actual - Same as osim.actual but for gas production


# Outputs -----------------------------------------------------------------

# osim/gsim - Matrix with rows = individual wells and columns = timesteps that 
# gives production volume timeseries (of oil/gas) for each well in bbl (for oil)
# or MCF (for gas)


# Description -------------------------------------------------------------

# This function calculates the production volumes of oil or gas (depending on
# well type) for each well in wsim according to [1] the hyperbolic decline curve
# equation or [2] the cumulative production curve:

# [1] q = qo*(1+b*Di*t)^(-1/b)
#
# [2] Q = Cp*t^0.5+c1

# where q is production (in bbl oil or MCF gas), qo is the initial production 
# rate, b is the decline exponent, Di is the initial decline rate, Cp and c1 are
# constants in the cumulative production curve, and t is time.

# If the value of production.type == "a", then the model calculates the 
# production volumes using either Eq.[1] or Eq.[2]. If decline.type == "a", then
# Eq.[1] is used. If decline.type == "b", then Eq.[2] is used. Regardless of 
# which equation is used, the coefficients are taken from the values stored in
# wsim and the calculation is peformed using the apply() function on each row of
# wsim.

# If the value of production.type == "b" then the function call is for a 
# validation run and osim/gsim == osim/gsim.actual.


# Function ---------------------------------------------------------------- 
productionsim <- function(wsim, timesteps, production.type, decline.type,
                          osim.actual, gsim.actual) {
  
  # Switch for production simulation type. Options are "a" for calculating 
  # production from decline curve coefficients in wsim, or "b" for loading the 
  # actual production schedule from DOGM database records processed and saved in
  # scheduleUpdate() function.
  switch(production.type,
         
         # Calculated from decline curve coefficients
         a = {
           
           # Extract required coefficients from wsim. These columns are lated
           # called (in the order specified here) by the function prodfun.
           switch(decline.type,
                  
                  # For hyperbolic decline curve
                  a = {
                    coef.oil <- with(wsim, matrix(c(tDrill, td.oil, qo.oil, b.oil, Di.oil), ncol = 5))
                    coef.gas <- with(wsim, matrix(c(tDrill, td.gas, qo.gas, b.gas, Di.gas), ncol = 5))
                  },
                  
                  # For cumulative production curve
                  b = {
                    coef.oil <- with(wsim, matrix(c(tDrill, td.oil, Cp.oil, c1.oil), ncol = 4))
                    coef.gas <- with(wsim, matrix(c(tDrill, td.gas, Cp.gas, c1.gas), ncol = 4))
                  })
           
           # Function for calculating production schedule for individual well
           # using Eq.[1]
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
               
               # Calculate production in each month by Eq.[1]
               Q <- x[3]*(1+x[4]*x[5]*tvec)^(-1/x[4])
             }
             
             # Return concatonated result
             return(c(TD, Q))
           }
           
           # Function for calculating production schedule for individual well
           # using Eq.[2]
           Qprodfun <- function(x) {
             
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
               
               # Calculate cumulative production in each month by Eq.[2]
               Q <- x[3]*sqrt(tvec)+x[4]
               
               # Production in first timestep is simply Q[1], after
               # first timestep production is differnce between the
               # each timestep as calculated by diff()
               Q <- c(Q[1], diff(Q))
             }
             
             # Return concatonated result
             return(c(TD, Q))
           }
           
           # Apply prodfun on each row of coefficient matrices to calculate oil
           # and gas production schedule
           switch(decline.type,
                  a = {
                    osim <- t(apply(coef.oil, MARGIN = 1, FUN = prodfun))
                    gsim <- t(apply(coef.gas, MARGIN = 1, FUN = prodfun))
                  },
                  
                  b = {
                    osim <- t(apply(coef.oil, MARGIN = 1, FUN = Qprodfun))
                    gsim <- t(apply(coef.gas, MARGIN = 1, FUN = Qprodfun))
                  })
         },
         
         # Actual production schedule from DOGM database
         b = {
           
           # osim/gsim are just osim.actual and gsim.actual
           osim <- osim.actual
           gsim <- gsim.actual
         })
  
  # Check for and overwrite any negative production values
  osim <- ifelse(test = osim < 0 , yes = 0, no = osim)
  gsim <- ifelse(test = gsim < 0 , yes = 0, no = gsim)
  
  # Return results
  return(list(osim, gsim))
}