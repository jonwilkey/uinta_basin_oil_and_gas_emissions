# Function Info -----------------------------------------------------------
# Name:      drillsim.R (Drilling schedule calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# GBMsim.OP/GP - energy price paths for oil/gas from GBMsim.R function

# nrun - number of iterations in overall simulation

# drilled.init - Number of dry/gas/oil wells drilled in timestep prior to start
# of simulation period

# drillModel - list of lm() fit objects for each drilling model

# type - character string for switch indicating "sim" to use simulated schedule
# or "actual" for actual drilling schedule

# p - DOGM database data frame

# tstart - start date of simulation period

# tstop - stop date of simulation period

# simtype - character switch indicating which model to choose for the simulated
# drilling schedule

# op/gp.init - initial oil/gas price ($/bbl or $/MCF) in time step immediately
# prior to start of simulation period


# Outputs -----------------------------------------------------------------

# Drilled - matrix with rows = MC simulation runs in model and columns =
# timesteps


# Description -------------------------------------------------------------

# This function calculates the number of wells drilled in response to energy
# prices.


# Function ---------------------------------------------------------------- 
drillsim <- function(path, GBMsim.OP, GBMsim.GP, nrun, drilled.init, drillModel,
                     type, p, tstart, tstop, simtype, op.init, gp.init) {
  
  # Check - simulate drilling schedule or use actual drilling schedule
  switch(type,
         
         # If simulating drilling schedule is used
         sim = {
           
           # Internal functions -------------------------------------------
           
           # Prior well model
           PWM <- function(OP, GP, par, init) {
             
             # Initial wells drilled
             w <- round(par[1]*OP[1]+par[2]*GP[1]+par[3]*init+par[4])
             
             # If negative, set as zero
             w <- ifelse(w < 0, 0, w)
             
             # For all other timesteps
             for(i in 2:length(OP)) {
               
               # Calculate wells drilled
               w <- c(w, round(par[1]*OP[i]+par[2]*GP[i]+par[3]*w[length(w)]+par[4]))
               
               # Again, if negative set to zero
               w <- ifelse(w < 0, 0, w)
             }
             
             # Return result
             return(w)
           }
           
           # Other models
           EPM <- function(fit, OP, GP) {temp <- round(fit[2]*OP+fit[3]*GP+fit[1]); ifelse(temp > 0, temp, 0)} # Energy price model
           OPM <- function(fit, OP)     {temp <- round(fit[2]*OP+fit[1]); ifelse(temp > 0, temp, 0)}           # Oil price model
           GPM <- function(fit, GP)     {temp <- round(fit[2]*GP+fit[1]); ifelse(temp > 0, temp, 0)}           # Gas price model
           
           
           # Calculate drilling schedule ----------------------------------
           
           # Predefine matrix for drilling schedule results. Rows = simulation
           # runs, columns = timesteps.
           Drilled <- matrix(0, nrow = nrun, ncol = ncol(GBMsim.OP))
           
           # Define time range (i.e. columns) for prior energy prices
           tdr <- 1:(ncol(GBMsim.OP)-1)
           
           switch(simtype,
                  
                  # Prior well model
                  a = {
                    
                    # For each run
                    for (i in 1:nrun) {
                      
                      # Calculate number of wells drilled
                      Drilled[i,] <- PWM(OP =   GBMsim.OP[i,],
                                         GP =   GBMsim.GP[i,],
                                         par =  drillModel$pwm,
                                         init = drilled.init)
                    }
                  },
                  
                  # Energy price model
                  b = {
                    
                    # For each run
                    for (i in 1:nrun) {
                      
                      # Calculate number of wells drilled
                      Drilled[i,] <- EPM(fit = coefficients(drillModel$epm),
                                         OP =  c(op.init, GBMsim.OP[i,tdr]),
                                         GP =  c(gp.init, GBMsim.GP[i,tdr]))
                    }
                  },
                  
                  # Oil price model
                  c = {
                    
                    # For each run
                    for (i in 1:nrun) {
                      
                      # Calculate number of wells drilled
                      Drilled[i,] <- OPM(fit = coefficients(drillModel$opm),
                                         OP =  c(op.init, GBMsim.OP[i,tdr]))
                    }
                  },
                  
                  # Gas price model
                  d = {
                    
                    # For each run
                    for (i in 1:nrun) {
                      
                      # Calculate number of wells drilled
                      Drilled[i,] <- GPM(fit = coefficients(drillModel$gpm),
                                         GP =  c(gp.init, GBMsim.GP[i,tdr]))
                    }
                  })
         },
         
         # If actual schedule is used...
         actual = {
           
           # Actual number of wells drilled as timeseries. sqldf returns result as a
           # data.frame, so brackets extract numerical results as a vector.
           Drilled.act <- subset(p,
                                 subset = ((h_well_type == "OW" |
                                              h_well_type == "GW") &
                                             h_first_prod >= tstart &
                                             h_first_prod <= tstop),
                                 select = c("p_api", "h_first_prod"))
           
           Drilled.act$h_first_prod <- as.Date(as.yearmon(Drilled.act$h_first_prod))
           
           Drilled.act <- sqldf("select count(distinct(p_api))
                     from 'Drilled.act'
                     group by h_first_prod")[[1]]
           
           # Turn into matrix
           Drilled <- matrix(rep(Drilled.act, nrun), nrow = nrun, byrow = T)
         })
  
  # Return result
  return(Drilled)
}