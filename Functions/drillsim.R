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

# drillModel - lm() fit object giving drilling rate as f(energy prices, previous
# # of wells drilled)

# type - character string for switch indicating "sim" to use simulated schedule
# or "actual" for actual drilling schedule

# p - DOGM database data frame

# tstart - start date of simulation period

# tstop - stop date of simulation period

# simtype - character switch indicating which model to choose for the simulated
# drilling schedule. Valid options are "global", for the full data fit in
# drillModel, or "window" for a randomly selected time window fit contained in
# dmWindow.

# dmWindow - data frame with rows containing fits of the drilling
# schedule model using a rolling time window

# drilledInitType - character switch for specifying which method to use for
# picking the # of wells drilled initially in each MC iteration

# diffWell - CDF for change in number of wells from one time step to the next


# Outputs -----------------------------------------------------------------

# Drilled - matrix with rows = MC simulation runs in model and columns =
# timesteps


# Description -------------------------------------------------------------

# This function calculates the number of wells drilled in response to energy
# prices.


# Function ---------------------------------------------------------------- 
drillsim <- function(path, GBMsim.OP, GBMsim.GP, nrun, drilled.init, drillModel,
                     type, p, tstart, tstop, simtype, dmWindow, drilledInitType,
                     diffWell) {
  
  # Check - simulate drilling schedule or use actual drilling schedule
  switch(type,
         
         # If simulating drilling schedule is used
         sim = {
           
           # Internal function for drilling schedule model ----------------
           
           # Function for determining number of wells W drilled in current time
           # step based on model:
           
           # W = a * OP + b * GP + c * Wo + d
           
           #where OP is oil price, GP is gas price, Wo is wells drilled in prior
           #time step, and a, b, c, and d are fitted coefficients.
           
           drillsched <- function(OP,GP,Wo, a, b, c, d) {
             
             # Function
             W <- a*OP+b*GP+c*Wo+d
             
             return(W)
           }
           
           
           # Get drilling model coefficients ------------------------------
           
           # Predefine drill model coefficient (DMC) matrix
           DMC <- matrix(0, nrow = nrun, ncol = 4)
           
           switch(simtype,
                  
                  # If using single global fit
                  global = {
                    
                    # Extract coefficients from drillModel lm() object
                    DMC[,1] <- drillModel$coefficients["OP"]
                    DMC[,2] <- drillModel$coefficients["GP"]
                    DMC[,3] <- drillModel$coefficients["prior"]
                    DMC[,4] <- drillModel$coefficients["(Intercept)"]
                  },
                  
                  # If using randomly selected window fit
                  window = {
                    
                    # Pick random rows from dmWindow
                    DMC <- dmWindow[round(runif(nrun, 1, nrow(dmWindow))),1:4]
                  })
           
           # Rename and convert to data.frame
           DMC <- data.frame(a = DMC[,1], b = DMC[,2], c = DMC[,3], d = DMC[,4])
           
           
           # Calculate drilling schedule ----------------------------------
           
           # Predefine matrix for drilling schedule results. Rows = simulation
           # runs, columns = timesteps.
           Drilled <- matrix(0, nrow = nrun, ncol = (ncol(GBMsim.OP)+1))
           
           # Set initial "prior" drilling value (number of wells drilled in
           # timestep immediately proceeding start of simulation period)
           switch(drilledInitType,
                  
                  # If the # of wells drilled initially is always the value specified
                  # in drilled.init
                  a = {Drilled[,1] <- drilled.init},
                  
                  # If the # of wells drilled initially is picked from well prior CDF
                  b = {
                    
                    # Perform random draw
                    Drilled[,1] <- drilled.init+round(diffWell$PDF.x[findInterval(runif(nrow(Drilled)),
                                                                                  c(0,diffWell$CDF),
                                                                                  all.inside = T)])
                    
                    # Correct for any negative #'s by overwriting as zeroes
                    Drilled[,1] <- ifelse(test = Drilled[,1] < 0,
                                          yes =  0,
                                          no =   Drilled[,1])
                  })
           
           # For each timestep in simulation period, calculate wells drilled
           for (i in 1:ncol(GBMsim.OP)) {
             Drilled[,(i+1)] <- round(drillsched(OP = GBMsim.OP[,i],
                                                 GP = GBMsim.GP[,i],
                                                 Wo = Drilled[,i],
                                                 a = DMC$a,
                                                 b = DMC$b,
                                                 c = DMC$c,
                                                 d = DMC$d))
             
             # Check - has drilling schedule gone negative? If so, overwrite as 0
             Drilled[,(i+1)] <- ifelse(test = Drilled[,(i+1)] < 0,
                                       yes =  0,
                                       no =   Drilled[,(i+1)])
           }
           
           # Drop first column (# of wells drilled in prior time period)
           Drilled <- Drilled[,-1]
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