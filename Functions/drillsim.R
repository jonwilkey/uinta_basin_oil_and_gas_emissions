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


# Outputs -----------------------------------------------------------------

# Drilled - matrix with rows = MC simulation runs in model and columns =
# timesteps


# Description -------------------------------------------------------------

# This function calculates the number of wells drilled in response to energy
# prices.


# Function ---------------------------------------------------------------- 
drillsim <- function(path, GBMsim.OP, GBMsim.GP, nrun, drilled.init, drillModel) {
  
  # Internal function for drilling schedule model ---------------------------
  
  # Function for determining number of wells W drilled in current time step
  # based on model:
  
  # W = a * OP + b * GP + c * Wo + d
  
  #where OP is oil price, GP is gas price, Wo is wells drilled in prior time
  #step, and a, b, c, and d are fitted coefficients.
  
  drillsched <- function(OP,GP,Wo, a, b, c, d) {
    
    # Function - take absolute value to prevent getting negative #s of wells
    # drilled
    W <- abs(a*OP+b*GP+c*Wo+d)
    
    return(W)
  }
  
  
  # Calculate drilling schedule ---------------------------------------------
  
  # Predefine matrix for drilling schedule results. Rows = simulation runs,
  # columns = timesteps.
  Drilled <- matrix(0, nrow = nrun, ncol = ncol(GBMsim.OP))
  
  # Set initial "prior" drilling value (number of wells drilled in
  # timestep immediately proceeding start of simulation period)
  Drilled[,1] <- drilled.init
  
  # For each timestep in simulation period, calculate wells drilled
  for (i in 1:(ncol(Drilled)-1)) {
    Drilled[,(i+1)] <- round(drillsched(OP = GBMsim.OP[,i],
                                        GP = GBMsim.GP[,i],
                                        Wo = Drilled[,i],
                                        a = drillModel$coefficients["OP"],
                                        b = drillModel$coefficients["GP"],
                                        c = drillModel$coefficients["prior"],
                                        d = drillModel$coefficients["(Intercept)"]))
  }
  
  # Drop first column (# of wells drilled in prior time period)
  Drilled <- Drilled[,-1]
  
  # Return result
  return(Drilled)
}