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

# ver - File version


# Outputs -----------------------------------------------------------------

# Drilled - matrix with rows = timesteps in model and columns = MC simulation
# runs


# Description -------------------------------------------------------------

# This function calculates the number of wells drilled in response to energy
# prices.


# Function ---------------------------------------------------------------- 
drillsim <- function(path, GBMsim.OP, GBMsim.GP, nrun, drilled.init, ver) {
  
  # Internal function for drilling schedule model ---------------------------
  
  # Function for determining number of wells W drilled in current time step
  # based on model:
  
  # W = a * OP + b * GP + c * Wo + d
  
  #where OP is oil price, GP is gas price, Wo is wells drilled in prior time
  #step, and a, b, c, and d are fitted coefficients.
  
  drillsched <- function(OP,GP,Wo, a, b, c, d) {
    W = a*OP+b*GP+c*Wo+d
    return(W)
  }
  
  
  # Load drilling fit data --------------------------------------------------
  
  # Drilling lm() model
  load(file.path(path$data, paste("drillModel_", ver, ".rda", sep = "")))
  
  
  # Calculate drilling schedule ---------------------------------------------
  
  # Predefine matrix for drilling schedule results. Rows = timesteps,
  # columns = simulation runs.
  Drilled <- matrix(0, nrow = nrow(GBMsim.OP), ncol = nrun)
  
  # Set initial "prior" drilling value (number of wells drilled in
  # timestep immediately proceeding start of simulation period)
  Drilled[1,] <- drilled.init
  
  # For each timestep in simulation period, calculate wells drilled
  for (i in 1:(nrow(Drilled)-1)) {
    Drilled[(i+1),] <- round(drillsched(OP = GBMsim.OP[(i+1),],
                                        GP = GBMsim.GP[(i+1),],
                                        Wo = Drilled[i,],
                                        a = drillModel$coefficients["OP"],
                                        b = drillModel$coefficients["GP"],
                                        c = drillModel$coefficients["prior"],
                                        d = drillModel$coefficients["(Intercept)"]))
  }
  
  # Drop first row (prior time period)
  Drilled <- Drilled[-1,]
  
  # Return result
  return(Drilled)
}