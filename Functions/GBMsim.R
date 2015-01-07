# Function Info -----------------------------------------------------------
# Name:      GBMsim.R (Geometric Brownian Motion Price Path Simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - path names for file directoires (data, plotting, etc.)

# oil.fpp.init - National first purchase price (FPP) for oil in opt$cpi $ per
# bbl

# gas.fpp.init - National first purchase price (FPP) for gas in opt$cpi $ per
# MMBtu

# timesteps - Number of months to be simulated

# nrun - number of iterations in overall simulation

# ver - File version


# Outputs -----------------------------------------------------------------

# GBMsim.OP/GP - data.frame containing simulated energy price paths where rows =
# timesteps and columns = simulation runs


# Description -------------------------------------------------------------

# This function simulates oil and gas price paths by using Geometric Brownian 
# Motion (GBM) to simulate oil and gas price paths. Given N number of GBM paths
# each of length T for given drift (mu) and squared volatility (v) parameters
# and initial price p.init. Returns a matrix whose columns hold the simulated
# paths. See (see Pindyck and Dixit p. 72).


# Function ----------------------------------------------------------------
GBMsim <- function(path, oil.fpp.init, gas.fpp.init, timesteps, nrun, ver) {
  
  # Internal Function -----------------------------------------------------
  
  # --- Inputs ---
  # p.init - Initial price
  # mu - Drift
  # v - Volatility
  # timesteps - number of timesteps in each path
  # N - number of GBM paths to generate
  
  sim <- function(p.init, mu, v, timesteps, N) {
    
    # Predefine space for results
    sim.data <- matrix(nrow=timesteps+1, ncol=N)
    
    # Assign initial price to first row of results matrix
    sim.data[1,] <- rep(p.init, ncol(sim.data))
    
    # For each N price path
    for (j in 1:ncol(sim.data)) {
      
      # For each timestep
      for (i in 2:nrow(sim.data)) {
        
        # Calculate price using GBM function
        sim.data[i,j] <- (1+mu)*sim.data[i-1,j]+sqrt(v)*sim.data[i-1,j]*rnorm(1,0,1)
      }
    }
    return(sim.data)
  }
  
  # Load required files ---------------------------------------------------
  
  # GBM fitted parameters "v" and "mu" for oil price (OP) and gas price
  # (GP) (GBMfitGP and GBMfitOP)
  load(file.path(path$data, paste("GBMfit_", ver, ".rda", sep = "")))
  
  # Generate price paths --------------------------------------------------
  
  # Call for GBMsim for oil prices. Columns are simulations, rows are timesteps.
  # First row is simply p.init price, total number of rows = value given in
  # timesteps + 1. Prices returned are UT crude oil FPP in opt$cpi dollars per
  # bbl.
  GBMsim.OP <- sim(p.init = oil.fpp.init,
                      mu = GBMfitOP$mu,
                      v = GBMfitOP$v,
                      timesteps = timesteps,
                      N = nrun)
  
  # Call for GBMsim for gas prices. Sames as above, except prices returned are 
  # UT gas FPP in opt$cpi dollars per MMBtu.
  GBMsim.GP <- sim(p.init = gas.fpp.init,
                      mu = GBMfitGP$mu,
                      v = GBMfitGP$v,
                      timesteps = timesteps,
                      N = nrun)
  
  # Return result as list
  return(list(GBMsim.OP, GBMsim.GP))
}