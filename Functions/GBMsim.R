# Function Info -----------------------------------------------------------
# Name:      GBMsim.R (Geometric Brownian Motion Price Path Simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - path names for file directoires (data, plotting, etc.)

# oil.fpp.init - National first purchase price (FPP) for oil in opt$cpi $/bbl

# gas.fpp.init - National first purchase price (FPP) for gas in opt$cpi $/MCF

# timesteps - Number of months to be simulated

# nrun - number of iterations in overall simulation

# GBMfitOP fitted parameters "v" and "mu" for oil price

# GBMfitGP fitted parameters "v" and "mu" for gas price


# Outputs -----------------------------------------------------------------

# GBMsim.OP/GP - data.frame containing simulated energy price paths where rows =
# simulation runs and columns = timesteps


# Description -------------------------------------------------------------

# This function simulates oil and gas price paths by using Geometric Brownian 
# Motion (GBM) to simulate oil and gas price paths. Given N number of GBM paths
# each of length T for given drift (mu) and squared volatility (v) parameters
# and initial price p.init. Returns a matrix whose columns hold the simulated
# paths. See (see Pindyck and Dixit p. 72).


# Function ----------------------------------------------------------------
GBMsim <- function(path, oil.fpp.init, gas.fpp.init, timesteps, nrun, GBMfitOP,
                   GBMfitGP) {
  
  # Internal Function -----------------------------------------------------
  
  # --- Inputs ---
  # p.init - Initial price
  # mu - Drift
  # v - Volatility
  # timesteps - number of timesteps in each path
  # N - number of GBM paths to generate
  
  sim <- function(p.init, mu, v, timesteps, N) {
    
    # Predefine space for results
    sim.data <- matrix(nrow=N, ncol=timesteps+1)
    
    # Assign initial price to first column of results matrix
    sim.data[,1] <- rep(p.init, nrow(sim.data))
    
    # For each N price path
    for (j in 1:nrow(sim.data)) {
      
      # For each timestep
      for (i in 2:ncol(sim.data)) {
        
        # Calculate price using GBM function
        sim.data[j,i] <- (1+mu)*sim.data[j,i-1]+sqrt(v)*sim.data[j,i-1]*rnorm(1,0,1)
      }
    }
    return(sim.data)
  }
  
  
  # Generate price paths --------------------------------------------------
  
  # Call for GBMsim for oil prices. Rows are simulations, columns are timesteps.
  # First column is simply p.init price, total number of columns = value given
  # in timesteps + 1. Prices returned are UT crude oil FPP in opt$cpi dollars
  # per bbl.
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
  
  # Drop first column (prices at timestep just prior to start of simulation time
  # period)
  GBMsim.OP <- GBMsim.OP[,-1]
  GBMsim.GP <- GBMsim.GP[,-1]
  
  # Return result as list
  return(list(GBMsim.OP, GBMsim.GP))
}