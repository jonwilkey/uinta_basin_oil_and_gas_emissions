# Function Info -----------------------------------------------------------
# Name:      GBMsim.R (Geometric Brownian Motion Price Path Simulation)
# Author(s): Michael Hogue, Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# p.init - Initial price

# mu - Drift

# v - Volatility

# timesteps - number of timesteps in each path

# N - number of GBM paths to generate


# Outputs -----------------------------------------------------------------

# sim.data - Matrix whose columns hold the simulated paths


# Description -------------------------------------------------------------

# The following function simulates a given number (N) of GBM paths each of
# length T for given drift (mu) and squared volatility (v) parameters and
# initial price p.init. Returns a matrix whose columns hold the simulated paths.
# See (see Pindyck and Dixit p. 72).


# Function ----------------------------------------------------------------
GBMsim <- function(p.init, mu, v, timesteps, N) {
  sim.data <- matrix(nrow=timesteps+1, ncol=N)
  sim.data[1,] <- rep(p.init, ncol(sim.data))
  for (j in 1:ncol(sim.data)) {
    for (i in 2:nrow(sim.data)) {
      sim.data[i, j] <- (1 + mu) * sim.data[i-1, j] +
        sqrt(v) * sim.data[i-1, j] * rnorm(1,0,1)
    }
  }
  return(sim.data)
}