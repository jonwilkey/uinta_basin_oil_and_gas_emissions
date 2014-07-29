### Workload-Based Employment Calculation Function ###

# Inputs ------------------------------------------------------------------

# wsim - well information data.table with drilling schedule information

# psim - matrix of production volume timeseries (either oil or gas) for each
# well

# nrun - number of iterations in overalll simulation

# timesteps - vector of dates comprising the timesteps in each iteration of nrun


# Outputs -----------------------------------------------------------------

# jobs.workload - total number of fulltime equivalent jobs required in each year
# for each iteration in nrun


# Description -------------------------------------------------------------

# This function determines the # of man-hours required for drilling a well and 
# processing the fluids it produces. The function first defines a set of 
# conversion factors used for estimating the labor requirements in each step of 
# this process. Next, it generates a labor demand schedule based on the drilling
# schedule laid out by wsim and the production volumes in psim. Annual labor
# demands for each work process are then converted from man-hours to FTE
# employees, and returned as the jobs.workload data.frame.


# Function ----------------------------------------------------------------
workload <- function (wsim, psim, nrun, timesteps) {
  
  # Constants for calculations
  rig.workers <- 23                   # Workers per rig (from Duane Winkler email for directional rig)
  rig.duration <- 15*24               # Duration of drilling a well in hours (to be replaced w/ CDF pick in wsim)
  frack.workers <- 2*rig.workers      # Fracking (assume 2x as many people needed as regular rig)
  frack.duration <- 0.25*rig.duration # Assumed portion of time that rig is onsite that fracking is occuring
  truck.milage <- 2660                # Heavy duty diesel truck (HDDT) miles traveled per well spud (Environ 2011 study Table 6)
  truck.speed <- 16.4                 # HDDT average speed over unpaved roads (Environ 2011 study Table 3)
  gosp.workers <- (4+                 # Freewater knockout drums (horizontal three phase vessels)
                   1                  # Vapor recovery and incinerator
                   )*2*               # Design capacity 10e3 bbl (1:1 oil:water), mass flowrate of 1620 ton/day assuming API gravity = 35 deg.
                   5                  # Shifts for 24/7 operation
  gosp.capacity <- 5e3*365/12         # Design capacity in terms of BPD oil, then converted to bbl/month
  gpp.workers <- (1+                  # Feed prep - condensate and water removal
                  1+                  # Acid gas removal (amine treating)
                  1+                  # Dehydration (gylcol unit)
                  1+                  # Nitrogen rejection (cyrogenic process)
                  1                   # NGL Recovery (turbo-expander and demethanizer tower)  
                  )*2*                # Double # of operators per section since any plant larger than 4.7 MMscfd > 100 ton/day limit
                  5                   # Shifts for 24/7 operation
  gpp.capacity <- 300e3*365/12        # Design capacity for gas processing plant in Mscfd, then converted to Mscf/month
  
  FTE.hours <- 2080/12                # Man-hours per month equivalent to one full-time employee
  
  # Predefine matrix space for man-hour calculations
  manhr.drill <- matrix(0, nrow = nrun, ncol = length(timesteps))
  manhr.frack <- manhr.drill
  manhr.truck <- manhr.drill
  manhr.gosp  <- manhr.drill
  manhr.gpp   <- manhr.drill
  
  # Calculate man-hours per time step for each process step
  for (i in 1:nrun) {
    
    # Get labor estimates for processes related to drilling wells
    for (j in 1:ncol(psim)) {
      ind.ow <- which(wsim$tDrill == j & wsim$runID == i & wsim$wellType == "OW")
      ind.gw <- which(wsim$tDrill == j & wsim$runID == i & wsim$wellType == "GW")
      ind <- length(ind.ow)+length(ind.gw)
      manhr.drill[i,j] <- ind*rig.workers*rig.duration
      manhr.frack[i,j] <- ind*frack.workers*frack.duration
      manhr.truck[i,j] <- ind*truck.milage/truck.speed
    }
    
    # Repeat for labor demands related to oil/gas production
    ind.ow <- which(wsim$runID == i & wsim$wellType == "OW")
    ind.gw <- which(wsim$runID == i & wsim$wellType == "GW")
    for (j in 1:ncol(psim)) {
      manhr.gosp[i,j]  <- ceiling(sum(psim[ind.ow,j])/gosp.capacity)*
        gosp.workers*FTE.hours
      manhr.gpp[i,j]   <- ceiling(sum(psim[ind.gw,j])/gpp.capacity)*
        gpp.workers*FTE.hours
    }
  }
  
  # Predefine matrix space for annual man-hour calculations
  manhr.an.drill <- matrix(0, nrow = nrun, floor(length(timesteps)/12))
  manhr.an.frack <- manhr.an.drill
  manhr.an.truck <- manhr.an.drill
  manhr.an.gosp  <- manhr.an.drill
  manhr.an.gpp   <- manhr.an.drill
  
  # Sum to annual man-hours
  for (i in 1:nrun) {
    for (j in 1:ncol(manhr.an.drill)) {
      tstart <- 12*(j-1)+1
      tstop <- 12*(j-1)+12
      ind <- which(wsim$runID == i & wsim$tDrill >= tstart & wsim$tDrill <= tstop)
      manhr.an.drill[i,j] <- sum(manhr.drill[tstart:tstop])
      manhr.an.frack[i,j] <- sum(manhr.frack[tstart:tstop])
      manhr.an.truck[i,j] <- sum(manhr.truck[tstart:tstop])
      manhr.an.gosp[i,j]  <- sum(manhr.gosp[tstart:tstop])
      manhr.an.gpp[i,j]   <- sum(manhr.gpp[tstart:tstop])
    }
  }
  
  # Adjust FTE.hours back to annual hours
  FTE.hours <- FTE.hours*12
  
  # Convert from man-hours to # of FTE employees
  manhr.an.drill <- c(manhr.an.drill/FTE.hours)
  manhr.an.truck <- c(manhr.an.truck/FTE.hours)
  manhr.an.frack <- c(manhr.an.frack/FTE.hours)
  manhr.an.gosp  <- c(manhr.an.gosp/FTE.hours)
  manhr.an.gpp   <- c(manhr.an.gpp/FTE.hours)
  
  # Convert to dataframe with columns = type of work and rows = years in model
  jobs.workload <- data.frame(manhr.an.drill, manhr.an.frack, manhr.an.truck,
                               manhr.an.gosp, manhr.an.gpp)
  
  # Rename columns
  names(jobs.workload) <- c("drill", "frack", "truck", "GOSP", "GPP")
  
  # Return result
  return(jobs.workload)
}