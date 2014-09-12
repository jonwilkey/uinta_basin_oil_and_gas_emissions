### Greenhouse Gas Calculation Function ###

# Inputs ------------------------------------------------------------------

# wsim - well information data.table containing timestep when each well is
# drilled and emission factors for each well

# psim - matrix of production volume timeseries (of oil or gas) for each well

# nrun - number of iterations in overall Monte-Carlo simulation

# timesteps - vector of dates comprising number of timesteps in each iteration
# of nrun

# ind.ow - index (of row numbers) of oil wells in wsim and psim

# ind.gw - index (of row numbers) of gas wells in wsim and psim

# truckload - capacity of oil trucks (in bbl/truck) used to ship oil from Basin
# to North SLC refineries


# Outputs -----------------------------------------------------------------

# result - list object containing the matrices CO2e.tot and CH4.tot, which give
# the total CO2 equivalent emissions (in 1e3 kg) and CH4 (in MCF) for each
# timestep in each iteration of nrun

# Description -------------------------------------------------------------

# This function uses the information in wsim and psim to calculate the amount of
# CO2 equivalent (CO2e) and CH4 emissions from oil and gas development. This is 
# done by summing together emissions from each step of the oil and gas 
# production process from well drilling and completion through delivery to 
# market. Emissions factors from each step are referenced from wsim and, where 
# applicable, are multiplied by production volumes recorded in psim. This is 
# first done for each individual well in the CO2e and CH4 matrices, and are then
# reported as a total amount for each simulation run in nrun. The matrices of
# total emissions are returned as a list object.


# Function ----------------------------------------------------------------
GHG <- function(wsim, psim, nrun, timesteps, ind.ow, ind.gw, truckload) {
  
  # Define CO2e, CH4, and total matrices
  CO2e <- matrix(0, nrow = nrow(psim), ncol = ncol(psim))
  CH4 <- CO2e
  CO2e.tot <- matrix(0, nrow = nrun, ncol = ncol(psim))
  CH4.tot <- CO2e.tot
  
  # Insert drilling and completion emissions into CO2e
  for (i in 1:length(timesteps)) {
    ind <- which(wsim$tDrill == i)
    CO2e[ind,i] <- wsim$EF.dcw[ind]
  }
  
  # Production-based emissions
  for (i in 1:length(timesteps)) {
    # For gas production from gas wells
    CO2e[ind.gw,i] <- CO2e[ind.gw,i]+           # Drilling and completion
                      (wsim$EF.prd.gas[ind.gw]+ # Gas production
                      wsim$EF.prc[ind.gw]+      # Gas processing
                      wsim$EF.trs.gas[ind.gw])* # Gas transportation
                      psim[ind.gw,i]            # Multiply by gas produced
    
    # For oil production from oil wells
    CO2e[ind.ow,i] <- CO2e[ind.ow,i]+                                              # Drilling and completion
                      wsim$EF.prd.oil[ind.ow]*psim[ind.ow,i]+                      # Oil production
                      wsim$EF.trs.oil[ind.ow]*round(sum(psim[ind.ow,i])/truckload) # Oil transportation by truck
    
    # CH4 emissions from gas wells
    CH4[ind.gw,i] <- (wsim$EF.tot[ind.gw]+         # CH4 from gas production
                      wsim$EF.trs.unconv[ind.gw])* # CH4 from transportation/storage/distribution
                      psim[ind.gw,i]               # Multiply by gas produced
  }
  
  # Sum to total CO2e and CH4 for each simulation run
  for (i in 1:nrun) {
    CO2e.tot[i,] <- colSums(CO2e[which(wsim$runID == i),])
    CH4.tot[i,]  <- colSums(CH4[which(wsim$runID == i),])
  }
  
  # Save as list
  result <- list(CO2e.tot, CH4.tot)
  
  # Return result
  return(result)
}