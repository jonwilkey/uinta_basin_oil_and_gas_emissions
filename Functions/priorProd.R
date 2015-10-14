# Function Info -----------------------------------------------------------
# Name:      priorProd.R (Oil/Gas Production from Wells Drilled Prior to Start of Simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# mo/mg - data frame with DCA records for each individual well in the Basin, 
# used here for the column 'tend' which lists the difference (in months) between
# the starting date of the simulation period and the date that the well was 
# drilled. Postive values indicate that the well was drilled 'n' # of months in
# the past, negative value 'n' # of months in the future

# MC.steps - number of time steps in simulation period

# tend.cut - cutoff threshold for how old a well can be and still be included as
# a prior well. For example, if tend.cut == 50, then any well that doesn't have 
# a last decline curve fit (either because it had too few production records or 
# because the solver failed to converge) and is > 50 months old would dropped
# from population of prior wells


# Outputs -----------------------------------------------------------------

# list with matrices of oil/gas production from each well drilled prior to the
# start of the simulation period (i.e. prior wells) based on how far into
# production history each well is and what field its located in


# Description -------------------------------------------------------------

# This function (1) collects well information about prior wells and (2)
# calculates oil and gas production from wells drilled prior to the start of the
# simulation period by extrapolating from the last fitted hyperbolic decline
# curve for each well with a hyperbolic decline curve fit in mo or mg. Any wells
# without fits (which were either skipped due to having too few points or for
# which the nonlinear solver failed to converge) are identified in the
# data.frame skip.


# Function ----------------------------------------------------------------
priorProd <- function(mo, mg, MC.tsteps, tend.cut) {
  
  # Get DCCs from wells with fits -----------------------------------------
  
  # Create subset of DCAfit that contains only those wells drilled prior to
  # start of simulation period and that have a last decline curve fit
  ind <- which(mo$tend > 0 & mo$fit.2 == 1 & mg$fit.2 == 1)
  ow <- subset(mo[ind,], select = c("p_api", "w_field_num", "tend", "h_well_type", "firstprod", "qo.2", "b.2", "Di.2"))
  gw <- subset(mg[ind,], select = c("p_api", "w_field_num", "tend", "h_well_type", "firstprod", "qo.2", "b.2", "Di.2"))
  
  # Rename columns
  names(ow) <- c("api", "field", "tend", "wellType", "firstprod", "qo", "b", "Di")
  names(gw) <- c("api", "field", "tend", "wellType", "firstprod", "qo", "b", "Di")
  
  
  # Get list of prior wells w/o fits --------------------------------------
  
  # Drop wells that had fits and select only those wells within tend constraints
  skip <- subset(mo[-ind,],
                 subset = (tend > 0 & tend <= tend.cut),
                 select = c("p_api", "w_field_num", "tend", "h_well_type", "firstprod"))
  
  skip <- rbind(skip,
                subset(mg[-ind,],
                       subset = (tend > 0 & tend <= tend.cut),
                       select = c("p_api", "w_field_num", "tend", "h_well_type", "firstprod")))
  
  # Rename columns
  names(skip) <- c("api", "field", "tend", "wellType", "firstprod")
  
  # Remove any duplicates
  skip <- unique(skip)
  
  
  # Calculate production for wells with fits ------------------------------
  
  # Predefine production matrix
  oil <- matrix(0, nrow = nrow(ow), ncol = MC.tsteps)
  gas <- matrix(0, nrow = nrow(gw), ncol = MC.tsteps)
  
  # For each time step, calculate production
  for (i in 1:MC.tsteps) {
    
    # Calculate production
    oil[,i] <- with(ow, qo*(1+b*Di*(tend+i-1))^(-1/b))
    gas[,i] <- with(gw, qo*(1+b*Di*(tend+i-1))^(-1/b))
  }
  
  
  # Return results --------------------------------------------------------
  
  # Return result
  return(list(oil =       oil,
              gas =       gas,
              apilist =   ow$api,
              firstprod = ow$firstprod,
              skip =      skip))
}