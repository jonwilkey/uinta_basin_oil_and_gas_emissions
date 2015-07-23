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

# acut - threshold oil production rate below which a well is consider to be
# abandoned

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

# This function calculates oil and gas production from wells drilled prior to 
# the start of the simulation period. The calculation is performed on each field
# by selecting the production time point at the beginning of the simulation 
# period "tend" for all wells located in that field. This returns a vector of 
# months into the production history for each well in that field. Next, the 
# calculation uses the hyperbolic decline curve coefficients for each field to 
# calculate the production in each timestep of the simulation for each well in 
# that field. This results in a production matrix with rows = wells in a given 
# field, columns = time steps in the simulation period, and values = production 
# from that well in that time step. The column sum is taken of this matrix and 
# added to the total production vectors "oil" and "gas". After repeating this
# process for each field in hypFF, the total production vector is returned.


# Function ----------------------------------------------------------------
priorProd <- function(mo, mg, MC.tsteps, acut, tend.cut) {
  
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
  
  
  # Well abandonment ------------------------------------------------------
  
  # Get row index of oil wells
  ind <- which(ow$wellType == "OW")
  
  # For each oil well
  for (i in 1:length(ind)) {
    
    # Get index of elements in oil production vector that are < value of acut
    temp <- which(oil[ind[i],] < acut)
    
    # If there are elements which are < acut
    if (length(temp) > 0) {
      
      # Then rewrite the production values in those elements with zeroes
      oil[ind[i],temp] <- 0
      gas[ind[i],temp] <- 0
    }
  }
  
  # Return result
  return(list(oil =       oil,
              gas =       gas,
              apilist =   ow$api,
              firstprod = ow$firstprod,
              skip =      skip))
}