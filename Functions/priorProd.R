# Function Info -----------------------------------------------------------
# Name:      priorProd.R (Oil/Gas Production from Wells Drilled Prior to Start of Simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# hypFF - data frame with hyperbolic decline curve coefficients fitted to
# field-level production records

# mo/mg - data frame with DCA records for each individual well in the Basin, 
# used here for the column 'tend' which lists the difference (in months) between
# the starting date of the simulation period and the date that the well was 
# drilled. Postive values indicate that the well was drilled 'n' # of months in
# the past, negative value 'n' # of months in the future

# MC.steps - number of time steps in simulation period

# acut - threshold oil production rate below which a well is consider to be
# abandoned

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
priorProd <- function(hypFF, mo, mg, MC.tsteps, acut) {
  
  # Create subset of DCAfit that contains only those wells drilled prior to
  # start of simulation period
  ow <- subset(mo, subset = (tend > 0), select = c("p_api", "w_field_num", "tend", "h_well_type"))
  gw <- subset(mg, subset = (tend > 0), select = c("p_api", "w_field_num", "tend", "h_well_type"))
  
  # Rename columns
  names(ow) <- c("api", "field", "tend", "wellType")
  names(gw) <- c("api", "field", "tend", "wellType")
  
  # Add hypFF DCA coefficients to ow/gw matrices
  
  # Step 1: add columns for coefficients
  ow <- data.frame(ow, qo = rep(NA, nrow(ow)), b = rep(NA, nrow(ow)), Di = rep(NA, nrow(ow)))
  gw <- data.frame(gw, qo = rep(NA, nrow(gw)), b = rep(NA, nrow(gw)), Di = rep(NA, nrow(gw)))
  
  # Step 2: For each individual field, replace NAs with DCCs, starting with oil
  for (i in 1:(nrow(hypFF$oil)-1)) {
    ow$qo[which(ow$field == hypFF$oil$ffo[i])] <- hypFF$oil$qo.oil[i]
    ow$b[ which(ow$field == hypFF$oil$ffo[i])] <- hypFF$oil$b.oil[i]
    ow$Di[which(ow$field == hypFF$oil$ffo[i])] <- hypFF$oil$Di.oil[i]
  }
  
  # Next for gas
  for (i in 1:(nrow(hypFF$gas)-1)) {
    gw$qo[which(gw$field == hypFF$gas$ffg[i])] <- hypFF$gas$qo.gas[i]
    gw$b[ which(gw$field == hypFF$gas$ffg[i])] <- hypFF$gas$b.gas[i]
    gw$Di[which(gw$field == hypFF$gas$ffg[i])] <- hypFF$gas$Di.gas[i]
  }
  
  # Step 3: Replace any remaining NAs with DCCs for Field 999
  ow$qo[which(is.na(ow$qo))] <- hypFF$oil$qo.oil[nrow(hypFF$oil)]
  ow$b[ which(is.na(ow$b))] <-  hypFF$oil$b.oil[nrow(hypFF$oil)]
  ow$Di[which(is.na(ow$Di))] <- hypFF$oil$Di.oil[nrow(hypFF$oil)]
  
  gw$qo[which(is.na(gw$qo))] <- hypFF$gas$qo.gas[nrow(hypFF$gas)]
  gw$b[ which(is.na(gw$b))] <-  hypFF$gas$b.gas[nrow(hypFF$gas)]
  gw$Di[which(is.na(gw$Di))] <- hypFF$gas$Di.gas[nrow(hypFF$gas)]
  
  
  # Predefine production matrix
  oil <- matrix(0, nrow = nrow(ow), ncol = MC.tsteps)
  gas <- matrix(0, nrow = nrow(gw), ncol = MC.tsteps)
  
  # For each time step, calculate production
  for (i in 1:MC.tsteps) {
    
    # Calculate production
    oil[,i] <- with(ow, qo*(1+b*Di*(tend+i-1))^(-1/b))
    gas[,i] <- with(gw, qo*(1+b*Di*(tend+i-1))^(-1/b))
  }
  
  
  # Well abandonment --------------------------------------------------------
  
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
  return(list(oil = oil, gas = gas, apilist = ow$api))
}