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


# Outputs -----------------------------------------------------------------

# data.frame with vector of total oil/gas production from all wells drilled 
# prior to the start of the simulation period (i.e. prior wells) based on how
# far into production history each well is and what field its located in


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
priorProd <- function(hypFF, mo, mg, MC.tsteps) {
  
  # Create subset of DCAfit that contains on those wells drilled prior to start of
  # simulation period
  ow <- subset(mo, subset = (tend > 0), select = c("w_field_num", "tend"))
  gw <- subset(mg, subset = (tend > 0), select = c("w_field_num", "tend"))
  
  # Rename columns
  names(ow) <- c("field", "tend")
  names(gw) <- c("field", "tend")
  
  # Predefine space for total production vectors
  oil <- rep(0, times = MC.tsteps)
  gas <- oil
  
  # Predefine index for Field 999
  ind999.ow <- NULL
  ind999.gw <- NULL
  
  # For all fields being analyzed individually
  for (i in 1:(nrow(hypFF)-1)) {
    
    # Get index of rows in ow/gw that exist in field i
    ind.ow <- which(ow$field == hypFF$field[i])
    ind.gw <- which(gw$field == hypFF$field[i])
    
    # Get vector of tend values for field i
    otend <- ow$tend[ind.ow]
    gtend <- gw$tend[ind.gw]
    
    # Predefine space for results of production calculation
    oq <- matrix(0, nrow = length(otend), ncol = MC.tsteps)
    gq <- matrix(0, nrow = length(gtend), ncol = MC.tsteps)
    
    # For each timestep in the simulation period
    for (j in 1:MC.tsteps) {
      
      # Calculate production vector for each element of tend
      oq[,j] <- hypFF$qo.oil[i]*(1+hypFF$b.oil[i]*hypFF$Di.oil[i]*(otend+j-1))^(-1/hypFF$b.oil[i])
      gq[,j] <- hypFF$qo.gas[i]*(1+hypFF$b.gas[i]*hypFF$Di.gas[i]*(gtend+j-1))^(-1/hypFF$b.gas[i])
    }
    
    # Calculate total oil/gas production from field i
    oil <- oil+colSums(oq)
    gas <- gas+colSums(gq)
    
    # Build index of rows to exclude from Field 999
    ind999.ow <- c(ind999.ow, ind.ow)
    ind999.gw <- c(ind999.gw, ind.gw)
  }
  
  # For Field 999, use exclusion index to get tend values
  otend <- ow$tend[-ind999.ow]
  gtend <- gw$tend[-ind999.gw]
  
  # Predefine space for results of production calculation
  oq <- matrix(0, nrow = length(otend), ncol = MC.tsteps)
  gq <- matrix(0, nrow = length(gtend), ncol = MC.tsteps)
  
  # For each timestep in the simulation period
  for (j in 1:MC.tsteps) {
    
    # Calculate production vector for each element of tend
    oq[,j] <- hypFF$qo.oil[i]*(1+hypFF$b.oil[i]*hypFF$Di.oil[i]*(otend+j-1))^(-1/hypFF$b.oil[i])
    gq[,j] <- hypFF$qo.gas[i]*(1+hypFF$b.gas[i]*hypFF$Di.gas[i]*(gtend+j-1))^(-1/hypFF$b.gas[i])
  }
  
  # Calculate total oil/gas production from field i
  oil <- oil+colSums(oq)
  gas <- gas+colSums(gq)
  
  # Return result
  return(data.frame(oil = oil, gas = gas))
}