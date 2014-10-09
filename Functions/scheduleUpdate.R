# Function Info -----------------------------------------------------------
# Name:      scheduleUpdate.R (Actual drilling/production schedule CDFs)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# p - DOGM databse data.frame

# tsteps - Range of time steps in modeling period

# field - Vector of field numbers to be considered individually

# min.depth - Minimum well depth, used as subsetting criteria

# version - Version number for file naming of exported data.frames


# Outputs -----------------------------------------------------------------

# cdf.ff - Cumulative probability distribution function (CDF) for field fraction,
#          identifying the likelyhood that any given well drilled will be
#          located in a particular field

# cdf.flt - CDF for lease type by field

# prob - The probability that a well is going to be (1) dry, and if not then (2)
#        that it is a gas well.

# wsim.actual - Data.frame structured to resemble wsim object produced by
#               welldata.R function, containing one row for each well with
#               information like well type, depth, field, time drilled, etc.

# osim.actual - Oil production from each well as a time series

# gsim.actual - Gas production from each well as a time series


# Description -------------------------------------------------------------

# This function loads the production data.frame "p" and extracts a single table 
# with one row for each well in the modeling period. It then builds tables for 
# the actual oil and gas production time series from each of those wells. Next
# it determines the CDF for various factors, such as field number, well type,
# and lease type. Finally it exports all of the extracted information.


# Function ----------------------------------------------------------------
scheduleUpdate <- function(path, p, tsteps, field, min.depth, max.depth,
                           well.depth.step, version) {
  
  # Extract well data from p ------------------------------------------------
  
  # This segment gets all of the pertinent information (well type, depth, field,
  # etc.) from p and formats it so that there is one row for each well.
  
  # Add column 'prod_date' as truncated-to-month version of h_first_prod
  p$prod_date <- as.Date(as.yearmon(p[,"h_first_prod"]))
  
  # Create dataframe containing dates of 1st production for each unique APD #
  # and the field it is located in
  well <- sqldf("select distinct p_api, prod_date, w_field_num, h_well_type,
                w_well_type, w_lease_type, h_td_md, h_rec_seq
                from p
                order by prod_date")
  
  # Drop NA observations
  well <- well[which(!is.na(well$h_well_type)),]
  well <- well[which(well$h_well_type != "NA"),]
  
  # Only wells for (1) desired timeframe with (2) depths > minimum well depth 
  # input option and (3) that were drilled with the intention of producing - oil
  # wells, gas wells, and dry wells (i.e. no water injection/disposal wells, gas
  # storage, water source wells, etc.)
  well <- subset(well, subset = c(prod_date >= min(tsteps) &
                                  prod_date <= max(tsteps) &
                                  h_td_md > min.depth &
                                  (h_well_type == "OW" |
                                   h_well_type == "GW" |
                                   h_well_type == "D")))
  
  # There can be multiple rows for a single well given query above (i.e. well 
  # was initially one well type, changed to another, result is two rows). Scan
  # back through prior query and build table with one row per unique API # that
  # lists just initial well type, assuming lowest value of h_rec_seq is earliest
  # in chronology.
  
  # First, get list of unique API #s
  api.list <- unique(well$p_api)
  
  # Predefine data.frame for cleaned up version "well"
  clean.well <- NULL
  
  # Next, for each API # get all rows in "well" that match. If there are
  # multiple rows, only keep that one that has lowest h_rec_seq value.
  for (i in 1:length(api.list)) {
    temp <- well[which(well$p_api == api.list[i]),]
    if (nrow(temp) > 1) {
      temp <- temp[which.min(temp$h_rec_seq),]
    }
    clean.well <- rbind(clean.well, temp)
  }
  
  # Finally, redefine clean.well as well
  well <- clean.well
  
  # Actual oil and gas production histories ---------------------------------
  
  # This segment shuffles the actual DOGM data into the same format used in
  # the "wsim" & "psim" data.frame/matrix.
  
  # === psim ===
  
  # Initial definition of osim and gsim (oil/gas production simulation
  # data.frame)
  osim <- data.frame(tsteps)
  gsim <- osim
  
  # Get list of unique well API #s in "well" data.frame (some wells have
  # multiple rows because of change in well type).
  
  
  # WARNING - very slow loop. This loop creates a subset of p for each unique 
  # well in "well," then pulls out that well's oil/gas production into two
  # temporary subsets. Finally, the loop merges the subsets from the current
  # well with the results from all previous iterations and renames the columns
  # as it goes.
  for (i in 1:length(api.list)) {
    temp <- unique(subset(p,
                          subset = p$p_api == api.list[i],
                          select = c("p_rpt_period", "p_oil_prod", "p_gas_prod")))
    osim <- merge(x = osim, y = temp[,c(1,2)],
                  by.x = "tsteps", by.y = "p_rpt_period",
                  all.x = TRUE)
    gsim <- merge(x = gsim, y = temp[,c(1,3)],
                  by.x = "tsteps", by.y = "p_rpt_period",
                  all.x = TRUE)
    names(osim)[i+1] <- i
    names(gsim)[i+1] <- i
  }
  
  # Transform from data.frame into matrix
  osim <- as.matrix(osim[,2:(length(api.list)+1)])
  gsim <- as.matrix(gsim[,2:(length(api.list)+1)])
  
  # Transpose so that matrix rows = wells and columns = timesteps
  osim <- t(osim)
  gsim <- t(gsim)
  
  # Rename for export
  osim.actual <- osim
  gsim.actual <- gsim
  
  # === wsim ===
  
  # Relabel p_api as wellID number
  well$p_api <- seq(1:nrow(well))
  
  # Define new column "runID" as runID == 1 for all wells
  runID <- rep(1, times = nrow(well))
  
  # Conventiently, use runID to predefine space for "tDrill", which holds
  # timestep that each well is drilled in
  tDrill <- runID
  
  # Match indices where wells were drilled (prod_date) with equivalent timesteps
  # in "tsteps"
  for (i in 1:length(tsteps)) {
    ind <- which (well$prod_date == tsteps[i])
    tDrill[ind] <- i
  }
  
  # Create new vector of field numbers
  fieldnum <- well$w_field_num
  
  # We need to find all rows which are associated with field numbers not
  # included in the vector "field" for individual analysis. For each field # in
  # "field" except the catch-all field (999), find which rows in fieldnum which
  # match that field and concatonate them together into the vector "ind".
  ind <- NULL
  for (i in 1:(length(field)-1)) {
    temp <- which(fieldnum == field[i])
    ind <- c(ind, temp)
  }
  
  # Now generate a sequence of row #s equal to the length of fieldnum
  ind999 <- seq(1:length(fieldnum))
  
  # Subtract out all elements in "ind" to get row #s of wells located fields
  # other than those listed in vector "field"
  ind999 <- ind999[-ind]
  
  # Replace all such fields with the catch-all field "999"
  fieldnum[ind999] <- 999
  
  # Create new vector for lease type and retype as numeric vector
  lease <- as.numeric(well$w_lease_type)
  
  # Get maximum production rate for each well
  acoef.oil <- apply(osim.actual, MARGIN = 1, FUN = max)
  acoef.gas <- apply(gsim.actual, MARGIN = 1, FUN = max)
  
  # Generate dataframe for export
  wsim.actual <- data.frame(well$p_api,
                            tDrill,
                            runID,
                            well$h_well_type,
                            fieldnum,
                            well$h_td_md,
                            lease,
                            acoef.oil,
                            acoef.gas)
  
  # Rename to match wsim format
  names(wsim.actual) <- c("wellID", "tDrill", "runID", "wellType", "fieldnum",
                          "depth", "lease", "acoefOil", "acoefGas")
  
  
  # Field Fractions ---------------------------------------------------------
  
  # Field count (fc) = # of unique well APIs in each field
  fc <- sqldf("select w_field_num, count(p_api)
              from well
              group by w_field_num")
  
  # Predefine field fraction (ff)
  ff <- rep(0, length.out = length(field))
  
  # Pick out wells from fco that match given field number
  for (i in 1:(length(field)-1)) {
    if (length(fc[which(fc[,1] == field[i]),2]) > 0) {
      ff[i] <- fc[which(fc[,1] == field[i]),2]
    }
  }
  ff[length(ff)] <- sum(fc[,2])-sum(ff[1:(length(ff)-1)])
  
  # From counts to fraction
  ff <- ff/sum(fc[,2])
  
  # Calculate CDF
  cdf.ff <- cumsum(ff)
  
  # Normalize
  cdf.ff <- cdf.ff/max(cdf.ff)
  
  # Turn into dataframe for export
  cdf.ff <- data.frame(field, cdf.ff)
  names(cdf.ff) <- c("Field", "CDF")
  
  
  # Probabilities for well type by field ------------------------------------
  
  # Predefine matrix for results
  prob <- matrix(0, nrow = length(field), ncol = 3)
  
  # For each field, get counts of (1) dry well, (2) gas wells, and (3) all wells
  for (i in 1:(length(field)-1)) {
    temp <- subset(well, subset = (w_field_num == field[i]))
    prob[i,1] <- length(which(temp$h_well_type == "D"))
    prob[i,2] <- length(which(temp$h_well_type == "GW"))
    prob[i,3] <- nrow(temp)
  }
  # Use previously defined index for Field 999
  temp <- well[ind999,]
  prob[nrow(prob),] <- c(length(which(temp$h_well_type == "D")),
                         length(which(temp$h_well_type == "GW")),
                         nrow(temp))
  
  # Calculate probability first for (1) a well being dry, and (2) if the well is
  # not dry, that it will be a gas well (conversely if it isn't a gas well then
  # its an oil well). Vector operation does this for each field.
  dry <- prob[,1]/prob[,3]
  gas <- prob[,2]/(prob[,3]-prob[,1])
  
  # Redefine prob as data.frame with probabilities above and add field column
  prob <- data.frame(field, dry, gas)
  
  
  # Lease type --------------------------------------------------------------
  
  # Pull lease type stats from "well" to get counts of unique wells in each type
  # of lease category in each field. Lease types are (1) Federal, (2) Indian,
  # (3) State, (4) Fee. Fraction lease type (flt) matrix has rows = field # and 
  # columns = landowner type.
  flt <- matrix(0, nrow = length(field), ncol = 4)
  for (i in 1:(nrow(flt)-1)) {
    temp <- subset(well, w_field_num == field[i])
    for (j in 1:ncol(flt)) {
      flt[i,j] <- length(which(temp$w_lease_type == j))
    }
  }
  
  # To handle the "other" field category, use previously defined row index
  # "ind999"
  temp <- well[ind999,]
  for (j in 1:ncol(flt)) {
    flt[nrow(flt),j] <- length(which(temp$w_lease_type == j))
  }
  
  # Divide by row sums to turn counts into fractions
  flt <- flt/rowSums(flt)
  
  # Apply cumulative sum to each row and normalize
  for (i in 1:nrow(flt)) {
    flt[i,] <- cumsum(flt[i,])
    flt[i,] <- flt[i,]/max(flt[i,])
  }
  
  # Transform to dataframe for export
  cdf.flt <- data.frame(field, flt)
  names(cdf.flt) <- c("Field", "Federal", "Indian", "State", "Fee")
  
  
  # Well Depth Analysis -----------------------------------------------------
  
  # Define local function for getting well depth CDFs
  welldepth <- function (input) {
    DF <- density(input$h_td_md,
                     from = min.depth,
                     to = max.depth,
                     n = (max.depth-min.depth)/well.depth.step)    
    cdf <- cumsum(DF$y*diff(DF$x[1:2]))
    cdf <- cdf/max(cdf)
    return(cdf)
  }
  
  # Define range of depths
  x <- seq(from = min.depth,
           to = max.depth,
           length.out = (max.depth-min.depth)/well.depth.step)
  
  # Can't differentiate CDFs for depth by field because there are several fields
  # for which there are not enough data points to run density(). As a result,
  # just run once for all oil wells and gas wells, respectively.
  cdf.depth.ow <- data.frame(x, welldepth(subset(well, h_well_type == "OW")))
  cdf.depth.gw <- data.frame(x, welldepth(subset(well, h_well_type == "GW")))
  
  # Rename columns
  names(cdf.depth.ow) <- c("x", "y")
  names(cdf.depth.gw) <- c("x", "y")  
  
  
  # Export Results ----------------------------------------------------------
  
  # Save CDF and probability results
  save(file=file.path(path$data,
                      paste("cdf_schedule_", version, ".rda", sep = "")),
       list=c("cdf.ff",
              "cdf.flt",
              "cdf.depth.ow",
              "cdf.depth.gw",
              "prob"))
  
  # Save wsim.actual data.frame
  save(file=file.path(path$data,
                      paste("wsim_actual_", version, ".rda", sep = "")),
       list=c("wsim.actual"))
  
  # Save osim.actual & gsim.actual matrices
  save(file=file.path(path$data,
                      paste("psim_actual_", version, ".rda", sep = "")),
       list=c("osim.actual",
              "gsim.actual"))
}