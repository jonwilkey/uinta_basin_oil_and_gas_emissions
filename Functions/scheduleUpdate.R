# Function Info -----------------------------------------------------------
# Name:      scheduleUpdate.R (DOGM *.dbf database file processing/updating)
# Author(s): Michael Hogue, Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - list object containing directory paths for file I/O


# Outputs -----------------------------------------------------------------

# production.rda - Processed and merged copy of three DOGM *.dbf files
#                  (proddata, histdata, and welldata). Also saves individual
#                  copies of each database file in *.csv and *.rda formats.


# Description -------------------------------------------------------------

# This function is used for extracting the information contained in DOGM's *.dbf
# database files into a format usable by R (*.rda). In addition to extracting
# the database files, this function also merges the database files together into
# a single large dataframe and adds several columns for subsequent analysis.


# Function ----------------------------------------------------------------
scheduleUpdate <- function(path, p, tsteps, field) {
  
  # Extract well data from p ------------------------------------------------
  
  # This segment gets all of the pertinent information (well type, depth, field,
  # etc.) from p and formats it so that there is one row for each well.
  
  # Add column 'prod_date' as truncated-to-month version of h_first_prod
  p$prod_date <- as.Date(as.yearmon(p[,"h_first_prod"]))
  
  # Create dataframe containing dates of 1st production for each unique APD #
  # and the field it is located in
  well <- sqldf("select distinct(p_api), prod_date, w_field_num, w_well_type,
                w_lease_type, h_td_md
                from p
                order by prod_date")
  
  # Drop NA observations
  well <- na.omit(well)
  
  # Only wells for (1) desired timeframe with (2) depths > minimum well depth 
  # input option and (3) that are oil wells or gas wells (i.e. no dry wells,
  # water injection/disposal wells, etc.)
  well <- subset(well, subset = c(prod_date >= min(tsteps) &
                                  prod_date <= max(tsteps) &
                                  h_td_md > opt$min.well.depth &
                                  (w_well_type == "OW" |
                                   w_well_type == "GW")))
  
  
  # Actual oil and gas production histories ---------------------------------
  
  # This segment shuffles the actual DOGM data into the same format used in
  # the "wsim" & "psim" data.frame/matrix.
  
  # === psim ===
  
  # Initial definition of psim
  psim <- data.frame(tsteps)
  
  # WARNING - very slow loop. This loop creates a subset of p for each unique
  # well in "well," then picks which columns to keep based on well type.
  # Finally, the loop merges the results from the current well with the results
  # from all previous iterations and renames the columns as it goes.
  for (i in 1:nrow(well)) {
    temp <- unique(subset(p,
                          subset = p$p_api == well$p_api[i],
                          select = c("p_rpt_period", "p_oil_prod", "p_gas_prod",
                                     "w_well_type")))
    if (temp$w_well_type[1] == "OW") {
      temp <- temp[,c("p_rpt_period", "p_oil_prod")]
    } else {
      temp <- temp[,c("p_rpt_period", "p_gas_prod")]
    }  
    psim <- merge(x = psim, y = temp,
                  by.x = "tsteps", by.y = "p_rpt_period",
                  all.x = TRUE)
    names(psim)[i+1] <- i
  }
  
  # Transform from data.frame into matrix
  psim <- as.matrix(psim[,2:(nrow(well)+1)])
  
  # Transpose so that matrix rows = wells and columns = timesteps
  psim <- t(psim)
  
  # Rename for export
  psim.actual <- psim
  
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
  acoef <- apply(psim.actual, MARGIN = 1, FUN = max)
  
  # Generate dataframe for export
  wsim.actual <- data.frame(well$p_api,
                            tDrill,
                            runID,
                            well$w_well_type,
                            fieldnum,
                            well$h_td_md,
                            lease,
                            acoef)
  
  # Rename to match wsim format
  names(wsim.actual) <- c("wellID", "tDrill", "runID", "wellType", "fieldnum",
                          "depth", "lease", "acoef")
  
  
  # Fraction Gas/Oil --------------------------------------------------------
  
  # Determine the probability that any given well is going to be a gas well
  prob.GW <- nrow(well[which(well$w_well_type == "GW"),])/nrow(well)
  
  
  # Field Fractions ---------------------------------------------------------
  
  # === Oil Wells ===
  
  # Field count oil (fco) = # of unique well APIs in each field that are oil
  # wells
  fco <- sqldf("select w_field_num, count(p_api)
               from well
               where w_well_type = 'OW'
               group by w_field_num")
  
  # Predefine field fraction oil (ffo)
  ffo <- rep(0, length.out = length(field))
  
  # Pick out wells from fco that match given field number
  for (i in 1:(length(field)-1)) {
    if (length(fco[which(fco[,1] == field[i]),2]) > 0) {
      ffo[i] <- fco[which(fco[,1] == field[i]),2]
    }
  }
  ffo[length(ffo)] <- sum(fco[,2]) - sum(ffo[1:(length(ffo)-1)])
  
  # From counts to fraction
  ffo <- ffo/sum(fco[,2])
  
  # Calculate CDF
  cdf.ffo <- cumsum(ffo)
  
  # Normalize
  cdf.ffo <- cdf.ffo/max(cdf.ffo)
  
  # Turn into dataframe for export
  cdf.ffo <- data.frame(field, cdf.ffo)
  names(cdf.ffo) <- c("Field", "CDF")
  
  # === Gas Wells ===
  
  # Repeat above steps for gas wells
  fcg <- sqldf("select w_field_num, count(p_api)
               from well
               where w_well_type = 'GW'
               group by w_field_num")
  
  ffg <- rep(0, length.out = length(field))
  
  for (i in 1:(length(field)-1)) {
    if (length(fcg[which(fcg[,1] == field[i]),2]) > 0) {
      ffg[i] <- fcg[which(fcg[,1] == field[i]),2]
    }
  }
  ffg[length(ffg)] <- sum(fcg[,2]) - sum(ffg[1:(length(ffg)-1)])
  
  ffg <- ffg/sum(fcg[,2])
  cdf.ffg <- cumsum(ffg)
  cdf.ffg <- cdf.ffg/max(cdf.ffg)
  cdf.ffg <- data.frame(field, cdf.ffg)
  names(cdf.ffg) <- c("Field", "CDF")  
  
  
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
  
  
  # Export Results ----------------------------------------------------------
  
  # Save PDF & CDF results
  save(file=file.path(path$data, "cdf_schedule_v2.rda"),
       list=c("cdf.ffg",
              "cdf.ffo",
              "cdf.flt",
              "prob.GW"))
  
  # Save wsim.actual data.frame
  save(file=file.path(path$data, "wsim_actual_v2.rda"), list=c("wsim.actual"))
  
  # Save psim.actual matrix
  save(file=file.path(path$data, "psim_actual_v2.rda"), list=c("psim.actual"))
}