# Function Info -----------------------------------------------------------
# Name:      scheduleUpdate.R (Actual drilling/production schedule CDFs)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - list object containing directory paths for file I/O

# p - DOGM database data.frame

# tsteps - Range of time steps in modeling period

# min.depth - Minimum well depth, used as subsetting criteria

# ver - Version number for file naming of exported data.frames

# field.cutoff - minimum fraction of total wells in Uinta Basin which a field
# must have in order to be analyzed individually


# Outputs -----------------------------------------------------------------

# cdf.ff - Cumulative probability distribution function (CDF) for field fraction,
#          identifying the likelyhood that any given well drilled will be
#          located in a particular field

# cdf.flt - CDF for lease type by field

# cdf.depth.ow - CDF for well depth for oil wells

# cdf.depth.gw - CDF for well depth for gas wells

# prob - The probability that a well is going to be (1) dry, and if not then (2)
#        that it is a gas well.

# wsim.actual - Data.frame structured to resemble wsim object produced by
#               welldata.R function, containing one row for each well with
#               information like well type, depth, field, time drilled, etc.

# osim.actual - Oil production from each well as a time series

# gsim.actual - Gas production from each well as a time series

# well.actual - Listing of all actual wells with data that was used to generate
# CDFs below

# field - Vector of field numbers to be considered individually


# Description -------------------------------------------------------------

# This function loads the production data.frame "p" and extracts a single table 
# with one row for each well in the modeling period. It then builds tables for 
# the actual oil and gas production time series from each of those wells. Next
# it determines the CDF for various factors, such as field number, well type,
# and lease type. Finally it exports all of the extracted information.


# Function ----------------------------------------------------------------
scheduleUpdate <- function(path, p, tsteps, min.depth, max.depth,
                           well.depth.step, ver, field.cutoff) {
  
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
  
  # Make copy for export
  well.actual <- well
  
  
  # Determine which fields to analyze individually --------------------------
  
  # Count number of wells located in each field using well.actual
  wcount <- sqldf("select distinct(w_field_num), count(p_api)
                  from 'well.actual'
                  group by w_field_num")
  
  # Rename columns
  names(wcount) <- c("field", "count")
  
  # Drop any field that has less than field.cutoff fraction of the wells in the
  # Basin.
  wcount <- wcount[which(wcount$count/sum(wcount$count) >= field.cutoff),]
  
  # Finally, extract field numbers from wcount and add in Field 999 as catch-all
  # category.
  field <- c(wcount$field, 999)
  
  
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
  
  
  # Find index of wells in Field 999 ----------------------------------------
  
  # We need to find all rows which are associated with field numbers not 
  # included in the vector "field" for individual analysis. For each field # in 
  # "field" except the catch-all field (999), find which rows in
  # well$w_field_num which match that field and concatonate them together into
  # the vector "ind".
  ind <- NULL
  for (i in 1:(length(field)-1)) {
    temp <- which(well$w_field_num == field[i])
    ind <- c(ind, temp)
  }
  
  # Now generate a sequence of row #s equal to the length of well$w_field_num
  ind999 <- seq(1:length(well$w_field_num))
  
  # Subtract out all elements in "ind" to get row #s of wells located fields
  # other than those listed in vector "field"
  ind999 <- ind999[-ind]
  
  
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
                      paste("cdf_schedule_", ver, ".rda", sep = "")),
       list=c("cdf.ff",
              "cdf.flt",
              "cdf.depth.ow",
              "cdf.depth.gw",
              "prob",
              "field"))
  
  # Save well.actual data.frame
  save(file=file.path(path$data,
                      paste("well_actual_", ver, ".rda", sep = "")),
       list=c("well.actual"))
}