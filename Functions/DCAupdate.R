# Function Info -----------------------------------------------------------
# Name:      DCAupdate.R (Decline Curve Analysis)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# minProdRec - Minimum number of non-zero production records

# minDayProd - Minimum number of days of a well produced in a given month
# required to include production data point

# diff.bin.cutoff - Minimum production differential on normalized scale required
# to consider a well as being restarted

# bin - Bin size (number of months)

# DCAplot - True/False flag indicating whether or not to print

# n.stopB.min - Any stop points identified that are lower than this value will
# be ignored

# n.startT.search - Look at the top "n" number of production points and pick the
# one with the lowest time value

# b.start.oil - Initial guess value for b coefficient for oil decline curve

# Di.start.oil - Initial guess value for Di coefficient for oil decline curve

# lower.oil - Lower limits for NLS for oil decline curve for (qo, b, Di)
# coefficients

# upper.oil - Upper limits for NLS for oil decline curve for (qo, b, Di)
# coefficients

# b.start.gas - Initial guess value for b coefficient for gas decline curve

# Di.start.gas - Initial guess value for Di coefficient for gas decline curve

# lower.gas - Lower limits for NLS for gas decline curve for (qo, b, Di)
# coefficients

# upper.gas - Upper limits for NLS for gas decline curve for (qo, b, Di)
# coefficients

# field - List of fields to be analyzed individually

# ver - Version number for results version tracking

# path - path names for file directoires (data, plotting, etc.)

# p - production database


# Outputs -----------------------------------------------------------------

# mo - data.frame containing full summary of every well in Basin with first and
# last curve fit results.

# mg - same as above, but for gas.


# Description -------------------------------------------------------------

# This function fits the hyperbolic decline curve function:

# q(t) = qo * (1 + b * Di * t) ^ (-1 / b)

# to all oil and gas production records in the Uinta Basin. The function begins 
# by generating a list of all wells in the basin and sorting them by cumulative 
# production rates. Next, for each field every well in that field has first its 
# oil and then its gas production record examined by the supporting functions 
# hypfit.R and binStartStop.R to (1) identify stop and start points for the 
# first and last decline curve in each production record, and (2) to run the 
# nlsLM() solver on each of these curves (see each of the functions names for 
# additional details). Results from the fits are stored in the matrices ro and 
# rg for oil and gas, respectively. Fit results are then merged back into the 
# listing of unique wells to create the data.frames mo and mg, which give a
# complete set of fit results for each well.


# Function ----------------------------------------------------------------
DCAupdate <- function(minProdRec, minDayProd, diff.bin.cutoff, bin,
                      DCAplot, n.stopB.min, n.startT.search, b.start.oil,
                      Di.start.oil, lower.oil, upper.oil, b.start.gas,
                      Di.start.gas, lower.gas, upper.gas, field, ver, path, p,
                      Cp.start.oil, c1.start.oil, Qlower.oil, Qupper.oil,
                      Cp.start.gas, c1.start.gas, Qlower.gas, Qupper.gas) {
  
  # Internal Debug Variables  -----------------------------------------------
  
#   # Uncomment if running as script
#   minProdRec = opt$minProdRec
#   minDayProd = opt$minDayProd
#   diff.bin.cutoff = opt$diff.bin.cutoff
#   bin = opt$bin
#   DCAplot = opt$DCAplot
#   n.stopB.min = opt$n.stopB.min
#   n.startT.search = opt$n.startT.search
#   b.start.oil = opt$b.start.oil
#   Di.start.oil = opt$Di.start.oil
#   lower.oil = opt$lower.oil
#   upper.oil = opt$upper.oil
#   b.start.gas = opt$b.start.gas
#   Di.start.gas = opt$Di.start.gas
#   lower.gas = opt$lower.gas
#   upper.gas = opt$upper.gas
#   field = opt$field
#   ver = opt$file_ver
#   Cp.start.oil = opt$Cp.start.oil
#   c1.start.oil = opt$c1.start.oil
#   Qlower.oil = opt$Qlower.oil
#   Qupper.oil = opt$Qupper.oil
#   Cp.start.gas = opt$Cp.start.gas
#   c1.start.gas = opt$c1.start.gas
#   Qlower.gas = opt$Qlower.gas
#   Qupper.gas = opt$Qupper.gas
  
  
  # Internal Functions ------------------------------------------------------
  
  # List of functions used in this script to be loaded here
  flst <- file.path(path$fun, c("hypfit.R",
                                "binStartStop.R"))
  
  # Load each function in list then remove temporary file list variables
  for (f in flst) source(f); remove(f, flst)
  
  
  # Subset data -------------------------------------------------------------
  
  ps <- subset(p,
               subset = (time != 0 &
                           (p$h_well_type == "OW" |
                              p$h_well_type == "GW") &
                           p$p_days_prod >= minDayProd),
               select = c("p_api",
                          "p_oil_prod",
                          "p_gas_prod",
                          "p_water_prod",
                          "time",
                          "h_well_type",
                          "h_first_prod",
                          "w_field_num",
                          "w_totcum_oil",
                          "w_totcum_gas",
                          "nrec"))
  
  # Get list of unique wells and order by cumulative oil and gas production
  well <- sqldf("select distinct p_api, w_field_num, w_totcum_oil, w_totcum_gas, nrec
                from ps
                order by w_totcum_oil DESC, w_totcum_gas DESC")
  
  # Add two columns for cumulative production fraction (CPF) of oil and gas
  well$CPFo <- well$w_totcum_oil/sum(well$w_totcum_oil)
  well$CPFg <- well$w_totcum_gas/sum(well$w_totcum_gas)
  
  
  # DCA Fitting - Oil -------------------------------------------------------
  
  # Predefine index of wells to skip for generating list of wells located in
  # Field 999
  ind999 <- NULL
  
  # Predefine results data.frame for oil and gas
  temp <- nrow(well)*2
  ro <- data.frame(api=        as.character(rep(0, times = temp)),
                   qo =        rep(0, times = temp),
                   b =         rep(0, times = temp),
                   Di =        rep(0, times = temp),
                   tdelay =    rep(0, times = temp),
                   fitFirst =  rep(0, times = temp),
                   fitLast =   rep(0, times = temp),
                   skipped =   rep(0, times = temp),
                   failed =    rep(0, times = temp),
                   Cp =        rep(0, times = temp),
                   c1 =        rep(0, times = temp),
                   QfitFirst = rep(0, times = temp),
                   QfitLast =  rep(0, times = temp),
                   Qfailed =   rep(0, times = temp))
  
  # Set initial value for row counters "row1" and "row2"
  row1 <- 1; row2 <- 2
  
  # For each field to be analyzed individually
  for (g in 1:(length(field)-1)) {
    
    # If printing, initialize PDF devices
    if (DCAplot == TRUE) {
      pdf(file.path(path$plot, paste("Field ",
                                     field[g],
                                     " oil DCA ",
                                     ver,
                                     ".pdf",
                                     sep = "")))
    }
    
    # Get row indices of wells located just in field[g]
    apilist <- which(well$w_field_num == field[g])
    
    # For each well locatd in field[g], fit oil production
    for (h in 1:length(apilist)) {
      # Get subset of production records for this individual well
      w <- subset(ps,
                  subset = (p_api == well$p_api[apilist[h]]),
                  select = c("time", "p_oil_prod"))
      
      # Only oil and gas production records which are non-zero
      w <- w[which(w$p_oil_prod > 0),]
      
      # Check - is number of rows in w >= minProdRec requirement?
      if (nrow(w) >= minProdRec) {
        
        # Rename to columns to fit hypfit function requirements
        names(w) <- c("time", "prod")
        
        # Run hypfit function
        ro[row1:row2,] <- hypfit(ws =              w,
                                 bin =             bin,
                                 diff.bin.cutoff = diff.bin.cutoff,
                                 minProdRec =      minProdRec,
                                 api =             well$p_api[apilist[h]],
                                 b.start =         b.start.oil,
                                 Di.start =        Di.start.oil,
                                 lower =           lower.oil,
                                 upper =           upper.oil,
                                 plotFlag =        DCAplot,
                                 type =            "Oil",
                                 n.stopB.min =     n.stopB.min,
                                 n.startT.search = n.startT.search,
                                 Cp.start =        Cp.start.oil,
                                 c1.start =        c1.start.oil,
                                 Qlower =          Qlower.oil,
                                 Qupper =          Qupper.oil)
      } else {
        
        # Skip and note failure
        ro$skipped[row1:row2] <- 1
        
        # Note well's API #
        ro$api[row1:row2] <- well$p_api[apilist[h]]
      }
      
      # Increment row counters (hopefully this is faster than using rbind)
      row1 <- row1+2; row2 <- row2+2
    }
    
    # If printing, close PDF devices
    if (DCAplot == TRUE) {
      dev.off()
    }
    
    # Add indices of wells located in field[g] to the skip list for Field 999
    ind999 <- c(ind999, apilist)
  }
  
  # --- For Field 999 ---
  # If printing, initialize PDF devices
  if (DCAplot == TRUE) {
    pdf(file.path(path$plot, paste("Field ",
                                   field[g+1],
                                   " oil DCA ",
                                   ver,
                                   ".pdf",
                                   sep = "")))
  }
  
  # Row indices are everything which is not in ind999
  apilist <- well$p_api[-ind999]
  
  # For each well locatd in Field 999, fit oil production
  for (h in 1:length(apilist)) {
    # Get subset of production records for this individual well
    w <- subset(ps,
                subset = (p_api == apilist[h]),
                select = c("time", "p_oil_prod"))
    
    # Only oil and gas production records which are non-zero
    w <- w[which(w$p_oil_prod > 0),]
    
    # Check - is number of rows in w >= minProdRec requirement?
    if (nrow(w) >= minProdRec) {
      
      # Rename to columns to fit hypfit function requirements
      names(w) <- c("time", "prod")
      
      # Run hypfit function
      ro[row1:row2,] <- hypfit(ws =              w,
                               bin =             bin,
                               diff.bin.cutoff = diff.bin.cutoff,
                               minProdRec =      minProdRec,
                               api =             apilist[h],
                               b.start =         b.start.oil,
                               Di.start =        Di.start.oil,
                               lower =           lower.oil,
                               upper =           upper.oil,
                               plotFlag =        DCAplot,
                               type =            "Oil",
                               n.stopB.min =     n.stopB.min,
                               n.startT.search = n.startT.search,
                               Cp.start =        Cp.start.oil,
                               c1.start =        c1.start.oil,
                               Qlower =          Qlower.oil,
                               Qupper =          Qupper.oil)
    } else {
      
      # Skip and note failure
      ro$skipped[row1:row2] <- 1
      
      # Note well's API #
      ro$api[row1:row2] <- apilist[h]
    }
    
    # Increment row counters (hopefully this is faster than using rbind)
    row1 <- row1+2; row2 <- row2+2
  }
  
  # If printing, close PDF devices
  if (DCAplot == TRUE) {
    dev.off()
  }
  
  
  # DCA Fitting - Gas -------------------------------------------------------
  
  # Predefine index of wells to skip for generating list of wells located in
  # Field 999
  ind999 <- NULL
  
  # Predefine results data.frame for oil and gas
  temp <- nrow(well)*2
  rg <- data.frame(api=        as.character(rep(0, times = temp)),
                   qo =        rep(0, times = temp),
                   b =         rep(0, times = temp),
                   Di =        rep(0, times = temp),
                   tdelay =    rep(0, times = temp),
                   fitFirst =  rep(0, times = temp),
                   fitLast =   rep(0, times = temp),
                   skipped =   rep(0, times = temp),
                   failed =    rep(0, times = temp),
                   Cp =        rep(0, times = temp),
                   c1 =        rep(0, times = temp),
                   QfitFirst = rep(0, times = temp),
                   QfitLast =  rep(0, times = temp),
                   Qfailed =   rep(0, times = temp))
  
  # Set initial value for row counters "row1" and "row2"
  row1 <- 1; row2 <- 2
  
  # For each field to be analyzed individually
  for (g in 1:(length(field)-1)) {
    
    # If printing, initialize PDF devices
    if (DCAplot == TRUE) {
      pdf(file.path(path$plot, paste("Field ",
                                     field[g],
                                     " gas DCA ",
                                     ver,
                                     ".pdf",
                                     sep = "")))
    }
    
    # Get row indices of wells located just in field[g]
    apilist <- which(well$w_field_num == field[g])
    
    # For each well locatd in field[g], fit oil production
    for (h in 1:length(apilist)) {
      # Get subset of production records for this individual well
      w <- subset(ps,
                  subset = (p_api == well$p_api[apilist[h]]),
                  select = c("time", "p_gas_prod"))
      
      # Only oil and gas production records which are non-zero
      w <- w[which(w$p_gas_prod > 0),]
      
      # Check - is number of rows in w >= minProdRec requirement?
      if (nrow(w) >= minProdRec) {
        
        # Rename to columns to fit hypfit function requirements
        names(w) <- c("time", "prod")
        
        # Run hypfit function
        rg[row1:row2,] <- hypfit(ws =              w,
                                 bin =             bin,
                                 diff.bin.cutoff = diff.bin.cutoff,
                                 minProdRec =      minProdRec,
                                 api =             well$p_api[apilist[h]],
                                 b.start =         b.start.gas,
                                 Di.start =        Di.start.gas,
                                 lower =           lower.gas,
                                 upper =           upper.gas,
                                 plotFlag =        DCAplot,
                                 type =            "Gas",
                                 n.stopB.min =     n.stopB.min,
                                 n.startT.search = n.startT.search,
                                 Cp.start =        Cp.start.gas,
                                 c1.start =        c1.start.gas,
                                 Qlower =          Qlower.gas,
                                 Qupper =          Qupper.gas)
      } else {
        
        # Skip and note failure
        rg$skipped[row1:row2] <- 1
        
        # Note well's API #
        rg$api[row1:row2] <- well$p_api[apilist[h]]
      }
      
      # Increment row counters (hopefully this is faster than using rbind)
      row1 <- row1+2; row2 <- row2+2
    }
    
    # If printing, close PDF devices
    if (DCAplot == TRUE) {
      dev.off()
    }
    
    # Add indices of wells located in field[g] to the skip list for Field 999
    ind999 <- c(ind999, apilist)
  }
  
  # --- For Field 999 ---
  # If printing, initialize PDF devices
  if (DCAplot == TRUE) {
    pdf(file.path(path$plot, paste("Field ",
                                   field[g+1],
                                   " gas DCA ",
                                   ver,
                                   ".pdf",
                                   sep = "")))
  }
  
  # Row indices are everything which is not in ind999
  apilist <- well$p_api[-ind999]
  
  # For each well locatd in Field 999, fit oil production
  for (h in 1:length(apilist)) {
    # Get subset of production records for this individual well
    w <- subset(ps,
                subset = (p_api == apilist[h]),
                select = c("time", "p_gas_prod"))
    
    # Only oil and gas production records which are non-zero
    w <- w[which(w$p_gas_prod > 0),]
    
    # Check - is number of rows in w >= minProdRec requirement?
    if (nrow(w) >= minProdRec) {
      
      # Rename to columns to fit hypfit function requirements
      names(w) <- c("time", "prod")
      
      # Run hypfit function
      rg[row1:row2,] <- hypfit(ws =              w,
                               bin =             bin,
                               diff.bin.cutoff = diff.bin.cutoff,
                               minProdRec =      minProdRec,
                               api =             apilist[h],
                               b.start =         b.start.gas,
                               Di.start =        Di.start.gas,
                               lower =           lower.gas,
                               upper =           upper.gas,
                               plotFlag =        DCAplot,
                               type =            "Gas",
                               n.stopB.min =     n.stopB.min,
                               n.startT.search = n.startT.search,
                               Cp.start =        Cp.start.gas,
                               c1.start =        c1.start.gas,
                               Qlower =          Qlower.gas,
                               Qupper =          Qupper.gas)
    } else {
      
      # Skip and note failure
      rg$skipped[row1:row2] <- 1
      
      # Note well's API #
      rg$api[row1:row2] <- apilist[h]
    }
    
    # Increment row counters (hopefully this is faster than using rbind)
    row1 <- row1+2; row2 <- row2+2
  }
  
  # If printing, close PDF devices
  if (DCAplot == TRUE) {
    dev.off()
  }
  
  
  # Analysis ----------------------------------------------------------------
  
  # Split into first decline curve fits and last decline curve fits
  ro.first <- ro[seq(from = 1, to = nrow(ro)-1, by = 2),]
  rg.first <- rg[seq(from = 1, to = nrow(ro)-1, by = 2),]
  ro.last  <- ro[seq(from = 2, to = nrow(ro), by = 2),]
  rg.last  <- rg[seq(from = 2, to = nrow(ro), by = 2),]
  
  # Drop repeat columns (fitLast and QfitLast for *.first, and tdelay, fitFirst,
  # and QfitFirst for *.last)
  ro.first <- ro.first[,c(-7, -13)]
  rg.first <- rg.first[,c(-7, -13)]
  ro.last  <- ro.last[,c(-5, -6, -12)]
  rg.last  <- rg.last[,c(-5, -6, -12)]
  
  # Change names
  names(ro.first) <- c("api", "qo.1", "b.1", "Di.1", "tdelay", "fit.1", "skip.1", "fail.1", "Cp.1", "c1.1", "Qfit.1", "Qfailed.1")
  names(rg.first) <- c("api", "qo.1", "b.1", "Di.1", "tdelay", "fit.1", "skip.1", "fail.1", "Cp.1", "c1.1", "Qfit.1", "Qfailed.1")
  names(ro.last)  <- c("api", "qo.2", "b.2", "Di.2", "fit.2", "skip.2", "fail.2", "Cp.2", "c1.2", "Qfit.2", "Qfailed.2")
  names(rg.last)  <- c("api", "qo.2", "b.2", "Di.2", "fit.2", "skip.2", "fail.2", "Cp.2", "c1.2", "Qfit.2", "Qfailed.2")
  
  # Merge with well data.frame
  mo <- merge(x = well, y = ro.first, by.x = "p_api", by.y = "api", all.x = TRUE)
  mg <- merge(x = well, y = rg.first, by.x = "p_api", by.y = "api", all.x = TRUE)
  mo <- merge(x = mo, y = ro.last, by.x = "p_api", by.y = "api", all.x = TRUE)
  mg <- merge(x = mg, y = rg.last, by.x = "p_api", by.y = "api", all.x = TRUE)
  
  
  # Export Results ----------------------------------------------------------
  
  # Save fit results
  save(file=file.path(path$data,
                      paste("DCA_fits_", ver, ".rda", sep = "")),
       list=c("mo",
              "mg"))
}