# Function Info -----------------------------------------------------------
# Name:      welldata.R (Well Data Simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# sched.type - Character switch for selecting method of determining drilling.
# Valid options are "a" for simulated schedule, "b" for actual schedule

# Drilled - matrix with rows = MC simulation runs in model and columns =
# timesteps

# timesteps - Number of months to be simulated

# nrun - number of iterations in overall simulation

# field - vector of field numbers to be analyzed individually

# ver - File version

# production.type - Character switch for selecting method of determining decline
# curve coefficients

# basis - Consumer price index at "basis" time index (i.e. CPI value to which
# other costs will be inflation adjusted)

# decline.type - character string switch for determining what decline/production
# curve equation to use (hyperbolic DC or cumulative production)

# EF - emission factors table, set in IO_options.R script


# Outputs -----------------------------------------------------------------

# wsim - data.table containing all of the randomly generated well data
# information (type, field, decline curve coefficients, depth, surface
# lease type, etc.)


# Description -------------------------------------------------------------

# This function generates all of the randomly selected information about all 
# wells used in the rest of the simulation. The function begins by checking 
# which method has been called for to determine the drilling schedule ("a" -
# simulated schedule, "b" - actual schedule).

# If the drilling schedule is simulated, the drilling schedule matrix is
# expanded so that each well has its own row in the vector "wellID" with a
# unique well ID# for every well. The wellID is then scanned in conjunction with
# the "Drilled" vector containing the original simulated drilling schedule to
# determine for each well: (1) the iteration # in nrun it is associated with
# (runID), and (2) the timestep in that iteration in which it is drilled
# (tDrill). Next, the simulation randomly determines the field number (i.e.
# geographic location) for each well. Based on that choice, the function then
# determines well type (dry, oil, or gas well). Finally, the function determines
# lease type and well depth.

# Otherwise the drilling schedule to be used is the actual drilling schedule,
# and the function begins by loading the prepared data in scheduleUpdate.R and
# repeating it however many times are called for in nrun.

# Next, the function generates decline curve coefficients "qo" (initial 
# production rate), "b" (decline exponent), and "Di" (initial decline rate) for 
# use with the Arps hyperbolic decline curve equation q = qo*(1+b*Di*t)^(-1/b). 
# If the value of "production.type" == "a" then these values are simulated using
# the CDFs in the produced by DCAupdate.R. Additionally, the time delay bewteen 
# the first reported production (i.e. drilling completion) and the start of the
# first production decline curve is also randomly selected.

# *** FIX THIS ***
# Otherwise the actual decline curve fits for actual wells are to be used. Note 
# that the decline curve coefficients for each actual well has not been 
# collected. Instead only the maximum production rate from each well has been 
# saved, and this value has been set to == coefficient "a". All other decline 
# curve values are set == NA. As such, production.type should not be set to "2" 
# for selecting the actual decline curve coefficients unless the actual 
# production schedule has also been used. To prevent this, there is no option
# for schedule.type = 1 and production.type = 2; these options will result in a
# function error.

# Next the following information is generated randomly: corporate income tax
# conversion factors, drilling and completion capital costs, emission
# factors, and water balance factors.

# Finally, the various vectors and matrices are assembled into the data.table
# "wsim" which is returned by the function.


# Function ----------------------------------------------------------------
welldata <- function(path, sched.type, Drilled, timesteps, nrun, field, ver,
                     production.type, basis, decline.type, EF) {
  
  # Pick well information that varies by simulation type --------------------
  
  # Switch for method of picking well information that varies by simulation type.
  # Option a simulates all well data. Option b uses actual well information as 
  # reported in DOGM database.
  switch(sched.type,
         a = {
           
           # Load required files -----------------------------------------------
           
           # CDFs ind "cdf_schedule_v*.rda" for:
           # (1) cdf.ff - field assignments
           # (2) cdf.flt - lease type
           # (3) prob - well type (dry, gas, or oil)
           # (4) cdf.depth.xx - well depth based on well type
           load(file.path(path$data, paste("cdf_schedule_", ver, ".rda", sep = "")))
           
           
           # Preprocess CDF data -----------------------------------------------
           
           # Drop field listing from cdf.flt dataframe and change type to
           # matrix, don't know why but won't work otherwise
           cdf.flt <- cdf.flt[,-1]
           cdf.flt <- as.matrix(cdf.flt)
           
           
           # Preprocess drilling schedule --------------------------------------
           
           # Concatonate drilling schedule matrix into single vector
           temp <- NULL
           for (i in 1:nrow(Drilled)) {
             temp <- c(temp, Drilled[i,])
           }
           
           # Reassign temp vector as "Drilled"
           Drilled <- temp
           
           # To prevent possibility of drilling negative wells, search for any
           # values < 0 and set them to zero
           if (min(Drilled) < 0) {
             temp <- which(Drilled < 0)
             Drilled[temp] <- 0
           }
           
           
           # Define wsim matrix and assign wellID #'s --------------------------
           
           # Predefine space for results matrix
           wellID <- seq(length.out = sum(Drilled))
           tDrill <- rep(0, times = length(wellID))
           runID  <- tDrill
           
           # Initialize loop counters
           a <- 1 # wellID
           b <- 1 # Tstep
           c <- 1 # runID
           bstop <- timesteps
           
           # Note - this is still slow. Intent is to generate a matrix with rows
           # = each unique well drilled in all simulation runs and columns
           # assigning each well a unique ID # (wellID), the time step it was
           # drilled in (tDrill), and the overall simulation run which it is a
           # part of (runID)
           for (i in 1:length(Drilled)) {
             if (Drilled[i] > 0) {
               for (j in 1:Drilled[i]) {
                 tDrill[a] <- b
                 runID[a]  <- c
                 a <- a+1
               }
             }
             b <- b+1
             if (b > bstop) {
               b <- 1
               c <- c+1
             }
           }
           
           
           # Pick field number, well type, well depth, and lease type ----------
           
           # Pick field number
           fieldnum <- cdf.ff[findInterval(runif(length(wellID)), c(0, cdf.ff[,2])), 1]
           
           # Pick well type
           type <- rep(0, times = length(wellID))
           for (i in 1:nrow(prob)) {
             ind <- which(fieldnum == prob$field[i])
             dry <- runif(length(ind)) <= prob$dry[i]
             type[ind] <- ifelse(test = dry,
                                 yes = "D",
                                 no = ifelse(test = runif(length(which(dry == FALSE))) <= prob$gas[i],
                                             yes = "GW",
                                             no = "OW"))
           }
           
           # Predefine vector sizes for well depth and lease type
           depth   <- rep(0, times = length(type))
           lease <- depth
           
           # Pull indices of oil and gas wells
           ind.ow <- which(type == "OW")
           ind.gw <- which(type == "GW")
           
           # Generate total measured depth for each well based on well type
           depth[ind.ow] <- cdf.depth.ow$x[findInterval(runif(length(ind.ow)), c(0, cdf.depth.ow$y))]
           depth[ind.gw] <- cdf.depth.gw$x[findInterval(runif(length(ind.gw)), c(0, cdf.depth.gw$y))]
           
           # Pick surface lease (1 - Federal, 2 - Indian, 3 - State, 4 - Fee)
           for (i in 1:length(field)) {
             ind <- which(fieldnum == field[i])
             lease[ind] <- findInterval(runif(length(ind)), c(0, cdf.flt[i,]))
           }
         },
         b = {
           # Function call is for validation against actual DOGM data. Load
           # actual results from analysis in scheduleUpdate.R and define
           # components to match results of simulation loop above.
           
           # Load actual DOGM well data from scheduleUpdate.R
           load(file.path(path$data, paste("wsim_actual_", ver, ".rda", sep = "")))
           
           # Define components
           wellID   <- rep(wsim.actual$wellID,   times = nrun)
           tDrill   <- rep(wsim.actual$tDrill,   times = nrun)
           type     <- rep(wsim.actual$wellType, times = nrun)
           fieldnum <- rep(wsim.actual$fieldnum, times = nrun)
           depth    <- rep(wsim.actual$depth,    times = nrun)
           lease    <- rep(wsim.actual$lease,    times = nrun)
           runID    <- rep(1:nrun, each = max(wellID))
         }
  )
  
  
  # Pick decline curve coefficients and time delay --------------------------
  
  # If production type flag is set to "a", then simulate DCA coefficients here
  if (production.type == "a") {
    
    # Load CDFs for DCA coefficients by field
    load(file.path(path$data, paste("DCA_CDF_coef_", ver, ".rda", sep = "")))
    load(file.path(path$data, paste("Q_DCA_CDF_coef_", ver, ".rda", sep = "")))
    
    # Check - which decline curve method is being used?
    switch(decline.type,
           
           # Hyperbolic DC
           a = {
             
             # Define DCA coefficient vectors and time delay
             qo.oil <- rep(0, times = length(type))
             b.oil  <- qo.oil
             Di.oil <- qo.oil
             td.oil <- qo.oil
             qo.gas <- qo.oil
             b.gas  <- qo.oil
             Di.gas <- qo.oil
             td.gas <- qo.oil
             
             # For each field
             for (i in 1:length(field)) {
               
               # Get indices of wells located in field i
               ind <- which(fieldnum == field[i])
               
               # Pull coefficient CDFs for field i
               cdf.qo.oil <- DCA.cdf.coef.oil[[(i-1)*4+1]]
               cdf.b.oil  <- DCA.cdf.coef.oil[[(i-1)*4+2]]
               cdf.Di.oil <- DCA.cdf.coef.oil[[(i-1)*4+3]]
               cdf.td.oil <- DCA.cdf.coef.oil[[(i-1)*4+4]]
               cdf.qo.gas <- DCA.cdf.coef.gas[[(i-1)*4+1]]
               cdf.b.gas  <- DCA.cdf.coef.gas[[(i-1)*4+2]]
               cdf.Di.gas <- DCA.cdf.coef.gas[[(i-1)*4+3]]
               cdf.td.gas <- DCA.cdf.coef.gas[[(i-1)*4+4]]
               
               # Pick values for each coefficient
               qo.oil[ind] <- cdf.qo.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.qo.oil$CDF))]
               b.oil[ind]  <- cdf.b.oil$PDF.x[ findInterval(runif(length(ind)),c(0,cdf.b.oil$CDF ))]
               Di.oil[ind] <- cdf.Di.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Di.oil$CDF))]
               td.oil[ind] <- cdf.td.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.td.oil$CDF))]
               qo.gas[ind] <- cdf.qo.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.qo.gas$CDF))]
               b.gas[ind]  <- cdf.b.gas$PDF.x[ findInterval(runif(length(ind)),c(0,cdf.b.gas$CDF ))]
               Di.gas[ind] <- cdf.Di.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Di.gas$CDF))]
               td.gas[ind] <- cdf.td.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.td.gas$CDF))]
             }
           },
           
           # Cumulative DC
           b = {
             
             # Define DCA coefficient vectors and time delay
             Cp.oil <- rep(0, times = length(type))
             c1.oil <- Cp.oil
             Cp.gas <- Cp.oil
             c1.gas <- Cp.oil
             td.oil <- Cp.oil
             td.gas <- Cp.oil
             
             # For each field
             for (i in 1:length(field)) {
               
               # Get indices of wells located in field i
               ind <- which(fieldnum == field[i])
               
               # Pull coefficient CDFs for field i
               cdf.td.oil <- DCA.cdf.coef.oil[[(i-1)*4+4]]
               cdf.td.gas <- DCA.cdf.coef.gas[[(i-1)*4+4]]
               cdf.Cp.oil <- Q.DCA.cdf.coef.oil[[(i-1)*2+1]]
               cdf.c1.oil <- Q.DCA.cdf.coef.oil[[(i-1)*2+2]]
               cdf.Cp.gas <- Q.DCA.cdf.coef.gas[[(i-1)*2+1]]
               cdf.c1.gas <- Q.DCA.cdf.coef.gas[[(i-1)*2+2]]
               
               # Pick values for each coefficient
               td.oil[ind] <- cdf.td.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.td.oil$CDF))]
               td.gas[ind] <- cdf.td.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.td.gas$CDF))]
               Cp.oil[ind] <- cdf.Cp.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Cp.oil$CDF))]
               c1.oil[ind] <- cdf.c1.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.c1.oil$CDF))]
               Cp.gas[ind] <- cdf.Cp.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Cp.gas$CDF))]
               c1.gas[ind] <- cdf.c1.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.c1.gas$CDF))]
             }
           })
    
      # Round time delay values to nearest month
      td.oil <- round(td.oil)
      td.gas <- round(td.gas)
    }
  
  
  # Pick NTI conversion factors for each well -------------------------------
  
  # Load corporate income tax data
  load(file.path(path$data, paste("corpNTIfrac_", ver, ".rda", sep = "")))
  
  # Pick net taxable income (NTI) fraction (% of revenue which is NTI)
  NTIfrac <- rnorm(n =    length(type),
                   mean = corpNTIfrac["mean"],
                   sd =   corpNTIfrac["sd"])
  
  # In case any % is picked that is < 0, set equal to zero
  NTIfrac <- ifelse(NTIfrac < 0, yes = 0, no = NTIfrac)
  
  
  # Pick PTI conversion factors for each well -------------------------------
  
  # Load property tax data
  load(file.path(path$data, paste("pTaxRate_", ver, ".rda", sep = "")))
  
  # Pick property tax fraction (% of revenue paid as property tax)
  pTaxfrac <- rnorm(n =    length(type),
                    mean = pTaxRate["mean"],
                    sd =   pTaxRate["sd"])
  
  # In case any % is picked that is < 0, set equal to zero
  pTaxfrac <- ifelse(pTaxfrac < 0, yes = 0, no = pTaxfrac)
  
  
  # Calculate drilling and completion capital cost --------------------------
  
  # ***REPLACE ME***
  
  # Drilling capital cost coefficients in eq. y = exp(a+b*(depth in ft))
  coef.drill <- c(11.46, 0.00024)
  
  # Completion capital cost coefficients in eq. y = exp(a+b*(depth in ft))
  coef.compl <- c(11.217, 0.00022)
  
  # Annual average CPI value for 2011 dollars (base year used for capital costs)
  base.index <- 224.939
  
  # Total capital cost is sum of drilling and capital costs, adjusted for
  # inflation from 2011 dollars to model-year dollars
  cost <- inf_adj(price = (exp(coef.drill[1]+coef.drill[2]*depth)+
                             exp(coef.compl[1]+coef.compl[2]*depth)),
                  index = base.index,
                  basis = basis)
  
  
  # Pick emission factors ---------------------------------------------------
  
  # Hardcoded - sorry!
  
  # For each species that has an emission factor
  for (i in 1:3) {
    
    # Predefine temporary EF matrix
    temp.EF <- matrix(0, nrow = length(type), ncol = nrow(EF))
    
    # For each EF category
    for (j in 1:nrow(EF)) {
      
      # If mean of EF for species i in category j is nonzero
      if(EF[j,i] != 0) {
        
        # Pick EFs using rnorm()
        temp.EF[,j] <- rnorm(n = length(type), mean = EF[j,i], sd = EF[j,(i+3)])
        
        # Rewrite negative EF values as zero
        temp.EF[,j] <- ifelse(temp.EF[,j] < 0, 0, temp.EF[,j])
      }
    }
    
    # If i = 1, working on CO2e
    if(i == 1) {
      
      # Save out EFs for CO2e
      EFdrill.co2  <- rowSums(temp.EF[,1:3])+rowSums(temp.EF[,5:7])
      EFrework.co2 <- temp.EF[,4]
      EFprod.co2   <- temp.EF[,8]
      EFproc.co2   <- temp.EF[,9]
      EFtrans.co2  <- temp.EF[,10]
    }
    
    # If i = 2, working on CH4
    if(i == 2) {
      
      # Save out EFs for CH4
      EFdrill.ch4  <- rowSums(temp.EF[,1:3])+rowSums(temp.EF[,5:7])
      EFrework.ch4 <- temp.EF[,4]
      EFprod.ch4   <- temp.EF[,8]
      EFproc.ch4   <- temp.EF[,9]
      EFtrans.ch4  <- temp.EF[,10]
    }
    
    # If i = 3, working on VOCs
    if(i == 3) {
      
      # Save out EFs for VOCs
      EFdrill.voc  <- rowSums(temp.EF[,1:3])+rowSums(temp.EF[,5:7])
      EFrework.voc <- temp.EF[,4]
      EFprod.voc   <- temp.EF[,8]
      EFproc.voc   <- temp.EF[,9]
      EFtrans.voc  <- temp.EF[,10]
    }
  }
  
  
  # Pick water balance terms based on CDFs ----------------------------------
  
  # Load Water Balance CDFs
  load(file.path(path$data, "water_models.rda"))
  
  # Pick amount of water used for fracking each well (bbl)
  frack <- ifelse(test = type == "OW",
                  yes = fw.ow.cdf$frackwater[findInterval(runif(length(type)), c(0, fw.ow.cdf$CDF))],
                  no = fw.gw.cdf$frackwater[findInterval(runif(length(type)), c(0, fw.gw.cdf$CDF))])
  
  # Pick flooding ratio (bbl water for flooding : bbl oil produced)
  flood <- flood.cdf$ratio[findInterval(runif(length(type)), c(0, flood.cdf$CDF))]
  
  
  
  # Format and return as data.table wsim ------------------------------------
  
  # Replace lease type #s with strings (switch expression in royalty.R function
  # requires switch to operate on a string, not a numerical value)
  lease[which(lease == 1)] <- "federal"
  lease[which(lease == 2)] <- "indian"
  lease[which(lease == 3)] <- "state"
  lease[which(lease == 4)] <- "fee"
  
  # Build wsim data.table out of possible component columns. The following
  # columns always exist
  wsim <- data.table(wellID,
                     tDrill,
                     runID,
                     type,
                     fieldnum,
                     depth,
                     lease,
                     NTIfrac,
                     pTaxfrac,
                     cost,
                     frack,
                     flood,
                     EFdrill.co2,
                     EFrework.co2,
                     EFprod.co2,
                     EFproc.co2,
                     EFtrans.co2,
                     EFdrill.ch4,
                     EFrework.ch4,
                     EFprod.ch4,
                     EFproc.ch4,
                     EFtrans.ch4,
                     EFdrill.voc,
                     EFrework.voc,
                     EFprod.voc,
                     EFproc.voc,
                     EFtrans.voc)
  
  # The following columns may or may not exist
  if(exists("qo.oil") == TRUE) {wsim$qo.oil <- qo.oil}
  if(exists("b.oil")  == TRUE) {wsim$b.oil  <- b.oil}
  if(exists("Di.oil") == TRUE) {wsim$Di.oil <- Di.oil}
  if(exists("td.oil") == TRUE) {wsim$td.oil <- td.oil}
  if(exists("qo.gas") == TRUE) {wsim$qo.gas <- qo.gas}
  if(exists("b.gas")  == TRUE) {wsim$b.gas  <- b.gas}
  if(exists("Di.gas") == TRUE) {wsim$Di.gas <- Di.gas}
  if(exists("td.gas") == TRUE) {wsim$td.gas <- td.gas}
  if(exists("Cp.oil") == TRUE) {wsim$Cp.oil <- Cp.oil}
  if(exists("c1.oil") == TRUE) {wsim$c1.oil <- c1.oil}
  if(exists("Cp.gas") == TRUE) {wsim$Cp.gas <- Cp.gas}
  if(exists("c1.gas") == TRUE) {wsim$c1.gas <- c1.gas}
  
  # Change some column names
  setnames(wsim, "type", "wellType")
  
  # Return wsim
  return(wsim)
}