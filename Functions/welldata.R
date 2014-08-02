### Well Data Simulation Function ###

# Inputs ------------------------------------------------------------------

# nrun - number of iterations in overall simulation

# data_root - location of all prepared CDF files and supporting data

# timesteps - vector of dates comprising timesteps used in simulation

# basis - Consumer price index for desired time to inflation adjust prices to

# field - vector of fields to analyze individually

# schedule.type - character switch for selecting method of determining drilling
# schedule

# production.type - character switch for selecting method of determining decline
# curve coefficients


# Outputs -----------------------------------------------------------------

# wsim - data.table containing all of the randomly generated well data
# information (type, field, decline curve coefficients, depth, surface
# landowner, etc.)


# Description -------------------------------------------------------------

# This function generates all of the randomly selected information about all 
# wells used in the rest of the simulation. The function begins by checking 
# which method has been called for to determine the drilling schedule ("a" -
# simulated schedule, "b" - actual schedule).

# If the drilling schedule is simulated, the function next determines how many 
# wells are drilled in each timestep of each nrun iteration. This vector is then
# expanded so that each well has its own row in the vector "wellID" with a 
# unique well ID# for every well. The wellID is then scanned in conjunction with
# the "Drilled" vector containing the original simulated drilling schedule to 
# determine for each well: (1) the iteration # in nrun it is associated with 
# (runID), and (2) the timestep in that iteration in which it is drilled 
# (tDrill). Next, the simulation randomly determines whether the well is going 
# to be an oil well or gas well. Based on that choice, the function next 
# determines the field number that well is drilled in. Finally, the function
# determines land ownership and well depth.

# Otherwise the drilling schedule to be used is the actual drilling schedule,
# and the function begins by loading the prepared data in actual.wsim and
# repeating it however many times are called for in nrun.

# Next, the function generates decline curve coefficients "a" (initial 
# production rate), "b" (decline exponent), and "c" (initial decline rate) for 
# use with the Arps hyperbolic decline curve equation q = a*(1+b*c*t)^(-1/b). If
# the value of "production.type" == 1 then these values are simulated using the 
# CDFs in the various OOW and GGW decline curve fit *.rda files.

# Otherwise the actual decline curve fits for actual wells are to be used. Note 
# that the decline curve coefficients for each actual well has not been 
# collected. Instead only the maximum production rate from each well has been 
# saved, and this value has been set to == coefficient "a". All other decline 
# curve values are set == NA. As such, production.type should not be set to "2" 
# for selecting the actual decline curve coefficients unless the actual 
# production schedule has also been used. To prevent this, there is no option
# for schedule.type = 1 and production.type = 2; these options will result in a
# function error.

# At this point, most of the information in wsim has either been randomly
# determined or loaded. Regardless of the simulation type the following
# infomration is generated randomly: corporate income tax conversion factors,
# drilling and completion capital costs, and emission factors.

# Finally, the various vectors and matrices are assembled into a data.table and
# formatted for the function return wsim.


# Function ----------------------------------------------------------------
welldata <- function(nrun, data_root, timesteps, basis, field, schedule.type,
                     production.type) {
  
  if (schedule.type == "a") {
    # For simulation drilling schedule, load required PDF and CDF data, generate
    # drilling schedule, pick well type, field location, and depth.
    
    # === Load required files ===
    # CDFs for drilling rates, field assignments, land ownership, and well type
    load(file.path(data_root, "cdf_schedule_v1.rda"))
    
    # Well Depth PDF x-values & CDF y-values
    load(file.path(data_root, "cdf_wellDepth_v1.rda"))
    
    # === Prep CDF data ===
    # Drop field listing from cdf.fsl dataframe and change type to matrix, don't
    # know why but won't work otherwise
    cdf.fsl <- cdf.fsl[,-1]; cdf.fsl <- as.matrix(cdf.fsl)
    
    # === Generate drilling schedule ===
    # Generate number of wells drilled in each timestep
    Drilled <- round(cdf.drill[findInterval(runif(length(timesteps)*nrun),
                                            c(0, cdf.drill[,2])),1])
    
    # Predefine space for results matrix
    wellID <- seq(length.out = sum(Drilled))
    tDrill <- rep(0, times = length(wellID))
    runID  <- tDrill
    
    # Loop counters
    a <- 1 # wellID
    b <- 1 # Tstep
    c <- 1 # runID
    bstop <- length(timesteps)
    
    # Note - this is still slow. Intent is to generate a matrix with rows = each
    # unique well drilled in all simulation runs and columns assigning each well
    # a unique ID # (wellID), the time step it was drilled in (tDrill), and the
    # overall simulation run which it is a part of (runID)
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
    
    # === Pick well type, field, depth, and landownership ===
    # Pick oil or gas well
    type <- ifelse(test = runif(length(wellID)) <= prob.gas,
                   yes = "GW",
                   no = "OW")
    
    # Pick field number
    fieldnum <- ifelse(test = type == "GW",
                       yes = cdf.ffg[findInterval(runif(length(type)),
                                                  c(0, cdf.ffg[,2])), 1],
                       no = cdf.ffo[findInterval(runif(length(type)),
                                                 c(0, cdf.ffo[,2])), 1])
    
    # Predefine vector sizes for well depth and landownership
    depth   <- rep(0, times = length(type))
    landown <- depth
    
    # Pull indices of oil and gas wells
    ind.ow <- which(type == "OW")
    ind.gw <- which(type == "GW")
    
    # Generate total measured depth for each well based on well type
    depth[ind.ow] <- cdf.depth.ow$x[findInterval(runif(length(ind.ow)),
                                                 c(0, cdf.depth.ow$y))]
    depth[ind.gw] <- cdf.depth.gw$x[findInterval(runif(length(ind.gw)),
                                                 c(0, cdf.depth.gw$y))]
    
    # Pick surface landowner
    for (i in 1:length(field)) {
      ind.ow <- which(type == "OW" & fieldnum == field[i])
      landown[ind.ow] <- findInterval(runif(length(ind.ow)),
                                      c(0, cdf.fsl[i,]))
      
      ind.gw <- which(type == "GW" & fieldnum == field[i])
      landown[ind.gw] <- findInterval(runif(length(ind.gw)),
                                      c(0, cdf.fsl[i,]))
    }
  } else {
    # Else function call is for validation against actual DOGM data. Load actual
    # results from analysis in schedule_v1.R and define components to match
    # results of simulation loop above.
    
    # Load actual DOGM well data from schedule_v1.R
    load(file.path(data_root, "wsim_actual_v1.rda"))
    
    # Define components
    wellID   <- rep(wsim.actual$wellID,    times = nrun)
    tDrill   <- rep(wsim.actual$tDrill,    times = nrun)
    type     <- rep(wsim.actual$wellType,  times = nrun)
    fieldnum <- rep(wsim.actual$fieldnum,  times = nrun)
    depth    <- rep(wsim.actual$depth,     times = nrun)
    landown  <- rep(wsim.actual$landOwner, times = nrun)
    
    # If using actual production decline curve coefficients
    if (production.type == "b") {
      acoef    <- rep(wsim.actual$acoef, times = nrun)
      bcoef    <- rep(NA,                times = length(acoef))
      ccoef    <- bcoef
    }
    
    # Define runID
    runID <- rep(1:nrun, each = max(wellID))
  }
  
  if (production.type == "a") {
    # Generate decline curve coefficients if production type == 1
    
    # === Load required files ===
    # PDF x-values & CDF y-values for MC simulation of DC coefficients
    load(file.path(data_root, "pdf_oow.rda"))
    load(file.path(data_root, "cdf_oow.rda"))
    load(file.path(data_root, "pdf_ggw.rda"))
    load(file.path(data_root, "cdf_ggw.rda"))
    
    # === Prep CDF data ===
    # Extract and reorder desired decline curve coefficients for selected fields
    cdf.oow <- cdf.oow[,c(34, 35, 36, 1, 2, 3, 4, 5, 6, 16, 17, 18, 22, 23, 24,
                          28, 29, 30, 25, 26, 27, 7, 8, 9, 19, 20, 21, 13, 14,
                          15, 34, 35, 36)]
    pdf.oow <- pdf.oow[,c(34, 35, 36, 1, 2, 3, 4, 5, 6, 16, 17, 18, 22, 23, 24,
                          28, 29, 30, 25, 26, 27, 7, 8, 9, 19, 20, 21, 13, 14,
                          15, 34, 35, 36)]
    # As placeholder
    cdf.ggw <- cdf.oow
    pdf.ggw <- pdf.oow
    
    # === Generate decline curve coefficients ===
    # Predefine space for coefficient vectors
    acoef <- rep(0, times = length(type))
    bcoef <- acoef
    ccoef <- acoef
    
    # Pick coefficients
    for (i in 1:length(field)) {
      ind.ow <- which(type == "OW" & fieldnum == field[i])
      acoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                            c(0,cdf.oow[,(i-1)*3+1])),(i-1)*3+1]
      bcoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                            c(0,cdf.oow[,(i-1)*3+2])),(i-1)*3+2]
      ccoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                            c(0,cdf.oow[,(i-1)*3+3])),(i-1)*3+3]
      
      ind.gw <- which(type == "GW" & fieldnum == field[i])
      acoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                            c(0,cdf.ggw[,(i-1)*3+1])),(i-1)*3+1]
      bcoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                            c(0,cdf.ggw[,(i-1)*3+2])),(i-1)*3+2]
      ccoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                            c(0,cdf.ggw[,(i-1)*3+3])),(i-1)*3+3]
    }
  }
    
  # === Generate corporate tax rates for each well ===
  # 1) Load corporate income tax data. Note - in 2011 dollars (CPI = 224.939)
  load(file.path(data_root, "cdf_corpIncomeTax_v1.rda"))
  
  # 2) Ajdust to "basis" 2013 dollars from 2011 dollars (CPI = 224.939) by
  #    modifying x-values ($/unit production) in cdf.CI
  cdf.CI$x <- cdf.CI$x*(basis/224.939)
  
  # 3) Pick corporate income tax conversion factors (cirSO - corp income rate
  #    state oil, cirSG - state gas, cirFO - fed oil, cirFG - fed gas).
  cirSO <- cdf.CI$x[findInterval(runif(length(type)),c(0,cdf.CI$ySO))]
  cirSG <- cdf.CI$x[findInterval(runif(length(type)),c(0,cdf.CI$ySG))]
  cirFO <- cdf.CI$x[findInterval(runif(length(type)),c(0,cdf.CI$yFO))]
  cirFG <- cdf.CI$x[findInterval(runif(length(type)),c(0,cdf.CI$yFG))]
    
  # === Drilling and completion capital cost ===
  # Drilling capital cost coefficients in eq. y = exp(a+b*(depth in ft))
  coef.drill <- c(11.46, 0.00024)
  
  # Completion capital cost coefficients in eq. y = exp(a+b*(depth in ft))
  coef.compl <- c(11.217, 0.00022)
  
  # Annual average CPI value for 2011 dollars (base year used for capital costs)
  base.index <- 224.939
  
  # Total capital cost is sum of drilling and capital costs, adjusted for
  # inflation from 2011 dollars to model-year dollars
  cost <- (exp(coef.drill[1]+coef.drill[2]*depth)+
             exp(coef.compl[1]+coef.compl[2]*depth))*(basis/base.index)
  
  # === Emission Factors ====
  # Load GHG Emission Factor CDFs
  load(file.path(data_root, "GHG_v2.rda"))
  
  # Use findInterval to pick index and assign value to each emission factor (EF)
  EF.dcw <- cdf.ghg$dcw.x[findInterval(runif(length(type)), c(0, cdf.ghg$dcw.y))] # Drilling and completion
  EF.prc <- cdf.ghg$prc.x[findInterval(runif(length(type)), c(0, cdf.ghg$prc.y))] # Processing
  EF.tot <- cdf.ghg$tot.x[findInterval(runif(length(type)), c(0, cdf.ghg$tot.y))] # CH4 as % total production
  EF.prd.gas <- cdf.ghg$prd.x[    findInterval(runif(length(type)), c(0, cdf.ghg$prd.y    ))] # Production of gas
  EF.prd.oil <- cdf.ghg$prd.oil.x[findInterval(runif(length(type)), c(0, cdf.ghg$prd.oil.y))] # Production of oil
  EF.trs.gas <- cdf.ghg$trs.x[    findInterval(runif(length(type)), c(0, cdf.ghg$trs.y    ))] # Transporting gas
  EF.trs.oil <- cdf.ghg$trs.oil.x[findInterval(runif(length(type)), c(0, cdf.ghg$trs.oil.y))] # Transporting oil
  EF.trs.unconv <- cdf.ghg$trs.unconv.x[findInterval(runif(length(type)),
                                                     c(0, cdf.ghg$trs.unconv.y))] # CH4 as % total production lost to transportation
  
  # === Format and return as data.table wsim ===
  # Replace landownership #s with strings (switch expression in royalty.R
  # function requires switch to operate on a string, not a numerical value)
  landown[which(landown == 1)] <- "federal"
  landown[which(landown == 2)] <- "indian"
  landown[which(landown == 3)] <- "state"
  landown[which(landown == 4)] <- "fee"
  
  # Make a data table
  wsim <- data.table(wellID, tDrill, runID, type, fieldnum, acoef, bcoef, ccoef,
                     depth, landown, cirSO, cirSG, cirFO, cirFG, cost, EF.dcw,
                     EF.prc, EF.tot, EF.prd.gas, EF.prd.oil, EF.trs.gas,
                     EF.trs.oil, EF.trs.unconv)
  
  # Set/change column names
  setnames(x = wsim,
           old = 1:ncol(wsim),
           new = c("wellID", "tDrill", "runID", "wellType", "fieldnum", "a",
                   "b", "c", "depth", "landOwner", "cirSO", "cirSG", "cirFO",
                   "cirFG", "cost", "EF.dcw", "EF.prc", "EF.tot", "EF.prd.gas",
                   "EF.prd.oil", "EF.trs.gas", "EF.trs.oil", "EF.trs.unconv"))
  
  # Return wsim
  return(wsim)
}