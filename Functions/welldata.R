### Well Data Simulation Function ###

# Inputs ------------------------------------------------------------------

# nrun - number of iterations in overall simulation

# data_root - location of all prepared CDF files and supporting data

# timesteps - vector of dates comprising timesteps used in simulation

# basis - Consumer price index for desired time to inflation adjust prices to

# field - vector of fields to analyze individually

# calltype - character switch for selecting type of simulation to run
# (simulation, validation, etc.)


# Outputs -----------------------------------------------------------------

# wsim - data.table containing all of the randomly generated well data
# information (type, field, decline curve coefficients, depth, surface
# landowner, etc.)


# Description -------------------------------------------------------------

# This function generates all of the randomly selected information about all 
# wells used in the rest of the simulation. The function begins by determining 
# whether a simulation or validation run has been called (depending on
# "calltype" input).

# If the run is a simulation, the function next determines how many wells are 
# drilled in each timestep of each nrun iteration. This vector is then expanded 
# so that each well has its own row in a matrix with three other columns 
# identifying (1) the unique well ID # for this well, (2) the iteration # in 
# nrun it is associated with (runID), and (3) the timestep in that iteration in 
# which it is drilled (tDrill). Next, the simulation randomly determines whether
# the well is going to be an oil well or gas well. Based on that choice, the 
# function next determines the field number that well is drilled in. Finally,
# all field dependent variables are randomly generated (surface landownership,
# decline curve coefficients, etc.).

# Otherwise the simulation is a validation run, and the function begins by
# loading the prepared data in actual.wsim.

# At this point, most of the information in wsim has either been randomly
# determined or loaded. Regardless of the simulation type the following
# infomration is generated randomly: corporate income tax conversion factors,
# drilling and completion capital costs, and emission factors.

# Finally, the various vectors and matrices are assembled into a data.table and
# formatted for the function return wsim.


# Function ----------------------------------------------------------------
welldata <- function(nrun, data_root, timesteps, basis, field, calltype = "sim") {
  
  # For simulation runs, load required PDF and CDF data, generate drilling
  # schedule, pick well type, field location, decline curve coefficients, etc.
  if (calltype == "sim") {
    
    # === Load required files ===
    # PDF x-values & CDF y-values for MC simulation of DC coefficients
    load(file.path(data_root, "pdf_oow.rda"))
    load(file.path(data_root, "cdf_oow.rda"))
    load(file.path(data_root, "pdf_ggw.rda"))
    load(file.path(data_root, "cdf_ggw.rda"))
    
    # CDFs for drilling rates, field assignments, land ownership, and well type
    load(file.path(data_root, "cdf_schedule_v1.rda"))
    
    # Well Depth PDF x-values & CDF y-values
    load(file.path(data_root, "cdf_wellDepth_v1.rda"))
    
    # === Prep CDF data ===
    # Drop field listing from cdf.fsl dataframe and change type to matrix, don't
    # know why but won't work otherwise
    cdf.fsl <- cdf.fsl[,-1]; cdf.fsl <- as.matrix(cdf.fsl)
    
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
    
    # === Generate drilling schedule ===
    # Generate number of wells drilled in each timestep
    Drilled <- round(cdf.drill[findInterval(runif(length(timesteps)*nrun),
                                            c(0, cdf.drill[,2])),1])
    
    # Predefine space for results matrix
    result <- matrix(0, nrow = sum(Drilled), ncol = 3)
    
    # Loop counters
    a <- 1 # WellID
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
          result[a,1] <- a
          result[a,2] <- b
          result[a,3] <- c
          a <- a+1
        }
      }
      b <- b+1
      if (b > bstop) {
        b <- 1
        c <- c+1
      }
    }
    
    # === Pick well type, field, DC coefficients, depth, and landownership ===
    # Pick oil or gas well
    type <- ifelse(test = runif(nrow(result)) <= prob.gas,
                   yes = "GW",
                   no = "OW")
    
    # Pick field number
    fieldnum <- ifelse(test = type == "GW",
                       yes = cdf.ffg[findInterval(runif(length(type)),
                                                  c(0, cdf.ffg[,2])), 1],
                       no = cdf.ffo[findInterval(runif(length(type)),
                                                 c(0, cdf.ffo[,2])), 1])
    
    # Predefine vector sizes for decline curve coefficients, well depth, and
    # landownership
    acoef   <- rep(0, times = length(type))
    bcoef   <- acoef
    ccoef   <- acoef
    depth   <- acoef
    landown <- acoef
    
    # Pull indices of oil and gas wells
    ind.ow <- which(type == "OW")
    ind.gw <- which(type == "GW")
    
    # Generate total measured depth for each well based on well type
    depth[ind.ow] <- cdf.depth.ow$x[findInterval(runif(length(ind.ow)),
                                                 c(0, cdf.depth.ow$y))]
    depth[ind.gw] <- cdf.depth.gw$x[findInterval(runif(length(ind.gw)),
                                                 c(0, cdf.depth.gw$y))]
    
    # Generate decline curve coefficient values for field and well type
    for (i in 1:length(field)) {
      ind.ow <- which(type == "OW" & fieldnum == field[i])
      acoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                            c(0,cdf.oow[,(i-1)*3+1])),(i-1)*3+1]
      bcoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                            c(0,cdf.oow[,(i-1)*3+2])),(i-1)*3+2]
      ccoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                            c(0,cdf.oow[,(i-1)*3+3])),(i-1)*3+3]
      landown[ind.ow] <- findInterval(runif(length(ind.ow)),
                                      c(0, cdf.fsl[i,]))
      
      ind.gw <- which(type == "GW" & fieldnum == field[i])
      acoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                            c(0,cdf.ggw[,(i-1)*3+1])),(i-1)*3+1]
      bcoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                            c(0,cdf.ggw[,(i-1)*3+2])),(i-1)*3+2]
      ccoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                            c(0,cdf.ggw[,(i-1)*3+3])),(i-1)*3+3]
      landown[ind.gw] <- findInterval(runif(length(ind.gw)),
                                      c(0, cdf.fsl[i,]))
    }
  } else {
    # Else function call is for validation against actual DOGM data. Load actual
    # results from analysis in schedule_v1.R and define components to match
    # results of simulation loop above
    
    # Load actual DOGM well data from schedule_v1.R
    load(file.path(data_root, "wsim_actual_v1.rda"))
    
    # Define components
    result   <- as.matrix(wsim.actual[,1:3])
    type     <- wsim.actual$wellType
    fieldnum <- wsim.actual$fieldnum
    depth    <- wsim.actual$depth
    acoef    <- wsim.actual$acoef
    bcoef    <- rep(NA, times = nrow(result))
    ccoef    <- bcoef
    landown  <- wsim.actual$landOwner
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
  wsim <- data.table(result, type, fieldnum, acoef, bcoef, ccoef, depth,
                     landown, cirSO, cirSG, cirFO, cirFG, cost, EF.dcw, EF.prc,
                     EF.tot, EF.prd.gas, EF.prd.oil, EF.trs.gas, EF.trs.oil,
                     EF.trs.unconv)
  
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