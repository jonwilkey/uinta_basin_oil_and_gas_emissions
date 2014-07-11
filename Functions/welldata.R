# welldata.R (Well Data Simulation Function)
# Version 1
# 07/09/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1 -This function generates the wsim data.table, which includes the drilling
#     schedule, the type of well, field, decline curve coefficients, etc. Script
#     handles both simulation and validation runs.

# Function call
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
      if (b >= bstop) {
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
    acoef <- rep(0, times = length(type))
    bcoef <- acoef
    ccoef <- acoef
    depth <- acoef
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
  
  # === Format and return as data.table wsim ===
  # Replace landownership #s with strings (switch expression in royalty.R
  # function requires switch to operate on a string, not a numerical value)
  landown[which(landown == 1)] <- "federal"
  landown[which(landown == 2)] <- "indian"
  landown[which(landown == 3)] <- "state"
  landown[which(landown == 4)] <- "fee"
  
  # Make a data table
  wsim <- data.table(result, type, fieldnum, acoef, bcoef, ccoef, depth,
                     landown, cirSO, cirSG, cirFO, cirFG, cost)
  
  # Set/change column names
  setnames(x = wsim,
           old = 1:ncol(wsim),
           new = c("wellID", "tDrill", "runID", "wellType", "fieldnum", "a",
                   "b", "c", "depth", "landOwner", "cirSO", "cirSG", "cirFO",
                   "cirFG", "cost"))
  
  # Return wsim
  return(wsim)
}