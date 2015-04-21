# Function Info -----------------------------------------------------------
# Name:      Ecalc.R (Emissions Calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# osim/gsim - matrix of oil and gas production volumes

# wsim - well information data.table

# tstart - start date of simulation

# edcut - vector of start dates for emissions reductions

# EFred.Nov12 - emissions reductions matrix for NSPS effective for wells drilled
# or reworked beginning Nov. 2012


# Outputs -----------------------------------------------------------------

# CO2/CH4/VOC - matrix of total CO2e, CH4, and VOcs as a timeseries for
# each well (in metric tons) with rows = wells and columns = timesteps

# rCO2/rCH4/rVOC - same as above, but with reductions applied


# Description -------------------------------------------------------------

# This function calculates total emissions by species from each well as a
# timeseries.


# Function ----------------------------------------------------------------
Ecalc <- function (osim, gsim, wsim, tstart, edcut, EFred.Nov12) {
  
  # Preallocate space for each emissions matrix
  Edrill.co2 <-  matrix(0, nrow = nrow(osim), ncol = ncol(osim))
  Edrill.ch4 <-  Edrill.co2
  Edrill.voc <-  Edrill.co2
  Erework.co2 <- Edrill.co2
  Erework.ch4 <- Edrill.co2
  Erework.voc <- Edrill.co2
  Ecompl.co2 <-  Edrill.co2
  Ecompl.ch4 <-  Edrill.co2
  Ecompl.voc <-  Edrill.co2
  Eprod.co2 <-   Edrill.co2
  Eprod.ch4 <-   Edrill.co2
  Eprod.voc <-   Edrill.co2
  Eproc.co2 <-   Edrill.co2
  Eproc.ch4 <-   Edrill.co2
  Eproc.voc <-   Edrill.co2
  Etransm.co2 <- Edrill.co2
  Etransm.ch4 <- Edrill.co2
  Etransm.voc <- Edrill.co2
  Eoprod.co2 <-  Edrill.co2
  Eoprod.ch4 <-  Edrill.co2
  Eoprod.voc <-  Edrill.co2
  Eotrans.co2 <- Edrill.co2
  Eotrans.ch4 <- Edrill.co2
  Eotrans.voc <- Edrill.co2
  
  # Part 1: Full emissions calculation -------------------------------------
  
  # Calculate emissions in each category by timestep
  for (i in 1:ncol(Edrill.co2)) {
    
    # Drilling events
    ind <- which(wsim$tDrill == i)
    Edrill.co2[ind,i] <- wsim$EFdrill.co2[ind]
    Edrill.ch4[ind,i] <- wsim$EFdrill.ch4[ind]
    Edrill.voc[ind,i] <- wsim$EFdrill.voc[ind]
    
    # Completion events for new wells
    Ecompl.co2[ind,i] <- wsim$EFcompl.co2[ind]
    Ecompl.ch4[ind,i] <- wsim$EFcompl.ch4[ind]
    Ecompl.voc[ind,i] <- wsim$EFcompl.voc[ind]
    
    # Rework events
    ind <- which(wsim$rework == i)
    Erework.co2[ind,i] <- wsim$EFrework.co2[ind]*ifelse(wsim$rework[ind] > 0, 1, 0)
    Erework.ch4[ind,i] <- wsim$EFrework.ch4[ind]*ifelse(wsim$rework[ind] > 0, 1, 0)
    Erework.voc[ind,i] <- wsim$EFrework.voc[ind]*ifelse(wsim$rework[ind] > 0, 1, 0)
    
    # Completion events for reworked wells
    Ecompl.co2[ind,i] <- wsim$EFcompl.co2[ind]*ifelse(wsim$rework[ind] > 0, 1, 0)
    Ecompl.ch4[ind,i] <- wsim$EFcompl.ch4[ind]*ifelse(wsim$rework[ind] > 0, 1, 0)
    Ecompl.voc[ind,i] <- wsim$EFcompl.voc[ind]*ifelse(wsim$rework[ind] > 0, 1, 0)
    
    # Production events for gas
    Eprod.co2[,i] <- wsim$EFprod.co2*ifelse(test = (gsim[,i] != 0), yes = 1, no = 0)
    Eprod.ch4[,i] <- wsim$EFprod.ch4*ifelse(test = (gsim[,i] != 0), yes = 1, no = 0)
    Eprod.voc[,i] <- wsim$EFprod.voc*ifelse(test = (gsim[,i] != 0), yes = 1, no = 0)
    
    # Processing events for gas
    Eproc.co2[,i] <- wsim$EFproc.co2*gsim[,i]
    Eproc.ch4[,i] <- wsim$EFproc.ch4*gsim[,i]
    Eproc.voc[,i] <- wsim$EFproc.voc*gsim[,i]
    
    # Transmission and distribution events for gas
    Etransm.co2[,i] <- wsim$EFtrans.co2*gsim[,i]
    Etransm.ch4[,i] <- wsim$EFtrans.ch4*gsim[,i]
    Etransm.voc[,i] <- wsim$EFtrans.voc*gsim[,i]
    
    # Production events for oil
    Eoprod.co2[,i] <- wsim$EFoprod.co2*osim[,i]
    Eoprod.ch4[,i] <- wsim$EFoprod.ch4*osim[,i]
    Eoprod.voc[,i] <- wsim$EFoprod.voc*osim[,i]
    
    # Transport events for oil
    Eotrans.co2[,i] <- wsim$EFotrans.co2*osim[,i]
    Eotrans.ch4[,i] <- wsim$EFotrans.ch4*osim[,i]
    Eotrans.voc[,i] <- wsim$EFotrans.voc*osim[,i]
  }
  
  # Sum everything together by species
  Eco2 <- Edrill.co2+Erework.co2+Ecompl.co2+Eprod.co2+Eproc.co2+Etransm.co2+Eoprod.co2+Eotrans.co2
  Ech4 <- Edrill.ch4+Erework.ch4+Ecompl.ch4+Eprod.ch4+Eproc.ch4+Etransm.ch4+Eoprod.ch4+Eotrans.ch4
  Evoc <- Edrill.voc+Erework.voc+Ecompl.voc+Eprod.voc+Eproc.voc+Etransm.voc+Eoprod.voc+Eotrans.voc
  
  # Part 2: Emission reductions --------------------------------------------
  
  # Step 0: Preliminaries
  
  # Emission reductions function
  redfun <- function(v, tstep, red) {
    
    # --- Inputs ---
    # v     - vector being reduced
    # tstep - starting time step / element for reduction
    # red   - actual reduction %
    
    c(v[1:(tstep-1)], v[tstep:length(v)]*(1+red))
  }
  
  # Predefine reduced emissions matrices as their unreduced counterparts
  rEprod.co2 <-   Eprod.co2
  rEprod.ch4 <-   Eprod.ch4
  rEprod.voc <-   Eprod.voc
  rEproc.co2 <-   Eproc.co2
  rEproc.ch4 <-   Eproc.ch4
  rEproc.voc <-   Eproc.voc
  rEtransm.co2 <- Etransm.co2
  rEtransm.ch4 <- Etransm.ch4
  rEtransm.voc <- Etransm.voc
  
  # Step 1: Emissions reduction for wells drilled beginning Nov. 2012
  
  # Cut time step equivalent to Nov. 2012 emissions reduction
  ttstep <- 1+round(as.numeric(difftime(edcut[1], tstart, units = "days"))*(12/365.25))
  
  # Get row indices of wells drilled on or after Nov. 2012 date
  ind <- which(wsim$tDrill >= ttstep)
  
  # If ind > 0, apply reductions to gas production, processing, and transport
  if(length(ind) > 0) {
    
    # Production events for gas
    rEprod.co2[ind,] <- t(apply(Eprod.co2[ind,], 1, redfun, tstep = ttstep, red = EFred.Nov12["prod","co2"]))
    rEprod.ch4[ind,] <- t(apply(Eprod.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred.Nov12["prod","ch4"]))
    rEprod.voc[ind,] <- t(apply(Eprod.voc[ind,], 1, redfun, tstep = ttstep, red = EFred.Nov12["prod","voc"]))
    
    # Processing events for gas
    rEproc.co2[ind,] <- t(apply(Eproc.co2[ind,], 1, redfun, tstep = ttstep, red = EFred.Nov12["proc","co2"]))
    rEproc.ch4[ind,] <- t(apply(Eproc.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred.Nov12["proc","ch4"]))
    rEproc.voc[ind,] <- t(apply(Eproc.voc[ind,], 1, redfun, tstep = ttstep, red = EFred.Nov12["proc","voc"]))
    
    # Transmission and distribution events for gas
    rEtransm.co2[ind,] <- t(apply(Etransm.co2[ind,], 1, redfun, tstep = ttstep, red = EFred.Nov12["transm","co2"]))
    rEtransm.ch4[ind,] <- t(apply(Etransm.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred.Nov12["transm","ch4"]))
    rEtransm.voc[ind,] <- t(apply(Etransm.voc[ind,], 1, redfun, tstep = ttstep, red = EFred.Nov12["transm","voc"]))
  }
  
  # Step 2: Emissions reductions for completions beginning Jan. 2015
  
  # Step 3: Emissions reductions for pneumatic controllers on production beginning Jan. 2015
  
  # Step 4: Summation
  rEco2 <- Edrill.co2+Erework.co2+Ecompl.co2+rEprod.co2+rEproc.co2+rEtransm.co2+Eoprod.co2+Eotrans.co2
  rEch4 <- Edrill.ch4+Erework.ch4+Ecompl.ch4+rEprod.ch4+rEproc.ch4+rEtransm.ch4+Eoprod.ch4+Eotrans.ch4
  rEvoc <- Edrill.voc+Erework.voc+Ecompl.voc+rEprod.voc+rEproc.voc+rEtransm.voc+Eoprod.voc+Eotrans.voc
  
  # Part 3: Return results -------------------------------------------------
  
  # Return result
  return(list(CO2 =  Eco2,  CH4 =  Ech4,  VOC =  Evoc,
              rCO2 = rEco2, rCH4 = rEch4, rVOC = rEvoc))
}