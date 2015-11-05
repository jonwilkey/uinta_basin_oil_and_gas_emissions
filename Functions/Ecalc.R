# Function Info -----------------------------------------------------------
# Name:      Ecalc.R (Emissions Calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# osim/gsim - matrix of oil and gas production volumes

# wsim - well information data.table

# tstart - start date of simulation

# edcut - vector of start dates for emissions reductions

# EFred - emissions reductions matrix for NSPS

# MC.tsteps - number of time steps in simulation


# Outputs -----------------------------------------------------------------

# CO2/CH4/VOC - matrix of total CO2e, CH4, and VOcs as a timeseries for
# each well (in metric tons) with rows = wells and columns = timesteps

# rCO2/rCH4/rVOC - same as above, but with reductions applied


# Description -------------------------------------------------------------

# This function calculates total emissions by species from each well as a
# timeseries.


# Function ----------------------------------------------------------------
Ecalc <- function (osim, gsim, wsim, tstart, edcut, EFred, MC.tsteps) {
  
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
    Erework.co2[ind,i] <- wsim$EFrework.co2[ind]
    Erework.ch4[ind,i] <- wsim$EFrework.ch4[ind]
    Erework.voc[ind,i] <- wsim$EFrework.voc[ind]
    
    # Completion events for reworked wells
    Ecompl.co2[ind,i] <- wsim$EFcompl.co2[ind]
    Ecompl.ch4[ind,i] <- wsim$EFcompl.ch4[ind]
    Ecompl.voc[ind,i] <- wsim$EFcompl.voc[ind]
    
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
  Eco2 <- Edrill.co2 + Ecompl.co2 + Erework.co2 + Eprod.co2 + Eproc.co2 + Etransm.co2 + Eoprod.co2 + Eotrans.co2
  Ech4 <- Edrill.ch4 + Ecompl.ch4 + Erework.ch4 + Eprod.ch4 + Eproc.ch4 + Etransm.ch4 + Eoprod.ch4 + Eotrans.ch4
  Evoc <- Edrill.voc + Ecompl.voc + Erework.voc + Eprod.voc + Eproc.voc + Etransm.voc + Eoprod.voc + Eotrans.voc
  
  # Find what fraction of total emissions is attributable to each activity
  fET <- data.frame(co2 = c(sum(Edrill.co2), sum(Ecompl.co2), sum(Erework.co2), sum(Eprod.co2),
                              sum(Eproc.co2), sum(Etransm.co2), sum(Eoprod.co2), sum(Eotrans.co2))/sum(Eco2),
                    ch4 = c(sum(Edrill.ch4), sum(Ecompl.ch4), sum(Erework.ch4), sum(Eprod.ch4),
                              sum(Eproc.ch4), sum(Etransm.ch4), sum(Eoprod.ch4), sum(Eotrans.ch4))/sum(Ech4),
                    voc = c(sum(Edrill.voc), sum(Ecompl.voc), sum(Erework.voc), sum(Eprod.voc),
                              sum(Eproc.voc), sum(Etransm.voc), sum(Eoprod.voc), sum(Eotrans.voc))/sum(Evoc),
                    row.names = c("drill", "compl", "rework", "prod", "proc", "transm", "oprod", "otrans"))
  
  
  # And what fraction is from oil wells (by difference, gas fraction is 1 - oil fraction)
  ind <- which(wsim$wellType == "OW")
  foil <- (sum(Eco2[ind,])+sum(Ech4[ind,])+sum(Evoc[ind,]))/(sum(Eco2)+sum(Ech4)+sum(Evoc))
  
  
  # Part 2: Emission reductions --------------------------------------------
  
  # Emission reductions function
  redfun <- function(v, tstep, red) {
    
    # --- Inputs ---
    # v     - vector being reduced
    # tstep - starting time step / element for reduction
    # red   - actual reduction %
    
    # If the starting time step is less than 2, then the entire vector is
    # reduced
    if (tstep < 2) {
      
      # Reduce entire vector
      v*(1+red)
    } else {
      
      # Only reduce the emissions after the starting time step
      c(v[1:(tstep-1)], v[tstep:length(v)]*(1+red))
    }
  }
  
  # Predefine reduced emissions matrices as their unreduced counterparts
  rEdrill.co2 <-  Edrill.co2
  rEdrill.ch4 <-  Edrill.ch4
  rEdrill.voc <-  Edrill.voc
  rErework.co2 <- Erework.co2
  rErework.ch4 <- Erework.ch4
  rErework.voc <- Erework.voc
  rEcompl.co2 <-  Ecompl.co2
  rEcompl.ch4 <-  Ecompl.ch4
  rEcompl.voc <-  Ecompl.voc
  rEprod.co2 <-   Eprod.co2
  rEprod.ch4 <-   Eprod.ch4
  rEprod.voc <-   Eprod.voc
  rEproc.co2 <-   Eproc.co2
  rEproc.ch4 <-   Eproc.ch4
  rEproc.voc <-   Eproc.voc
  rEtransm.co2 <- Etransm.co2
  rEtransm.ch4 <- Etransm.ch4
  rEtransm.voc <- Etransm.voc
  rEoprod.co2 <-  Eoprod.co2
  rEoprod.ch4 <-  Eoprod.ch4
  rEoprod.voc <-  Eoprod.voc
  rEotrans.co2 <- Eotrans.co2
  rEotrans.ch4 <- Eotrans.ch4
  rEotrans.voc <- Eotrans.voc
  
  
  # --(1)-- Emissions reduction for wells drilled beginning Nov. 2012
  
  # Cut time step equivalent to Nov. 2012 emissions reduction
  ttstep <- 1+round(as.numeric(difftime(EFred$date[1], tstart, units = "days"))*(12/365.25))
  
  # Get row indices of wells drilled on or after Nov. 2012 date (and aren't existing wells)
  ind <- which(wsim$tDrill >= ttstep & wsim$tDrill != 0)
  
  # Get prior wells which may be subject to change. If implementation date
  # occurs before start of simulation...
  if (ttstep < 0) {
    
    # ... then find any well younger than implementation date
    temp <- which(wsim$tend <= abs(ttstep) & wsim$tDrill == 0)
    
    # If there are any such wells
    if (length(temp) > 0) {
      
      # Then add them to row index
      ind <- c(ind, temp)
    }
  }
  
  # If ind > 0, apply reductions to gas production, processing, and transport
  if(length(ind) > 0) {
    
    # Production events for gas
    rEprod.co2[ind,] <- t(apply(Eprod.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "prod", "co2"]))
    rEprod.ch4[ind,] <- t(apply(Eprod.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "prod", "ch4"]))
    rEprod.voc[ind,] <- t(apply(Eprod.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "prod", "voc"]))
    
    # Processing events for gas
    rEproc.co2[ind,] <- t(apply(Eproc.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "proc", "co2"]))
    rEproc.ch4[ind,] <- t(apply(Eproc.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "proc", "ch4"]))
    rEproc.voc[ind,] <- t(apply(Eproc.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "proc", "voc"]))
    
    # Transmission and distribution events for gas
    rEtransm.co2[ind,] <- t(apply(Etransm.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "transm", "co2"]))
    rEtransm.ch4[ind,] <- t(apply(Etransm.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "transm", "ch4"]))
    rEtransm.voc[ind,] <- t(apply(Etransm.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "transm", "voc"]))
  }
  
  # Overall emissions reduction fraction
  temp <- (1 - (sum(Edrill.voc) +
                  sum(Ecompl.voc) +
                  sum(Erework.voc) +
                  sum(rEprod.voc) +
                  sum(rEproc.voc) +
                  sum(rEtransm.voc) +
                  sum(Eoprod.voc) +
                  sum(Eotrans.voc)) /
             sum(Evoc))
  NSPSred <- temp*(fET["prod", "voc"]+fET["proc", "voc"]+fET["transm", "voc"])
  
  
  # --(2)-- Completions
  
  # Cut time step equivalent to Jan 2015 emissions reduction
  ttstep <- 1+round(as.numeric(difftime(EFred$date[4], tstart, units = "days"))*(12/365.25))
  
  # Get row indices of wells drilled on or after Jan. 2015 (and aren't existing wells)
  ind <- which(wsim$tDrill >= ttstep & wsim$tDrill != 0)
  
  # If ind > 0, apply reductions to gas production, processing, and transport
  if(length(ind) > 0) {
    
    # Completion events for all wells
    rEcompl.co2[ind,] <- t(apply(Ecompl.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "compl", "co2"]))
    rEcompl.ch4[ind,] <- t(apply(Ecompl.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "compl", "ch4"]))
    rEcompl.voc[ind,] <- t(apply(Ecompl.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "compl", "voc"]))
  }
  
  # Overall emissions reduction fraction
  temp <- (1 - (sum(Edrill.voc) +
                  sum(rEcompl.voc) +
                  sum(Erework.voc) +
                  sum(Eprod.voc) +
                  sum(Eproc.voc) +
                  sum(Etransm.voc) +
                  sum(Eoprod.voc) +
                  sum(Eotrans.voc)) /
             sum(Evoc))
  NSPSred <- c(NSPSred, temp*(fET["compl", "voc"]))
  
  
  # --(3)-- Drilling construction activity
  
  # Cut time step equivalent to Jan 2015 emissions increase in drilling (related to construction activity)
  ttstep <- 1+round(as.numeric(difftime(EFred$date[5], tstart, units = "days"))*(12/365.25))
  
  # Get row indices of wells drilled on or after Jan. 2015 (and aren't existing wells)
  ind <- which(wsim$tDrill >= ttstep & wsim$tDrill != 0)
  
  # If ind > 0, apply reductions to gas production, processing, and transport
  if(length(ind) > 0) {
    
    # Completion events for all wells
    rEdrill.co2[ind,] <- t(apply(Edrill.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "drill", "co2"]))
    rEdrill.ch4[ind,] <- t(apply(Edrill.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "drill", "ch4"]))
    rEdrill.voc[ind,] <- t(apply(Edrill.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "drill", "voc"]))
  }
  
  # No VOC impact from drilling construction activity
  
  
  # --(4)-- Pneumatic controllers
  
  # Cut time step equivalent to Jan 2015 emissions reduction for pneumatic controllers
  ttstep <- 1+round(as.numeric(difftime(EFred$date[6], tstart, units = "days"))*(12/365.25))
  
  # If the implementation date is effective
  if (ttstep > 0 & ttstep <= MC.tsteps) {
    
    # Then all wells have their VOC/CH4 emissions reduced, with percentages specified by county
    
    # Get row indices of wells drilled on or after Jan. 2015 and located in Uintah County
    ind <- which(wsim$county == "UINTAH")
    
    # If ind > 0, apply reductions to everything
    if(length(ind) > 0) {
      
      # Drilling events
      rEdrill.co2[ind,] <- t(apply(rEdrill.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "co2"]))
      rEdrill.ch4[ind,] <- t(apply(rEdrill.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "ch4"]))
      rEdrill.voc[ind,] <- t(apply(rEdrill.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "voc"]))
      
      # Completion events
      rEcompl.co2[ind,] <- t(apply(rEcompl.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "co2"]))
      rEcompl.ch4[ind,] <- t(apply(rEcompl.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "ch4"]))
      rEcompl.voc[ind,] <- t(apply(rEcompl.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "voc"]))
      
      # Rework events
      rErework.co2[ind,] <- t(apply(rErework.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "co2"]))
      rErework.ch4[ind,] <- t(apply(rErework.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "ch4"]))
      rErework.voc[ind,] <- t(apply(rErework.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "voc"]))
      
      # Production events for gas
      rEprod.co2[ind,] <- t(apply(rEprod.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "co2"]))
      rEprod.ch4[ind,] <- t(apply(rEprod.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "ch4"]))
      rEprod.voc[ind,] <- t(apply(rEprod.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "voc"]))
      
      # Processing events for gas
      rEproc.co2[ind,] <- t(apply(rEproc.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "co2"]))
      rEproc.ch4[ind,] <- t(apply(rEproc.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "ch4"]))
      rEproc.voc[ind,] <- t(apply(rEproc.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "voc"]))
      
      # Transmission and distribution events for gas
      rEtransm.co2[ind,] <- t(apply(rEtransm.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "co2"]))
      rEtransm.ch4[ind,] <- t(apply(rEtransm.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "ch4"]))
      rEtransm.voc[ind,] <- t(apply(rEtransm.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "voc"]))
      
      # Production events for oil
      rEoprod.co2[ind,] <- t(apply(rEoprod.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "co2"]))
      rEoprod.ch4[ind,] <- t(apply(rEoprod.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "ch4"]))
      rEoprod.voc[ind,] <- t(apply(rEoprod.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "voc"]))
      
      # Transport events for oil
      rEotrans.co2[ind,] <- t(apply(rEotrans.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "co2"]))
      rEotrans.ch4[ind,] <- t(apply(rEotrans.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "ch4"]))
      rEotrans.voc[ind,] <- t(apply(rEotrans.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pUintah", "voc"]))
    }
    
    # Get row indices of wells drilled on or after Jan. 2015 and located in DUchesne County
    ind <- which(wsim$county == "DUCHESNE")
    
    # If ind > 0, apply reductions to everything
    if(length(ind) > 0) {
      
      # Drilling events
      rEdrill.co2[ind,] <- t(apply(rEdrill.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "co2"]))
      rEdrill.ch4[ind,] <- t(apply(rEdrill.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "ch4"]))
      rEdrill.voc[ind,] <- t(apply(rEdrill.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "voc"]))
      
      # Completion events
      rEcompl.co2[ind,] <- t(apply(rEcompl.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "co2"]))
      rEcompl.ch4[ind,] <- t(apply(rEcompl.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "ch4"]))
      rEcompl.voc[ind,] <- t(apply(rEcompl.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "voc"]))
      
      # Rework events
      rErework.co2[ind,] <- t(apply(rErework.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "co2"]))
      rErework.ch4[ind,] <- t(apply(rErework.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "ch4"]))
      rErework.voc[ind,] <- t(apply(rErework.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "voc"]))
      
      # Production events for gas
      rEprod.co2[ind,] <- t(apply(rEprod.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "co2"]))
      rEprod.ch4[ind,] <- t(apply(rEprod.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "ch4"]))
      rEprod.voc[ind,] <- t(apply(rEprod.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "voc"]))
      
      # Processing events for gas
      rEproc.co2[ind,] <- t(apply(rEproc.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "co2"]))
      rEproc.ch4[ind,] <- t(apply(rEproc.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "ch4"]))
      rEproc.voc[ind,] <- t(apply(rEproc.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "voc"]))
      
      # Transmission and distribution events for gas
      rEtransm.co2[ind,] <- t(apply(rEtransm.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "co2"]))
      rEtransm.ch4[ind,] <- t(apply(rEtransm.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "ch4"]))
      rEtransm.voc[ind,] <- t(apply(rEtransm.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "voc"]))
      
      # Production events for oil
      rEoprod.co2[ind,] <- t(apply(rEoprod.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "co2"]))
      rEoprod.ch4[ind,] <- t(apply(rEoprod.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "ch4"]))
      rEoprod.voc[ind,] <- t(apply(rEoprod.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "voc"]))
      
      # Transport events for oil
      rEotrans.co2[ind,] <- t(apply(rEotrans.co2[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "co2"]))
      rEotrans.ch4[ind,] <- t(apply(rEotrans.ch4[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "ch4"]))
      rEotrans.voc[ind,] <- t(apply(rEotrans.voc[ind,], 1, redfun, tstep = ttstep, red = EFred[EFred$cat == "pDuchesne", "voc"]))
    }
  }
  
  # Sum everything together by species
  rEco2 <- rEdrill.co2 + rEcompl.co2 + rErework.co2 + rEprod.co2 + rEproc.co2 + rEtransm.co2 + rEoprod.co2 + rEotrans.co2
  rEch4 <- rEdrill.ch4 + rEcompl.ch4 + rErework.ch4 + rEprod.ch4 + rEproc.ch4 + rEtransm.ch4 + rEoprod.ch4 + rEotrans.ch4
  rEvoc <- rEdrill.voc + rEcompl.voc + rErework.voc + rEprod.voc + rEproc.voc + rEtransm.voc + rEoprod.voc + rEotrans.voc
  
  # VOC reductions from pneumatic controllers can be found be difference
  
  # Find what fraction of total emissions is attributable to each activity
  rfET <- data.frame(co2 = c(sum(rEdrill.co2), sum(rEcompl.co2), sum(rErework.co2),  sum(rEprod.co2),
                             sum(rEproc.co2),  sum(rEtransm.co2), sum(rEoprod.co2),  sum(rEotrans.co2))/sum(rEco2),
                     ch4 = c(sum(rEdrill.ch4), sum(rEcompl.ch4), sum(rErework.ch4),  sum(rEprod.ch4),
                             sum(rEproc.ch4),  sum(rEtransm.ch4), sum(rEoprod.ch4),  sum(rEotrans.ch4))/sum(rEch4),
                     voc = c(sum(rEdrill.voc), sum(rEcompl.voc), sum(rErework.voc),  sum(rEprod.voc),
                             sum(rEproc.voc),  sum(rEtransm.voc), sum(rEoprod.voc),  sum(rEotrans.voc))/sum(rEvoc),
                     row.names = c("drill", "compl", "rework", "prod", "proc", "transm", "oprod", "otrans"))
  
  # Part 3: Return results -------------------------------------------------
  
  # Return result
  return(list(CO2 =     Eco2,
              CH4 =     Ech4,
              VOC =     Evoc,
              rCO2 =    rEco2,
              rCH4 =    rEch4,
              rVOC =    rEvoc,
              fET =     fET,
              rfET =    rfET,
              foil =    foil,
              NSPSred = NSPSred))
}