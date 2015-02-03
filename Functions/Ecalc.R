# Function Info -----------------------------------------------------------
# Name:      Ecalc.R (Emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# osim/gsim - matrix of oil and gas production volumes

# wsim - well information data.table


# Outputs -----------------------------------------------------------------

# Eco2/Ech4/Evoc - matrix of total CO2e, CH4, and VOcs as a timeseries for
# each well (in metric tons) with rows = wells and columns = timesteps


# Description -------------------------------------------------------------

# This function calculates total emissions by species from each well as a
# timeseries.


# Function ----------------------------------------------------------------
Ecalc <- function (osim, gsim, wsim) {
  
  # Preallocate space for each emissions matrix
  Edrill.co2 <- matrix(0, nrow = nrow(osim), ncol = ncol(osim))
  Edrill.ch4 <- Edrill.co2
  Edrill.voc <- Edrill.co2
  Eprod.co2 <-  Edrill.co2
  Eprod.ch4 <-  Edrill.co2
  Eprod.voc <-  Edrill.co2
  Eproc.co2 <-  Edrill.co2
  Eproc.ch4 <-  Edrill.co2
  Eproc.voc <-  Edrill.co2
  
  # Calculate emissions in each category by timestep
  for (i in 1:ncol(Edrill.co2)) {
    
    # Drilling events
    ind <- which(wsim$tDrill == i)
    Edrill.co2[ind,i] <- wsim$EFdrill.co2[ind]
    Edrill.ch4[ind,i] <- wsim$EFdrill.ch4[ind]
    Edrill.voc[ind,i] <- wsim$EFdrill.voc[ind]
    
    # Production events
    Eprod.co2[,i] <- wsim$EFprod.co2*ifelse(test = (osim[,i] != 0 | gsim[,i] != 0), yes = 1, no = 0)
    Eprod.ch4[,i] <- wsim$EFprod.ch4*ifelse(test = (osim[,i] != 0 | gsim[,i] != 0), yes = 1, no = 0)
    Eprod.voc[,i] <- wsim$EFprod.voc*ifelse(test = (osim[,i] != 0 | gsim[,i] != 0), yes = 1, no = 0)
    
    # Processing events for gas
    Eproc.co2[,i] <- wsim$EFproc.co2*gsim[,i]
    Eproc.ch4[,i] <- wsim$EFproc.ch4*gsim[,i]
    Eproc.voc[,i] <- wsim$EFproc.voc*gsim[,i]
    
    # Transmission and distribution events for gas
    # nothing for now!
  }
  
  # Summ everything together by species
  Eco2 <- Edrill.co2+Eprod.co2+Eproc.co2
  Ech4 <- Edrill.ch4+Eprod.ch4+Eproc.ch4
  Evoc <- Edrill.voc+Eprod.voc+Eproc.voc
  
  # Return result
  return(list(Eco2, Ech4, Evoc))
}