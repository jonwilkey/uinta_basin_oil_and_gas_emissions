# RIMS.R (RIMS II Employment Model)
# Version 1
# 07/10/14
# Jon Wilkey

# RIMS II Employment Calculation Function
RIMS <- function (multiplier, wsim, LOC, nrun) {
    
  # Preallocate investment schedule space
  invest <- matrix(0, nrow = nrun, floor(ncol(LOC)/12))
  
  # Determine spending schedule on annual basis
  for (i in 1:nrun) {
    for (j in 1:ncol(invest)) {
      tstart <- 12*(j-1)+1
      tstop <- 12*(j-1)+12
      ind <- which(wsim$runID == i &
                   wsim$tDrill >= tstart &
                   wsim$tDrill <= tstop)
      invest[i,j] <- sum(wsim$cost[ind])+
        sum(rowSums(LOC[ind,tstart:tstop], na.rm = TRUE), na.rm = TRUE) # This is erroneous. Doesn't include LOCs for wells outside of tstart/tstop
    }
  }
  
  # RIMS II job creation calculation (muliplier * spending in millions of
  # dollars)
  jobs.RIMS <- multiplier*invest/1e6
  
  # Return result
  return(jobs.RIMS)
}