# Function Info -----------------------------------------------------------
# Name:      binStartStop.R (Binning Algorithm for Identifying Start/Stop Points in Decline Curves)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# w - data.frame/matrix with "time" in the first column and production volumes 
# (oil or gas) in the second column. Note that all zero values should be
# excluded (otherwise can't find local minimums)

# bin - size of bin increment for summation (e.g. bin = 6 gives 6 month
# intervals over which months 1-6, 7 - 12, etc. will have their production
# values summed together)

# diff.bin.cutoff - minimum relative % difference between any two bins required
# for well to be considered restarted/reworked

# n.stopB.min - integer value which drops any stopB bin indentified that is less
# than the value of this variable

# n.startT.search - when identifying start points, the script will search for 
# the "n" largest production points inbetween each set of stop points, where "n"
# is the value of this variable. Next, it will select as the start point the
# production value which has the lowest time index value.


# Outputs -----------------------------------------------------------------

# stsp - start/stop point data.frame. Rows are pairs of points, 1st column is 
# start point, 2nd column is stop point, 1st row is first curve, 2nd row is 2nd 
# curve. Note that if only one curve is identified 1st and 2nd rows will be
# identical. Numerical values indicate the row index of w containing the
# start/stop points.


# Description -------------------------------------------------------------

# This function sums the non-zero production history of the given well w into 
# bins, takes the difference in the bin heights, and identifies start/stop 
# points for unique decline curve segments in w, returning a 2x2 data.frame with
# the row indices of the start and stop points of the first decline curve
# segment and the last decline curve segment.


# Function ----------------------------------------------------------------

binStartStop <- function(w, bin, diff.bin.cutoff, n.stopB.min, n.startT.search) {
  
  # Predefine temporary vector for results of binning summation
  temp <- rep(0, times = ceiling(nrow(w)/bin))
  
  # For each interval of "bin" months, get sum of production. Since last bin
  # may vary in size, define summation range as beginning of last bin to the
  # end of the prodution subset.
  for (k in 1:(length(temp)-1)) {
    temp[k] <- sum(w[(bin*(k-1)+1):(bin*k),2])
  }
  temp[length(temp)] <- sum(w[(bin*k+1):(nrow(w)),2])
  
  # Get indices in temp which have a normalized positive diff value larger
  # than cutoff fraction. These bins contain the stop points.
  stopB <- which(diff(temp/max(temp)) >= diff.bin.cutoff)
  
  # Drop stop points found in bins less than n.stopB.min bins into production
  # record. Example: stopB bin identified in 1st bin, if n.stopB.min == 2 then
  # this stopB bin will be omitted since it is less than 2.
  stopB <- stopB[which(stopB >= n.stopB.min)]
  
  # If there are any stopB bins identified
  if (length(stopB) >= 1) {
    # Look for minimum value in each stopbin (limited to non-zero values by
    # previous exclusion of zeroes)
    stopT <- rep(0, times = length(stopB))
    for (m in 1:length(stopB)) {
      stopT[m] <- (bin*(stopB[m]-1)+1)+             # Start of bin index in context of w
                  which.min(w[(bin*(stopB[m]-1)+1): # Search for local minimum index (returns value between 1 and bin size)
                            (bin*stopB[m]),2])-1    # Subtract 1 to adjust local index of which vector to match w
    }
  } else {
    stopT <- NULL
  }
  
  # Add last datapoint as stopT point
  stopT <- c(stopT, nrow(w))
  
  # Define startT vector
  startT <- rep(0, times = length(stopT))
  
  # Next, look for local maximum between stop points - these will be the start 
  # points. The n.startT.search largest production points will be selected from 
  # the production data between the beginning of the last decline curve segment 
  # and the stop point at stopT[n]. The production point in this selection with 
  # the lowest time value (i.e. that gives longest curve) will be selected as
  # the start point.
  t0 <- 1
  for (n in 1:length(stopT)) {
    temp <- w[t0:stopT[n],]
    temp <- temp[order(-temp[,2],temp[,1]),][1:n.startT.search,]
    temp <- min(temp[,1], na.rm = TRUE)
    startT[n] <- which(w[,1] == temp)
    t0 <- stopT[n]
  }
  
  # Return results as matrix with rows = pairs of start/stop points, 1st row = 
  # first curve, 2nd row = last curve, 1st column = start point, and 2nd column
  # = stop point. Values given are  of start/stop points  
  return(data.frame(startT[c(1,length(startT))], stopT[c(1,length(stopT))]))
}