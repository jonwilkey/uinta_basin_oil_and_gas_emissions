# Function Info -----------------------------------------------------------
# Name:      CDFd.R (Cumulative distribution function using density() function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# vector - vector of data to run which density() base function will be run on

# from - start of density input range

# to - end of density input range

# np - number of points to return density points to, rounded up by density
# function to nearest power of 2


# Outputs -----------------------------------------------------------------

# data.frame with first column of points giving sequence of values that span 
# range of values in density input (x-values) and second column giving the
# cumulative probability of that point occuring (y-values).


# Description -------------------------------------------------------------

# This function returns the cumulative distribution function of the input vector
# by (1) getting the probabilty density function using the base function 
# density(), (2) taking the cumulative sum of the density function output, and 
# (3) normalizing the cumulative sum, so that (4) a data.frame containing the
# probability of each point is returned.


# Function ----------------------------------------------------------------
CDFd <- function(vector, from, to, np) {
  PDF <- density(vector, from = from, to = to, n = np)
  CDF <- cumsum(PDF$y*diff(PDF$x[1:2]))
  CDF <- CDF/max(CDF)
  return(data.frame(PDF$x, CDF))
}