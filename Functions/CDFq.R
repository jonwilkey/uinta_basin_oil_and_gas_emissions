# Function Info -----------------------------------------------------------
# Name:      CDFq.R (Cumulative distribution function via quantile() function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# vector - vector of data to run which density() base function will be run on

# xq - sequence of probability points at which to find quantiles


# Outputs -----------------------------------------------------------------

# data.frame with first column of points giving sequence of values that span 
# range of values in density input (x-values) and second column giving the
# cumulative probability of that point occuring (y-values).


# Description -------------------------------------------------------------

# This function returns the cumulative distribution function of the input vector
# by using the quantile() function.


# Function ----------------------------------------------------------------
CDFq <- function(vector, xq) {
  
  # Get quantiles of vector at probability points given by xq, then merge
  # numerical values in quantile result with xq to make data.frame qdf
  qdf <- data.frame(quantile(vector, probs = xq, names = FALSE, na.rm = TRUE), xq)
  
  # Rename qdf columns as "PDF.x" (for numerical value) and "CDF" for cumulative
  # distr. fun.
  names(qdf) <- c("PDF.x", "CDF")
  
  # Return result
  return(qdf)
}