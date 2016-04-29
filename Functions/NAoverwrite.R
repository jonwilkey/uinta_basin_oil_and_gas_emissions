# Function Info -----------------------------------------------------------
# Name:      NAoverwrite.R (NA Overwrite)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# x - any vector or matrix/data.frame

# replace - value to replace NA with


# Outputs -----------------------------------------------------------------

# y - same object as x, but with any NA values replaced with "replace" value


# Description -------------------------------------------------------------

# This function finds and replaces any NA values with the value specified by
# "replace" (default replacement value is 0)


# Function ----------------------------------------------------------------

NA.overwrite <- function(x, replace = 0) {
  
  # Find and replace function
  fnr <- function(x, r = replace) {
    
    z <- ifelse(test = is.na(x),
                yes =  r,
                no =   x)
  }
  
  # If x is a vector
  if (is.vector(x)) {
    
    # Then run along x
    y <- fnr(x)
    
  } else {
    
    # Make a copy of x
    y <- x
    
    # For each column
    for (i in 1:ncol(x)) {
      
      # Run function on y
      y[,i] <- fnr(x[,i])
    }
  }
  
  # Return result
  return(y)
}