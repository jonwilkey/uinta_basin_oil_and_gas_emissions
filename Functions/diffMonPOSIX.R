# Monthly Difference Calculation

# Inputs ------------------------------------------------------------------

# first - first date

# last - last date


# Outputs -----------------------------------------------------------------

# diffMonPosix - # of months between first and last


# Description -------------------------------------------------------------

# This function returns number of months between first and last


# Function ----------------------------------------------------------------
diffMonPOSIX <- function(first, last) {
  first = as.POSIXlt(first)
  last =  as.POSIXlt(last)
  (12 * last$year + last$mon) - (12 * first$year + first$mon)
}
