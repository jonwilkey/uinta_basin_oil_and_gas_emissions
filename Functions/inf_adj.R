# Inflation Adjustment


# Inputs ------------------------------------------------------------------

# price - a vector of prices at a prior time

# index - the value of an inflation adjustment index at the prior time

# basis - the value of the same inflation adjustment index at the new time


# Outputs -----------------------------------------------------------------

# price_adj - a vector of prices at the new time


# Description -------------------------------------------------------------

# This function performs the following inflation adjustment calculation:

# (price_adj) = (price) * (basis/index)

# where the terms in the above equation follow the definitions given above in
# the input/output section.


# Function ----------------------------------------------------------------
inf_adj <- function (price, index, basis) {
  price_adj <- price * (basis / index)
  return (price_adj)
}