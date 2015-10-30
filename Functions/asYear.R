# Function Info -----------------------------------------------------------
# Name:       asYear.R (Date to year conversion function)
# Author(s):  Jon Wilkey
# Contact:    jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# x - a date or string (following the standard YYYY-MM-DD format) object


# Outputs -----------------------------------------------------------------

# numeric year value


# Description -------------------------------------------------------------

# Converts any date (or string following the date format) into a numeric year
# value.


# Function ----------------------------------------------------------------
as.year <- function(x) {

  # Pull year from date object x and change to numeric
  as.numeric(format(as.Date(x), "%Y"))
}
