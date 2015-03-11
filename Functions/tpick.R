# Function Info -----------------------------------------------------------
# Name:      tpick.R (Time step picking function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# p - logical value indicating whether or not to choose the global time step
# value (T) or the function specific time step value (F)

# g - date object containing the global time step value

# f - date object containing the function specific time step value


# Outputs -----------------------------------------------------------------

# Returns either g or f, depending on choice p


# Description -------------------------------------------------------------

# This is just a simple function handle the ifelse structure of picking either 
# the global time step values or the function specific time step values in a
# given function call.


# Function ----------------------------------------------------------------
tpick <- function(p, g, f) {
  
  # If p is true, use global value
  if (p == T) {
    return(g)
  } else {
    
    # Else use function specific value
    return(f)
  }
}