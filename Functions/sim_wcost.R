# Function Info -----------------------------------------------------------
# Name:      sim_wcost.R (Simulated Well Capital Cost Calculator)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# type - character switch indicating if function should be run in new or prior
# well mode

# depth - vector of well depths

# drillCost.fit - lm() regression fit object for drilling costs as a function of
# depth

# complCR - mean and standard deviation of well completion cost ratio
# (completion / drilling)

# rework - vector indicating if well was reworked


# Outputs -----------------------------------------------------------------

# drill - well drilling capital cost

# compl - well completion capital cost


# Description -------------------------------------------------------------

# This function calculates well drilling and completion capital costs


# Function ----------------------------------------------------------------

sim_wcost <- function(type, depth, drillCost.fit, complCR, rework) {
  
  # Calculate directly as cost = exp(a+b*(depth in ft)) where 'a' and 'b' are
  # fitted coefficients from drillCost.fit
  total <- exp(coef(drillCost.fit)["(Intercept)"]+coef(drillCost.fit)["depth"]*depth)
  
  # Next, pick completion cost ratio
  CR <- rnorm(n = length(depth), mean = complCR["mean"], sd = complCR["sd"])
  
  # Redraw any value less than 0
  ind <- 1
  while (length(ind) > 0) {
    ind <- which(CR < 0)
    if (length(ind) > 0) {
      CR[ind] <- rnorm(n = length(ind), mean = complCR["mean"], sd = complCR["sd"])
    }
  }
  
  # Calculate drilling and completion costs from total and CR
  drill <- total/(1+CR)
  compl <- drill*CR
  
  # Check - are costs being calculated for new wells or prior wells?
  switch(type,
         
         # For new wells
         new = {
           
           # Just zero out drilling cost if the well is a rework
           drill <- ifelse(test = is.na(rework),
                           yes =  0,
                           no =   drill)
         },
         
         # For prior wells
         prior = {
           
           # Drilling costs are always zero
           drill <- 0
           
           # Completion costs are only nonzero if rework occurs
           compl <- ifelse(test = rework > 0,
                           yes =  compl,
                           no =   0)
         })
  
  # Return the results
  return(data.frame(cap.drill = drill, cap.compl = compl))
}