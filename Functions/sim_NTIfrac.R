# Function Info -----------------------------------------------------------
# Name:      sim_NTIfrac.R (Simulated Net Taxable Income Fraction Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# times - number of factors to generate

# corpNTIfrac - vector with mean and standard deviation of NTI expressed as 
# a fraction of revenue from oil and gas sales


# Outputs -----------------------------------------------------------------

# NTIfrac - net taxable income (NTI) fraction (% of revenue which is NTI)


# Description -------------------------------------------------------------

# This function randomly picks the net taxable income (NTI) fraction (% of
# revenue which is NTI)


# Function ----------------------------------------------------------------

sim_NTIfrac <- function(times, corpNTIfrac) {
  
  # Pick net taxable income (NTI) fraction (% of revenue which is NTI)
  NTIfrac <- rnorm(n =    times,
                   mean = corpNTIfrac["mean"],
                   sd =   corpNTIfrac["sd"])
  
  # In case any % is picked that is < 0, set equal to zero
  NTIfrac <- ifelse(NTIfrac < 0, yes = 0, no = NTIfrac)
  
  # Return the wellType vector
  return(NTIfrac)
}