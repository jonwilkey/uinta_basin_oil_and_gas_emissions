# Function Info -----------------------------------------------------------
# Name:      sim_pTaxfrac.R (Simulated Property Tax Fraction Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# times - number of factors to generate

# pTaxRate - vector with mean and standard deviation of PTI expressed as 
# a fraction of revenue from oil and gas sales


# Outputs -----------------------------------------------------------------

# pTaxfrac - property tax fraction (% of revenue paid as property tax)


# Description -------------------------------------------------------------

# This function randomly picks the property tax fraction (% of revenue paid as
# property tax)


# Function ----------------------------------------------------------------

sim_pTaxfrac <- function(times, pTaxRate) {
  
  # Pick property tax fraction (% of revenue paid as property tax)
  pTaxfrac <- rnorm(n =    times,
                    mean = pTaxRate["mean"],
                    sd =   pTaxRate["sd"])
  
  # In case any % is picked that is < 0, set equal to zero
  pTaxfrac <- ifelse(pTaxfrac < 0, yes = 0, no = pTaxfrac)
  
  # Return the wellType vector
  return(pTaxfrac)
}