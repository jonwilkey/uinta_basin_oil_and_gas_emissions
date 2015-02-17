# Function Info -----------------------------------------------------------
# Name:      corpIncomeUpdate.R (Corporate Income Tax Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# production - full DOGM database (without subsetting out wells not located in 
# Uinta Basin)

# path - list object containing directory paths for file I/O

# basis - CPI index value for dollar basis used in rest of model

# ver - version # for numbering *.rda output with unique filename

# NTI - net corporate income tax received by Utah State Tax Commission, 
# structured as data.frame with columns for year and NTI

# eia.hp - data.frame of oil and gas FPP inflation adjusted to basis $/bbl or
# $/MCF each month since 1977


# Outputs -----------------------------------------------------------------

# corpNTIfrac - data.frame with mean and standard deviation of NTI expressed as
# a fraction of revenue from oil and gas sales for use with rnorm() function to
# randomly pick property tax rates in MC simulation


# Description -------------------------------------------------------------

# The function below gets the mean and standard deviation for NTI conversion
# factors and is intended to be run when new corporate tax data becomes
# available.

# The function first determines to the total oil and gas production in the 
# entire state for each year where tax data is available, as well as the mean
# oil and gas prices in those years.

# Next, the function determines the revenue from oil and gas sales. Assuming 
# that NTI can be estimated as a fraction of revenue, the function determines 
# the mean and standard deviation of the sample set of NTI corporate income tax 
# data. These two values are then saved so that they can be used in the MC 
# simulation for randomly picking NTI fractions using the rnorm() function.


# Function ----------------------------------------------------------------

corpIncomeUpdate <- function(production, path, basis, ver, NTI, eia.hp) {
  
#   # Internal variables - uncomment to debug ---------------------------------
#   
#   basis <- opt$cpi
#   ver <- opt$file_ver
#   NTI <- opt$NTI
  
  
  # Oil and Gas Data Selection ----------------------------------------------
  
  # Predefine space for annual oil/gas summation, mean energy prices, and mean
  # CPI
  prod.oil  <- rep(0, times = nrow(NTI))
  prod.gas  <- prod.oil
  price.oil <- prod.oil
  price.gas <- prod.oil
  
  # Loop for getting data for each year in NTI
  for (i in 1:nrow(NTI)) {
    
    # Oil & Gas production summation
    tstart <- as.Date(paste(NTI$year[i],"-01-01",sep=""))
    tstop  <- as.Date(paste(NTI$year[i],"-12-01",sep=""))
    temp <- subset(production, subset = (p_rpt_period >= tstart &
                                         p_rpt_period <= tstop))
    prod.oil[i] <- sum(temp$p_oil_prod, na.rm = TRUE)
    prod.gas[i] <- sum(temp$p_gas_prod, na.rm = TRUE)
    
    # Get row index selection from eia.hp for year "i"
    ind <- which(eia.hp$month >= as.yearmon(tstart) &
                 eia.hp$month <= as.yearmon(tstop))
    
    # Mean oil/gas prices by year
    price.oil[i] <- mean(eia.hp$OP[ind])  # Oil price
    price.gas[i] <- mean(eia.hp$GP[ind])  # Gas price
  }
  
  
  # Calculation -------------------------------------------------------------
  
  # Calculate revenue from oil and gas sales
  revenue <- price.oil*prod.oil+price.gas*prod.gas
  
  # Inflation adjust NTI values to basis $
  NTI$NTI <- inf_adj(price = NTI$NTI,
                     index = NTI$cpi,
                     basis = basis)
  
  # Calculate fraction of revenue which becomes NTI
  frac <- NTI$NTI/revenue
  
  # Get mean and standard deviation so that rnorm() can be used to pick
  # conversion factor for property tax payments
  corpNTIfrac <- c(mean(frac), sd(frac))
  names(corpNTIfrac) <- c("mean", "sd")
    
  
  # Export results as *.rda file --------------------------------------------
  
  save(file=file.path(path$data,
                      paste("corpNTIfrac_", ver, ".rda", sep = "")),
       list=c("corpNTIfrac"))
}