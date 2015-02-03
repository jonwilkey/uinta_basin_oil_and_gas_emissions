# Function Info -----------------------------------------------------------
# Name:      propertyTaxUpdate.R (Property Tax Conversion Factor Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# p - DOGM database with subsetting to select just Uintah & Duchesne county
# wells

# path - list object containing directory paths for file I/O

# basis - CPI index value for dollar basis used in rest of model

# ver - version # for numbering *.rda output with unique filename

# PTI - property tax data.frame with total property taxes paid in Uintah and
# Duchesne counties given by year in non-inflation adjusted dollars

# eia.hp - data.frame of oil and gas FPP inflation adjusted to basis $/bbl or
# $/MCF each month since 1977


# Outputs -----------------------------------------------------------------

# pTaxRate - data.frame with mean and standard deviation of property tax rates 
# expressed as a fraction of revenue from oil and gas sales for use with rnorm()
# function to randomly pick property tax rates in MC simulation


# Description -------------------------------------------------------------

# The function below gets the mean and standard deviation for property tax
# conversion factors and is intended to be run when new property tax data 
# becomes available.

# The function first determines to the total oil and gas production in Uintah 
# and Duchesne counties for each year where tax data is available, as well as
# the mean oil and gas prices in those years.

# Next, the function determines the revenue from oil and gas sales. Assuming 
# that property tax revenue can be estimated as a fraction of revenue, the 
# function determines the mean and standard deviation of the sample set of 
# property tax data. These two values are then saved so that they can be used in
# the MC simulation for randomly picking property tax rates using the rnorm()
# function.


# Function ----------------------------------------------------------------

propertyTaxUpdate <- function(p, path, basis, ver, PTI, eia.hp, cf.MCF.to.MMBtu) {
  
  # # Internal variables - uncomment to debug ---------------------------------
  # 
  # basis <- opt$cpi
  # ver <- opt$file_ver
  # PTI <- opt$PTI
  
  
  # Get production and mean prices ------------------------------------------
  
  # Predefine space for annual oil/gas summation, mean energy prices, and mean
  # CPI
  prod.oil  <- rep(0, times = nrow(PTI))
  prod.gas  <- prod.oil
  price.oil <- prod.oil
  price.gas <- prod.oil
  
  # Loop for getting data for each year in NTI
  for (i in 1:nrow(PTI)) {
    
    # Get start and stop dates from PTI
    tstart <- as.Date(paste(PTI$year[i],"-01-01",sep=""))
    tstop  <- as.Date(paste(PTI$year[i],"-12-01",sep=""))
    
    # Get subset of p between start/stop dates
    temp <- subset(p, subset = (p_rpt_period >= tstart &
                                  p_rpt_period <= tstop))
    
    # Oil & Gas production summation
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
  
  # Inflation adjust PTI values to basis $
  PTI$PTI <- inf_adj(price = PTI$PTI,
                     index = PTI$cpi,
                     basis = basis)
  
  # Calculate fraction of revenue paid in property taxes each year
  frac <- PTI$PTI/revenue
  
  # Get mean and standard deviation so that rnorm() can be used to pick conversion
  # factor for property tax payments
  pTaxRate <- c(mean(frac), sd(frac))
  names(pTaxRate) <- c("mean", "sd")
  
  
  # Export results as *.rda file --------------------------------------------
  
  save(file = file.path(path$data,
                        paste("pTaxRate_", ver, ".rda", sep = "")),
       list = c("pTaxRate"))
}