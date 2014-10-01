# Function Info -----------------------------------------------------------
# Name:      corpIncomeUpdate.R (Corporate Income Tax Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# production - full DOGM proddata database (or merged database)

# NTI - net corporate income tax received by Utah State Tax Commission,
#       structured as data.frame with columns for year and NTI

# CIrate.state - State corporate income tax rate

# CIrate.fed - Federal corporate income tax rate

# basis - CPI index value for dollar basis used in rest of model

# CI.pdf.min - Minimum value of CI conversion factor PDF

# CI.pdf.max - Maximum value of CI conversion factor PDF

# version - version # for numbering *.rda output with unique filename

# path - list object containing directory paths for file I/O


# Outputs -----------------------------------------------------------------

# cdf.CI - data.frame with columns for (1) conversion factor of $/bbl or $/MCF
#          corporate income tax in basis dollar paid to the state or federal
#          government on oil or gas production along with the cumulative
#          probability density for each conversion factor rate


# Description -------------------------------------------------------------

# The function below updates the cumulative distribution function for corporate 
# income tax conversion factors and is intended to be run when new corporate 
# income tax data becomes available.

# The function first determines to the total oil and gas production across the 
# entire state for each year where tax data is available, as well as the mean
# oil and gas prices in those years and the mean CPI index value.

# Next, the determines the net revenue from oil and gas sales to attribute the 
# portion of revenue attributable to oil and to gas sales. Assuming tax revenue 
# is proportional to sales revenue, the script then splits revenue to oil and 
# gas on a $/bbl and $/MCF basis based on actually tax payments to the state of 
# UT. Further assuming corporate income tax rates are as state in the input file
# (5% for state and 35% for federal), the federal corporate income tax 
# conversion factor can be inferred. All of these coversion factors are bundled
# into a single data.frame and adjusted for inflation to "basis" dollars.

# Finally, the conversion factors data points are used to generate a probability
# distribution function assuming a normal distribution (since there are only a
# couple of years of tax data available).

# Note that this approach is very rough, and tax data vary widely year-to-year.
# Predictions from this method should be taken with a large grain of salt.


# Function ----------------------------------------------------------------

corpIncomeUpdate <- function(production, NTI, CIrate.state, CIrate.fed, basis,
                             CI.pdf.min, CI.pdf.max, version, path) {
  
  # Oil and Gas Data Selection ----------------------------------------------
  
  # Predefine space for annual oil/gas summation, mean energy prices, and mean
  # CPI
  prod.oil  <- rep(0, times = nrow(NTI))
  prod.gas  <- prod.oil
  price.oil <- prod.oil
  price.gas <- prod.oil
  cpi       <- prod.oil
  
  # Loop for getting data for each year in NTI
  for (i in 1:nrow(NTI)) {
    
    # Oil & Gas production summation
    tstart <- as.Date(paste(NTI$year[i],"-01-01",sep=""))
    tstop  <- as.Date(paste(NTI$year[i],"-12-01",sep=""))
    temp <- subset(production, subset = (p_rpt_period >= tstart &
                                           p_rpt_period <= tstop))
    prod.oil[i] <- sum(temp$p_oil_prod, na.rm = TRUE)
    prod.gas[i] <- sum(temp$p_gas_prod, na.rm = TRUE)
    
    # OGprice row index selection for year "i"
    ind <- which(OGprice$t >= tstart &
                   OGprice$t <= tstop)
    
    # Mean ...
    price.oil[i] <- mean(OGprice$bw[ind])   # Oil price
    price.gas[i] <- mean(OGprice$uswp[ind]) # Gas price
    cpi[i] <- mean(OGprice$cpi[ind])        # CPI
  }
  
  
  # Calculation -------------------------------------------------------------
  
  # Total revenue
  rev.oil <- price.oil*prod.oil
  rev.gas <- price.gas*prod.gas
  
  # Fraction revenue attributable to each resource
  frac.oil <- rev.oil/(rev.oil+rev.gas)
  frac.gas <- 1-frac.oil
  
  # State income tax
  state <- CIrate.state*NTI$NTI
  
  # Federal income tax
  fed <- CIrate.fed*NTI$NTI-state
  
  # Put everything on $ per unit of production basis ($/bbl oil, $/MCF gas)
  state.oil <- frac.oil*state/prod.oil
  state.gas <- frac.gas*state/prod.gas
  fed.oil   <- frac.oil*fed/prod.oil
  fed.gas   <- frac.gas*fed/prod.gas
  
  # Make data.frame and rename
  CI <- data.frame(NTI$year, cpi, state.oil, state.gas, fed.oil, fed.gas)
  names(CI) <- c("year", "cpi", "state.oil", "state.gas", "fed.oil", "fed.gas")
  
  # Adjust to dollar basis used in rest of model using inf_adj.R function
  for (i in 1:nrow(CI)) {
    CI[i,3:6] <- inf_adj(price = CI[i,3:6], index = CI$cpi[i], basis = basis)
  }
  
  # Find PDF
  x <- seq(from = CI.pdf.min, to = CI.pdf.max, by = 0.001)
  y.state.oil <- dnorm(x = x, mean = mean(CI$state.oil), sd = sd(CI$state.oil))
  y.state.gas <- dnorm(x = x, mean = mean(CI$state.gas), sd = sd(CI$state.gas))
  y.fed.oil   <- dnorm(x = x, mean = mean(CI$fed.oil), sd = sd(CI$fed.oil))
  y.fed.gas   <- dnorm(x = x, mean = mean(CI$fed.gas), sd = sd(CI$fed.gas))
  
  # Convert to CDF
  cdf.state.oil <- cumsum(y.state.oil*diff(x[1:2]))
  cdf.state.gas <- cumsum(y.state.gas*diff(x[1:2]))
  cdf.fed.oil   <- cumsum(y.fed.oil*diff(x[1:2]))
  cdf.fed.gas   <- cumsum(y.fed.gas*diff(x[1:2]))
  
  # Normalize and rename
  ySO <- cdf.state.oil/max(cdf.state.oil)
  ySG <- cdf.state.gas/max(cdf.state.gas)
  yFO <- cdf.fed.oil/max(cdf.fed.oil)
  yFG <- cdf.fed.gas/max(cdf.fed.gas)
  
  # Place results for export into dataframe
  cdf.CI <- data.frame(x, ySO, ySG, yFO, yFG)
  
  
  # Export results as *.rda file --------------------------------------------
  
  save(file=file.path(path$data,
                      paste("cdf_corpIncomeTax_", version, ".rda", sep = "")),
       list=c("cdf.CI"))
}