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

# cdf.PT - data.frame with columns for (1) conversion factor in $/bbl or $/MCF 
# property tax paid in basis $ and (2) the cumulative probability density for
# each conversion factor rate


# Description -------------------------------------------------------------

# The function below updates the cumulative distribution function for property 
# tax conversion factors and is intended to be run when new property tax data
# becomes available.

# The function first determines to the total oil and gas production in Uintah 
# and Duchesne counties for each year where tax data is available, as well as
# the mean oil and gas prices in those years and the mean CPI index value.

# Next, the function determines the net revenue from oil and gas sales. Assuming
# property tax revenue is proportional to sales revenue, the script then splits 
# revenue to oil and gas on a $/bbl and $/MCF basis based on actual tax payments
# to the state of UT. All of these coversion factors are bundled into a single
# data.frame and adjusted for inflation to "basis" dollars.

# Finally, the conversion factors are used to generate a probability
# distribution function assuming a normal distribution.


# Function ----------------------------------------------------------------

corpIncomeUpdate <- function(p, path, basis, ver, PTI) {}


# Internal variables - uncomment to debug ---------------------------------

basis <- opt$cpi
ver <- opt$file_ver
PTI <- opt$PTI


# Get production and mean prices ------------------------------------------

# Predefine space for annual oil/gas summation, mean energy prices, and mean
# CPI
prod.oil  <- rep(0, times = nrow(PTI))
prod.gas  <- prod.oil
price.oil <- prod.oil
price.gas <- prod.oil
cpi       <- prod.oil

# Loop for getting data for each year in NTI
for (i in 1:nrow(PTI)) {
  
  # Oil & Gas production summation
  tstart <- as.Date(paste(PTI$year[i],"-01-01",sep=""))
  tstop  <- as.Date(paste(PTI$year[i],"-12-01",sep=""))
  temp <- subset(p, subset = (p_rpt_period >= tstart &
                              p_rpt_period <= tstop))
  prod.oil[i] <- sum(temp$p_oil_prod, na.rm = TRUE)
  prod.gas[i] <- sum(temp$p_gas_prod, na.rm = TRUE)
  
  # eia.hp row index selection for year "i"
  ind <- which(eia.hp$month >= as.yearmon(tstart) &
               eia.hp$month <= as.yearmon(tstop))
  
  # Mean oil/gas prices by year
  price.oil[i] <- mean(eia.hp$OP[ind])  # Oil price
  price.gas[i] <- mean(eia.hp$GP[ind])  # Gas price
}

# Calculation -------------------------------------------------------------

# Total revenue
rev.oil <- price.oil*prod.oil
rev.gas <- price.gas*prod.gas

# Fraction revenue attributable to each resource
frac.oil <- rev.oil/(rev.oil+rev.gas)
frac.gas <- 1-frac.oil

# Inflation adjust PTI values
PTI$PTI <- inf_adj(price = PTI$PTI,
                   index = cpisomethingsomething,
                   basis = basis)

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
                    paste("cdf_corpIncomeTax_", ver, ".rda", sep = "")),
     list=c("cdf.CI"))

