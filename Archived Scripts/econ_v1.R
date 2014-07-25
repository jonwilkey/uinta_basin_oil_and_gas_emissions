# Script Info -------------------------------------------------------------
# econ_v1.R (Economic Impacts of Oil and Gas E&P)
# Version 1
# 05/13/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1. Loads *.rda files, prepares data, determines royalties, severance taxes,
#     property taxes, bonus payments, and corporate income taxes.


# Options -----------------------------------------------------------------
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(work_root)


# Functions ---------------------------------------------------------------
inf_adj <- function (price, index, basis) {
  price_adj <- price * (basis / index)
  return (price_adj)
}

R <- function (volume, price, landowner = "state") {
  # This function determines the royalty payments.
  
  # Pick rate based on landownership
  rate <- switch(landowner,
                 state   = 0.1250,
                 federal = 0.1250,
                 tribe   = 0.1667,
                 cat(landowner, "is not a recognized type\n"))
  
  royalty <- rate * volume * price # Total royalty
  r_rate <- royalty / volume # Royalty payments per unit volume
  out <- data.frame(royalty, r_rate)
  return(out)
}

ST <- function (volume, price, royalty_rate, API = 39.6) {
  # This function determines severance tax payments.
  # Severance tax rates
  s_low  <- 0.030 # for values <= $13/bbl
  s_high <- 0.050 # for values > $13/bbl
  s_cf   <- 0.002 # Conservation fee
  
  # Determine wellhead value (WV). API gravity of WTI (39.6 degrees) is used as
  # the basis for this calculation. If producing an unconventional oil (from
  # shale or sand), replace the default API gravity argument in the function
  # call with the API gravity of the oil in question
  WV <- (API / 39.6) * price
  
  # Deductions on WV to determine taxable value (TV). Currently just deducting
  # royalty payments (given here on $/bbl basis from "R" function)
  TV <- WV - royalty_rate
  
  # Determine fraction of TV above split tax rate (f_st) of $13/bbl
  f_st <- rep(0, length(TV))
  f_st <- ifelse(test = (TV - 13)/TV < 0,
                 yes = 0,
                 no = (TV - 13)/TV)
    
  rST <- (s_low * (1 - f_st) + s_high * f_st) * TV + s_cf * TV # ST per unit vol
  ST <- rST * volume # ST total
  return(ST)
}


# Libraries ---------------------------------------------------------------


# Load Data ---------------------------------------------------------------
# Production history
load(file.path(data_root, "prod_gas_fit.rda"))
load(file.path(data_root, "prod_oil_fit.rda"))
# Drilling history
load(file.path(data_root, "schedule_ow.rda"))
load(file.path(data_root, "schedule_gw.rda"))
# Oil & gas price history
load(file.path(data_root, "oil_and_gas_price_history_1999_to_2012.rda"))


# Prepare Data ------------------------------------------------------------
# Determine wells drilled in each time step
schedule.gw <- rowSums(schedule.gw[1:168,])
schedule.ow <- rowSums(schedule.ow[1:168,])

# Rename for brevity
gas <- prod.gas.fit[1:168]
oil <- prod.oil.fit[1:168]
remove(prod.gas.fit, prod.oil.fit)

# Define time series
t <- OGprice$t

# Define prices and adjust for inflation - op = oil price, gp = gas price. Given
# basis (233.049) inflation adjusts to 2013-12-01.
op <- inf_adj(OGprice$bw, OGprice$cpi, basis = 233.049)
gp <- inf_adj(OGprice$uswp, OGprice$cpi, basis = 233.049)


# Royalties --------------------------------------------------------------- 
# Determine oil royalties (R_oil) and gas royalties (R_gas). Note that this
# currently neglecting issues regarding landownership, and assumes state/federal
# rate (12.5%)
R_oil <- R(volume = oil, price = op, landowner = "state")
R_gas <- R(volume = gas, price = gp, landowner = "state")


# Severance Taxes ---------------------------------------------------------
# Determine severance tax for oil (ST_oil) and gas (ST_gas)
ST_oil <- ST(volume = oil, price = op, royalty_rate = R_oil$r_rate)
ST_gas <- ST(volume = gas, price = gp, royalty_rate = R_gas$r_rate)


# Property Taxes ----------------------------------------------------------

#blah I am property taxes hear me roar


# Bonus Payments ----------------------------------------------------------

# blah blah blah I am bonus payments hear me squeak


# Corporate Income Tax ----------------------------------------------------

# ha I am corp income tax you can't find me


# Plots -------------------------------------------------------------------
# Save to PDF
pdf(file.path(plot_root, "econ_v1 Results.pdf"))

# Oil Price History
plot(t, op,
     type = "l",
     xlab = "Time",
     ylab = "Oil Price (USD/bbl - Dec. 2013)",
     main = "Oil Price History")

# Gas Price History
plot(t, gp,
     type = "l",
     xlab = "Time",
     ylab = "Gas Price (USD/MCF - Dec. 2013)",
     main = "Gas Price History")

# Oil Production
plot(t, oil/1e3,
     type = "l",
     xlab = "Time",
     ylab = "Oil Production (1e3 bbl)",
     main = "Oil Production History")

# Gas Production
plot(t, gas/1e6,
     type = "l",
     xlab = "Time",
     ylab = "Gas Production (1e6 MCF)",
     main = "Gas Production History")

# Oil Royalty Payments
plot(t, R_oil[,1]/1e6,
     type = "l",
     xlab = "Time",
     ylab = "Royalties (million USD - Dec. 2013)",
     main = "Oil Royalty Payments")

# Gas Royalty Payments
plot(t, R_gas[,1]/1e6,
     type = "l",
     xlab = "Time",
     ylab = "Royalties (million USD - Dec. 2013)",
     main = "Gas Royalty Payments")

# Oil Severance Tax Payments
plot(t, ST_oil/1e6,
     type = "l",
     xlab = "Time",
     ylab = "Severance Taxes (million USD - Dec. 2013)",
     main = "Oil Severance Tax Payments")

# Gas Severance Tax Payments
plot(t, ST_gas/1e6,
     type = "l",
     xlab = "Time",
     ylab = "Severance Taxes (million USD - Dec. 2013)",
     main = "Gas Severance Tax Payments")

dev.off()