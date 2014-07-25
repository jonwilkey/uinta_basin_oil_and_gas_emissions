# Script Info -------------------------------------------------------------
# corpIncomeTax_v1.R (Corporate income taxes conversion factor)
# Version 1
# 06/17/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1 -Loads *.rda files, user inputs UT state tax commission returns data,
#     script calculates resulting tax conversion factor, plots output, saves
#     result to *.rda file.


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------
# Windows
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"
# Functions directory
fin <- "D:/Dropbox/CLEAR/DOGM Data/Functions"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"

# # Mac
# # Prepared data directory
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"
# # Plot directory
# plot_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"
# # Functions directory
# fin <- "/Users/john/Dropbox/CLEAR/DOGM Data/Functions"
# # Working directory
# work_root <- "/Users/john//Dropbox/CLEAR/DOGM Data"

setwd(work_root)


# Functions ---------------------------------------------------------------
# List of functions used in this script to be loaded here


# Libraries ---------------------------------------------------------------
library(sqldf)

# Load required data files ------------------------------------------------

# DOGM data
load(file.path(data_root, "production.rda"))

# Oil & gas price history
load(file.path(data_root, "oil_and_gas_price_history_1999_to_2012.rda"))

# Rename for brevity
p <- production
remove(production)

# User Inputs -------------------------------------------------------------

# Net taxable income (2009, 2010, 2011) - from UT State Tax Comission
net <- c(66341510, 209171843, 220215146)


# Oil and Gas Data Selection ----------------------------------------------

# Oil and Gas Production for 2009 - 2011 period
prod.oil <- c(sum(p$p_oil_prod[which(p$p_rpt_period >= as.Date("2009-01-01") &
                                     p$p_rpt_period <= as.Date("2009-12-01"))]),
              sum(p$p_oil_prod[which(p$p_rpt_period >= as.Date("2010-01-01") &
                                     p$p_rpt_period <= as.Date("2010-12-01"))]),
              sum(p$p_oil_prod[which(p$p_rpt_period >= as.Date("2011-01-01") &
                                     p$p_rpt_period <= as.Date("2011-12-01"))]))

prod.gas <- c(sum(p$p_gas_prod[which(p$p_rpt_period >= as.Date("2009-01-01") &
                                     p$p_rpt_period <= as.Date("2009-12-01"))]),
              sum(p$p_gas_prod[which(p$p_rpt_period >= as.Date("2010-01-01") &
                                     p$p_rpt_period <= as.Date("2010-12-01"))]),
              sum(p$p_gas_prod[which(p$p_rpt_period >= as.Date("2011-01-01") &
                                     p$p_rpt_period <= as.Date("2011-12-01"))]))

# OGprice annual index selection
ind.2009 <- which(OGprice$t >= as.Date("2009-01-01") &
                  OGprice$t <= as.Date("2009-12-01"))
ind.2010 <- which(OGprice$t >= as.Date("2010-01-01") &
                  OGprice$t <= as.Date("2010-12-01"))
ind.2011 <- which(OGprice$t >= as.Date("2011-01-01") &
                  OGprice$t <= as.Date("2011-12-01"))

# Annual average oil and gas prices
price.oil <- c(mean(OGprice$bw[ind.2009]),
               mean(OGprice$bw[ind.2010]),
               mean(OGprice$bw[ind.2011]))

price.gas <- c(mean(OGprice$uswp[ind.2009]),
               mean(OGprice$uswp[ind.2010]),
               mean(OGprice$uswp[ind.2011]))

# Annual average CPI
cpi <- c(mean(OGprice$cpi[ind.2009]),
         mean(OGprice$cpi[ind.2010]),
         mean(OGprice$cpi[ind.2011]))


# Calculation -------------------------------------------------------------

# Total revenue
rev.oil <- price.oil*prod.oil
rev.gas <- price.gas*prod.gas

# Fraction revenue attributable to each resource
frac.oil <- rev.oil/(rev.oil+rev.gas)
frac.gas <- 1-frac.oil

# State income tax
state <- 0.05*net

# Federal income tax
fed <- 0.35*net-state

# Put everything on $ per unit of production basis ($/bbl oil, $/MCF gas)
state.oil <- frac.oil*state/prod.oil
state.gas <- frac.gas*state/prod.gas
fed.oil <- frac.oil*fed/prod.oil
fed.gas <- frac.gas*fed/prod.gas
CI <- data.frame(c(2009, 2010, 2011), cpi, state.oil, state.gas, fed.oil, fed.gas)
names(CI) <- c("year", "cpi", "state.oil", "state.gas", "fed.oil", "fed.gas")

# Adjust 2009 and 2010 years to 2011 dollars
CI[1,3:6] <- CI[1,3:6]*CI$cpi[3]/CI$cpi[1]
CI[2,3:6] <- CI[2,3:6]*CI$cpi[3]/CI$cpi[2]

# Find PDF
x <- seq(from = 0, to = 3, by = 0.001)
y.state.oil <- dnorm(x = x, mean = mean(CI$state.oil), sd = sd(CI$state.oil))
y.state.gas <- dnorm(x = x, mean = mean(CI$state.gas), sd = sd(CI$state.gas))
y.fed.oil <- dnorm(x = x, mean = mean(CI$fed.oil), sd = sd(CI$fed.oil))
y.fed.gas <- dnorm(x = x, mean = mean(CI$fed.gas), sd = sd(CI$fed.gas))

# Convert to CDF
cdf.state.oil <- cumsum(y.state.oil*diff(x[1:2]))
cdf.state.gas <- cumsum(y.state.gas*diff(x[1:2]))
cdf.fed.oil <- cumsum(y.fed.oil*diff(x[1:2]))
cdf.fed.gas <- cumsum(y.fed.gas*diff(x[1:2]))

# Normalize and rename
ySO <- cdf.state.oil/max(cdf.state.oil)
ySG <- cdf.state.gas/max(cdf.state.gas)
yFO <- cdf.fed.oil/max(cdf.fed.oil)
yFG <- cdf.fed.gas/max(cdf.fed.gas)

# Place results for export into dataframe
cdf.CI <- data.frame(x, ySO, ySG, yFO, yFG)


# Plot Results ------------------------------------------------------------

# Save plots to PDF file
pdf(file.path(plot_root, "corpIncomeTax_v1 Results.pdf"))

# PDF - State Oil
plot(x, y.state.oil,
     type = "l",
     xlab = "Corporate Income Tax (2011 $/bbl)",
     ylab = "Probability Density",
     main = "PDF for State Corporate Income Tax for Oil")
rug(CI$state.oil)

# PDF - State Gas
plot(x, y.state.gas,
     type = "l",
     xlab = "Corporate Income Tax (2011 $/MCF)",
     ylab = "Probability Density",
     main = "PDF for State Corporate Income Tax for Gas")
rug(CI$state.gas)

# PDF - Fed Oil
plot(x, y.fed.oil,
     type = "l",
     xlab = "Corporate Income Tax (2011 $/bbl)",
     ylab = "Probability Density",
     main = "PDF for Federal Corporate Income Tax for Oil")
rug(CI$fed.oil)

# PDF - Fed Gas
plot(x, y.fed.gas,
     type = "l",
     xlab = "Corporate Income Tax (2011 $/MCF)",
     ylab = "Probability Density",
     main = "PDF for Federal Corporate Income Tax for Gas")
rug(CI$fed.gas)

# CDF - State Oil
plot(x, cdf.CI$ySO,
     type = "l",
     xlab = "Corporate Income Tax (2011 $/bbl)",
     ylab = "Cumulative Probability Density",
     main = "CDF for State Corporate Income Tax for Oil")
rug(CI$state.oil)

# CDF - State Gas
plot(x, cdf.CI$ySG,
     type = "l",
     xlab = "Corporate Income Tax (2011 $/MCF)",
     ylab = "Cumulative Probability Density",
     main = "CDF for State Corporate Income Tax for Gas")
rug(CI$state.gas)

# CDF - Fed Oil
plot(x, cdf.CI$yFO,
     type = "l",
     xlab = "Corporate Income Tax (2011 $/bbl)",
     ylab = "Cumulative Probability Density",
     main = "CDF for Federal Corporate Income Tax for Oil")
rug(CI$fed.oil)

# CDF - Fed Gas
plot(x, cdf.CI$yFG,
     type = "l",
     xlab = "Corporate Income Tax (2011 $/MCF)",
     ylab = "Cumulative Probability Density",
     main = "CDF for Federal Corporate Income Tax for Gas")
rug(CI$fed.gas)

dev.off()


# Export results as *.rda file --------------------------------------------

# Note - adjust x-values from 2011 dollars (CPI = 224.9392) to desired dollars
# in destination script
save(file=file.path(data_root, "cdf_corpIncomeTax_v1.rda"), list=c("cdf.CI"))
