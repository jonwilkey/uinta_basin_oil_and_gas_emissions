# Script Info -------------------------------------------------------------
# conventional_v9.R (Conventional Oil and Gas Model)
# Version 10
# 05/29/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1 -Loads *.rda files, finds APDs approved in each field at each timestep
#     according to Model 8, determines monthly production by field, plots
#     predicted vs. actual APD approvals, plots predicted production.
#
# v2 -Added sqldf query to pull actual production history of gas and oil in Basin.
#
# v3 -Added off-well type production (gas from oil wells, oil from gas wells).
#    -Expanded data set from Michael's July 2012 analysis to Nov. 2013 dataset
#     with proddata, histdata, and welldata for h_apd_aprovd > "1999-01-01" and
#     w_county == "UINTAH" | w_county == "DUCHESNE". Named file conventional_data
#     and added value ptime = p_rpt_period - h_apd_aprovd, converted into units
#     of weeks and then divided by 4 to get months.
#    -Updated m.fit.ow and m.fit.gw to cover full APD approval dataset.
#
# v4 -Added Monte-Carlo simulation of production rates taking the drilling
#     schedule (# of wells and field location) as a given.
#
# v5 -Fixed indexing error in MC simulation call of findInterval.
#
# v6 -Added GHG emissions estimates for oil production from oil wells. Added GGW
#     loop for MC simulations.
#
# v7 -Corrected determination of actual oil and gas production rates to only
#     select production data from Uintah and Duchesne counties.
#
# v8 -Added back code for determining drilling schedule using regression curves
#     to all data in each field (i.e. pre-Monte Carlo approach) for debugging.
#
# v9 -Rewrote code to vectorize most steps in simulation (generation of well
#     data, calculating production).
#
# v10-Added calculations for production, royatlies, severance taxes, and
#     property taxes.


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
flst <- file.path(fin,c("royalty.R",
                        "severance_tax.R",
                        "inflation_adjust.R",
                        "write_excel.R"))

# Load each function in list
for (f in flst) source(f)

# Remove temporary variables
remove(fin, flst, f)


# Libraries ---------------------------------------------------------------
library(sqldf)
library(zoo)
library(data.table)


# Load required data files ------------------------------------------------

# CDFs for drilling rates, field assignments, land ownership, well type, and
# actual oil and gas drilling rates
load(file.path(data_root, "cdf_schedule_v1.rda"))

# PDF x-values & CDF y-values for MC simulation of decline curve coefficients
load(file.path(data_root, "pdf_oow.rda"))
load(file.path(data_root, "cdf_oow.rda"))
load(file.path(data_root, "pdf_ggw.rda"))
load(file.path(data_root, "cdf_ggw.rda"))

# DOGM data
load(file.path(data_root, "production.rda"))
load(file.path(data_root, "histdata.rda"))

# Decline curve fits at field level
load(file.path(data_root, "decline_field_ow.rda"))
load(file.path(data_root, "decline_field_gw.rda"))

# Oil & gas price history
load(file.path(data_root, "oil_and_gas_price_history_1999_to_2012.rda"))

# Leasing operating cost fits
load(file.path(data_root, "leaseOpCost_v1.rda"))

# Rename some dataframes for brevity
p <- production
h <- histdata
remove(production, histdata)

# Drop field listing from cdf.fsl dataframe and change type to matrix, don't
# know why but won't work otherwise
cdf.fsl <- cdf.fsl[,-1]; cdf.fsl <- as.matrix(cdf.fsl)

# Extract and reorder desired decline curve coefficients for selected fields
cdf.oow <- cdf.oow[,c(34, 35, 36, 1, 2, 3, 4, 5, 6, 16, 17, 18, 22, 23, 24, 28,
                      29, 30, 25, 26, 27, 7, 8, 9, 19, 20, 21, 13, 14, 15,
                      34, 35, 36)]
pdf.oow <- pdf.oow[,c(34, 35, 36, 1, 2, 3, 4, 5, 6, 16, 17, 18, 22, 23, 24, 28,
                      29, 30, 25, 26, 27, 7, 8, 9, 19, 20, 21, 13, 14, 15,
                      34, 35, 36)]
# As placeholder
cdf.ggw <- cdf.oow
pdf.ggw <- pdf.oow

# Other Inputs ------------------------------------------------------------
# Create a complete set of months between 1999-01-01 and 2013-12-01.
all_months <- seq(from = as.Date("1999-01-01"),
                  to = as.Date("2012-12-01"),
                  by = "months")

# Field Selection (i.e. fields that will be analyzed individually). Note that
# "Field 999" is placeholder for all other fields category.
field <- c(630, 105, 72, 55, 65, 710, 665, 590, 60, 718, 999)


# MC Simulation: Well Data ------------------------------------------------

# Number of iterations
nrun <- 10^0

Drilled <- round(cdf.drill[findInterval(runif(length(all_months)*nrun),
                                        c(0, cdf.drill[,2])),1])
result <- matrix(0, nrow = sum(Drilled), ncol = 3)

# Loop counters
a <- 1 # WellID
b <- 1 # Tstep
c <- 1 # runID
bstop <- length(all_months)

# Note - this is still slow
for (g in 1:length(Drilled)) {
  if (Drilled[g] > 0) {
    for (h in 1:Drilled[g]) {
      result[a,1] <- a
      result[a,2] <- b
      result[a,3] <- c
      a <- a+1
    }
  }
  b <- b+1
  if (b >= bstop) {
    b <- 1
    c <- c+1
  }
}

# Pick oil or gas well
type <- ifelse(test = runif(nrow(result)) <= prob.gas,
               yes = "GW",
               no = "OW")

fieldnum <- ifelse(test = type == "GW",
                   yes = cdf.ffg[findInterval(runif(length(type)),
                                              c(0, cdf.ffg[,2])), 1],
                   no = cdf.ffo[findInterval(runif(length(type)),
                                             c(0, cdf.ffo[,2])), 1])

# Predefine vector sizes
acoef <- rep(0, times = length(type))
bcoef <- acoef
ccoef <- acoef
landown <- acoef

# Generate decline curve coefficient values for field and well type
for (i in 1:length(field)) {
  ind.ow <- which(type == "OW" & fieldnum == field[i])
  acoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                        c(0, cdf.oow[,(i-1)*3+1])), (i-1)*3+1]
  bcoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                        c(0, cdf.oow[,(i-1)*3+2])), (i-1)*3+2]
  ccoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                        c(0, cdf.oow[,(i-1)*3+3])), (i-1)*3+3]
  
  ind.gw <- which(type == "GW" & fieldnum == field[i])
  acoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                        c(0, cdf.ggw[,(i-1)*3+1])), (i-1)*3+1]
  bcoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                        c(0, cdf.ggw[,(i-1)*3+2])), (i-1)*3+2]
  ccoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                        c(0, cdf.ggw[,(i-1)*3+3])), (i-1)*3+3]
  
  landown[c(ind.ow, ind.gw)] <- findInterval(runif(length(c(ind.ow, ind.gw))),
                                             c(0, cdf.fsl[i,]))
}

# Replace landownership #s with strings (switch expression in royalty.R function
# requires switch to operate on a string, not a numerical value)
landown[which(landown == 1)] <- "federal"
landown[which(landown == 2)] <- "indian"
landown[which(landown == 3)] <- "state"
landown[which(landown == 4)] <- "fee"

# Make a data table
wsim <- data.table(result, type, fieldnum, acoef, bcoef, ccoef, landown)
# Set/change column names
setnames(x = wsim,
         old = 1:ncol(wsim),
         new = c("wellID", "tDrill", "runID", "wellType", "fieldnum", "a", "b",
                 "c", "landOwner"))

# Cleanup
remove(result, type, fieldnum, acoef, bcoef, ccoef, landown, a, b, c, Drilled,
       nrun, g, h, i, ind.gw, ind.ow)


# Production Simulation ---------------------------------------------------

# Predefine production matrix (rows = individual wells, columns = time steps)
psim <- matrix(0, nrow = nrow(wsim), ncol = length(all_months))

# Calculate production by selecting all wells drilled in a given time step and
# calculating production simultaneously for all selected wells
for (i in 1:length(all_months)) {
  ind <- which(wsim$tDrill == i)
  for (t in 1:(length(all_months)+1-i)) {
    psim[ind,(i+t-1)] <- wsim$a[ind]*(1+wsim$b[ind]*wsim$c[ind]*t)^(-1/wsim$b[ind])
  }
}

nancount <- 0
negcount <- 0
# Cleanup NaN and negative values from psim
for (i in 1:length(all_months)) {
  ind.nan <- which(psim[,i] == "NaN")
  ind.neg <- which(psim[,i] < 0)
  nancount <- nancount + length(ind.nan)
  negcount <- negcount + length(ind.neg)
  psim[ind.nan,i] <- 0
  psim[ind.neg,i] <- 0
}

# Fiscal Impacts ----------------------------------------------------------

# Define prices and adjust for inflation - op = oil price, gp = gas price. Given
# basis (233.049) inflation adjusts to 2013-12-01.
basis <- 233.049
op <- inf_adj(OGprice$bw, OGprice$cpi, basis)
gp <- inf_adj(OGprice$uswp, OGprice$cpi, basis)

# Royalties
rsim <- matrix(0, nrow = nrow(wsim), ncol = length(all_months))

ind.gw     <- which(wsim$wellType == "GW")
ind.gw.fed <- which(wsim$wellType == "GW" & wsim$landOwner == "federal")
ind.gw.ind <- which(wsim$wellType == "GW" & wsim$landOwner == "indian")
ind.gw.sta <- which(wsim$wellType == "GW" & wsim$landOwner == "state")
ind.gw.fee <- which(wsim$wellType == "GW" & wsim$landOwner == "fee")

ind.ow     <- which(wsim$wellType == "OW")
ind.ow.fed <- which(wsim$wellType == "OW" & wsim$landOwner == "federal")
ind.ow.ind <- which(wsim$wellType == "OW" & wsim$landOwner == "indian")
ind.ow.sta <- which(wsim$wellType == "OW" & wsim$landOwner == "state")
ind.ow.fee <- which(wsim$wellType == "OW" & wsim$landOwner == "fee")

for (i in 1:length(all_months)) {
  rsim[ind.gw.fed,i] <- royalty(psim[ind.gw.fed,i], gp[i], "federal")
  rsim[ind.gw.ind,i] <- royalty(psim[ind.gw.ind,i], gp[i], "indian")
  rsim[ind.gw.sta,i] <- royalty(psim[ind.gw.sta,i], gp[i], "state")
  rsim[ind.gw.fee,i] <- royalty(psim[ind.gw.fee,i], gp[i], "fee")
  
  rsim[ind.ow.fed,i] <- royalty(psim[ind.ow.fed,i], op[i], "federal")
  rsim[ind.ow.ind,i] <- royalty(psim[ind.ow.ind,i], op[i], "indian")
  rsim[ind.ow.sta,i] <- royalty(psim[ind.ow.sta,i], op[i], "state")
  rsim[ind.ow.fee,i] <- royalty(psim[ind.ow.fee,i], op[i], "fee")
}


# Severance Taxes
# This is slow, fix NaN problem so we can get past ifelse()-ing to handle division by 0
rrate <- ifelse(test = rsim/psim != "NaN",
                yes = rsim/psim,
                no = 0)

# Define severance tax (stsim) matrix
stsim <- matrix(0, nrow = nrow(wsim), ncol = length(all_months))

# Calculate severance taxes for oil/gas wells in each month of simulation
for (i in 1:length(all_months)) {
  stsim[ind.gw,i] <- ST(psim[ind.gw,i], gp[i], rrate[ind.gw,i])
  stsim[ind.ow,i] <- ST(psim[ind.ow,i], op[i], rrate[ind.ow,i])
}

# Exempt first six months of production by writing 0 over ST calculations from proceeding loop
for (i in 1:(length(all_months)-5)) {
  ind <- which(wsim$tDrill == i)
  stsim[ind,i:(i+5)] <- 0
}

# Handle special case where < 5 time steps remain in matrix row
for (i in (length(all_months)-4):length(all_months)) {
  ind <- which(wsim$tDrill == i)
  stsim[ind,i:ncol(stsim)] <- 0
}

# Property Taxes

# Predefine NPV and LOC matrix
rev <- matrix(0, nrow = nrow(wsim), ncol = length(all_months))
LOC <- rev

# Determine revenue (price * production)
for (i in 1:ncol(rev)) {
  rev[ind.ow,i] <- op[i]*psim[ind.ow,i]
  rev[ind.gw,i] <- gp[i]*psim[ind.gw,i]
}


# Determine lease operating costs - 6500 is placeholder for well depth value. 
# LOC is in 2009 dollars (CPI = 214.537), so it must be adjusted for inflation 
# to desired basis and converted to monthly value. Also, production rate must be
# in terms of SCFD.
depth <- 3140
for (i in 1:ncol(LOC)) {
  LOC[ind.ow,i] <- (fit.LOC.oil$coefficients[2]*op[i]+
                    fit.LOC.oil$coefficients[3]*depth+
                    fit.LOC.oil$coefficients[1])*(basis/214.537)*(1/12)
  LOC[ind.gw,i] <- (fit.LOC.gas$coefficients[2]*gp[i]+
                    fit.LOC.gas$coefficients[3]*depth+
                    fit.LOC.gas$coefficients[4]*wsim[ind.gw,]$a/30+
                    fit.LOC.gas$coefficients[1])*(basis/214.537)*(1/12)
}


NPV <- rev-rsim-stsim-LOC