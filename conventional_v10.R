# Script Info -------------------------------------------------------------
# conventional_v10.R (Conventional Oil and Gas Model)
# Version 10
# 06/26/14
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
# v+ -Started using version control, see comments on commits for changelog


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

# Well Depth PDF x-values & CDF y-values
load(file.path(data_root, "cdf_wellDepth_v1.rda"))

# Corporate income taxes in 2011 dollars (CPI = 224.9392)
load(file.path(data_root, "cdf_corpIncomeTax_v1.rda"))

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

# CPI value for inflation adjustment
basis <- 233.049


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
for (i in 1:length(Drilled)) {
  if (Drilled[i] > 0) {
    for (j in 1:Drilled[i]) {
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

# Pick field number
fieldnum <- ifelse(test = type == "GW",
                   yes = cdf.ffg[findInterval(runif(length(type)),
                                              c(0, cdf.ffg[,2])), 1],
                   no = cdf.ffo[findInterval(runif(length(type)),
                                             c(0, cdf.ffo[,2])), 1])

# Generate corporate tax rates for each well
# 1) Ajdust to "basis" 2013 dollars from 2011 dollars (CPI = 244.9392) by
#    modifying x-values ($/unit production) in cdf.CI
cdf.CI$x <- cdf.CI$x*(basis/224.9392)

# 2) Predefine vectors for results (cirSO - corp income rate state oil, cirSG -
#    state gas, cirFO - fed oil, cirFG - fed gas).
cirSO <- cdf.CI$x[findInterval(runif(length(type)),c(0,cdf.CI$ySO))]
cirSG <- cdf.CI$x[findInterval(runif(length(type)),c(0,cdf.CI$ySG))]
cirFO <- cdf.CI$x[findInterval(runif(length(type)),c(0,cdf.CI$yFO))]
cirFG <- cdf.CI$x[findInterval(runif(length(type)),c(0,cdf.CI$yFG))]

# 3) Run runif() for: 1- state oil, 2- state gas, 3- fed oil, 4- fed

# Predefine vector sizes
acoef <- rep(0, times = length(type))
bcoef <- acoef
ccoef <- acoef
depth <- acoef
landown <- acoef

# Pull indices of oil and gas wells
ind.ow <- which(type == "OW")
ind.gw <- which(type == "GW")

# Generate total measured depth for each well based on well type
depth[ind.ow] <- cdf.depth.ow$x[findInterval(runif(length(ind.ow)),
                                             c(0, cdf.depth.ow$y))]
depth[ind.gw] <- cdf.depth.gw$x[findInterval(runif(length(ind.gw)),
                                             c(0, cdf.depth.gw$y))]

# Drilling and completion capital cost

# Drilling capital cost coefficients in eq. y = exp(a+b*(depth in ft))
coef.drill <- c(11.46, 0.00024)

# Completion capital cost coefficients in eq. y = exp(a+b*(depth in ft))
coef.compl <- c(11.217, 0.00022)

# Annual average CPI value for 2011 dollars (base year used for capital costs)
base.index <- 224.939

# Total capital cost is sum of drilling and capital costs, adjusted for
# inflation from 2011 dollars to model-year dollars
cost <- (exp(coef.drill[1]+coef.drill[2]*depth)+
         exp(coef.compl[1]+coef.compl[2]*depth))*(basis/base.index)

# Generate decline curve coefficient values for field and well type
for (i in 1:length(field)) {
  ind.ow <- which(type == "OW" & fieldnum == field[i])
  acoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                        c(0, cdf.oow[,(i-1)*3+1])), (i-1)*3+1]
  bcoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                        c(0, cdf.oow[,(i-1)*3+2])), (i-1)*3+2]
  ccoef[ind.ow] <- pdf.oow[findInterval(runif(length(ind.ow)),
                                        c(0, cdf.oow[,(i-1)*3+3])), (i-1)*3+3]
  landown[ind.ow] <- findInterval(runif(length(ind.ow)),
                                        c(0, cdf.fsl[i,]))
  
  ind.gw <- which(type == "GW" & fieldnum == field[i])
  acoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                        c(0, cdf.ggw[,(i-1)*3+1])), (i-1)*3+1]
  bcoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                        c(0, cdf.ggw[,(i-1)*3+2])), (i-1)*3+2]
  ccoef[ind.gw] <- pdf.ggw[findInterval(runif(length(ind.gw)),
                                        c(0, cdf.ggw[,(i-1)*3+3])), (i-1)*3+3]
  landown[ind.gw] <- findInterval(runif(length(ind.gw)),
                                        c(0, cdf.fsl[i,]))
}

# Replace landownership #s with strings (switch expression in royalty.R function
# requires switch to operate on a string, not a numerical value)
landown[which(landown == 1)] <- "federal"
landown[which(landown == 2)] <- "indian"
landown[which(landown == 3)] <- "state"
landown[which(landown == 4)] <- "fee"

# Make a data table
wsim <- data.table(result, type, fieldnum, acoef, bcoef, ccoef, depth, landown,
                   cirSO, cirSG, cirFO, cirFG, cost)
# Set/change column names
setnames(x = wsim,
         old = 1:ncol(wsim),
         new = c("wellID", "tDrill", "runID", "wellType", "fieldnum", "a", "b",
                 "c", "depth", "landOwner", "cirSO", "cirSG", "cirFO", "cirFG",
                 "cost"))

# Cleanup
remove(result, type, fieldnum, acoef, bcoef, ccoef, depth, landown, a, b, c,
       Drilled, i, j, ind.gw, ind.ow, cirSO, cirSG, cirFO, cirFG,
       coef.drill, coef.compl, base.index, cost)


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


# Royalties ---------------------------------------------------------------

# Define prices and adjust for inflation - op = oil price, gp = gas price. Given
# basis (233.049) inflation adjusts to 2013-12-01.
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



# Severance Taxes ---------------------------------------------------------

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


# Property Taxes ----------------------------------------------------------

# Predefine NPV and LOC matrix
rev <- matrix(0, nrow = nrow(wsim), ncol = length(all_months))
LOC <- rev

# Determine revenue (price * production)
for (i in 1:ncol(rev)) {
  rev[ind.ow,i] <- op[i]*psim[ind.ow,i]
  rev[ind.gw,i] <- gp[i]*psim[ind.gw,i]
}

# Determine lease operating costs - LOC is in 2009 dollars (CPI = 214.537), so 
# it must be adjusted for inflation to desired basis and converted to monthly 
# value. Also, production rate must be in terms of SCFD (so divide initial 
# production rate coefficient by 30 to covert from months to days). Finally LOC
# data is in dollars per year, so convert from annual costs to monthly costs by
# dividing by 12.
for (i in 1:ncol(LOC)) {
  LOC[ind.ow,i] <- (fit.LOC.oil$coefficients[2]*op[i]+
                    fit.LOC.oil$coefficients[3]*wsim$depth[ind.ow]+
                    fit.LOC.oil$coefficients[1])*(basis/214.537)*(1/12)
  LOC[ind.gw,i] <- (fit.LOC.gas$coefficients[2]*gp[i]+
                    fit.LOC.gas$coefficients[3]*wsim$depth[ind.gw]+
                    fit.LOC.gas$coefficients[4]*wsim[ind.gw,]$a/30+
                    fit.LOC.gas$coefficients[1])*(basis/214.537)*(1/12)
}

# Take deductions for royalties, severance taxes, and LOCs
NPV <- rev-rsim-stsim-LOC

# Specify annual discount rate
rate <- 0.1164

# Calculate vector of discount factors
DF <- (1+rate/12)^(-c(0:(length(all_months)-1)))


# Corporate Income Taxes --------------------------------------------------

# Predefine matrices for calculation results
ciSO <- matrix(0, nrow = nrow(psim), ncol = ncol(psim))
ciSG <- ciSO
ciFO <- ciSO
ciFG <- ciSO

# Calculate corporate income taxes for on-type (OOW & GGW) production
for (i in 1:ncol(psim)) {
  ciSO[ind.ow,i] <- wsim$cirSO[ind.ow]*psim[ind.ow,i]
  ciSG[ind.gw,i] <- wsim$cirSG[ind.gw]*psim[ind.gw,i]
  ciFO[ind.ow,i] <- wsim$cirFO[ind.ow]*psim[ind.ow,i]
  ciFG[ind.gw,i] <- wsim$cirFG[ind.gw]*psim[ind.gw,i]
}


# Employment --------------------------------------------------------------

# ===== RIMS Estimate =====

# RIMS II Multiplier
RIMS <- 4.4524

# Preallocate investment schedule space
invest <- matrix(0, nrow = nrun, floor(length(all_months)/12))

# Determine spending schedule on annual basis
for (i in 1:nrun) {
  for (j in 1:ncol(invest)) {
    tstart <- 12*(j-1)+1
    tstop <- 12*(j-1)+12
    ind <- which(wsim$runID == i & wsim$tDrill >= tstart & wsim$tDrill <= tstop)
    invest[i,j] <- sum(wsim$cost[ind])+sum(rowSums(LOC[ind,tstart:tstop]))
  }
}

# RIMS II job creation calculation (muliplier * spending in millions of dollars)
jobs.RIMS <- RIMS*invest/1e6

# ===== Workload Estimate =====

# Constants for calculations
rig.workers <- 23                   # Workers per rig (from Duane Winkler email for directional rig)
rig.duration <- 15*24               # Duration of drilling a well in hours (to be replaced w/ CDF pick in wsim)
frack.workers <- 2*rig.workers      # Fracking (assume 2x as many people needed as regular rig)
frack.duration <- 0.25*rig.duration # Assumed portion of time that rig is onsite that fracking is occuring
truck.milage <- 2660                # Heavy duty diesel truck (HDDT) miles traveled per well spud (Environ 2011 study Table 6)
truck.speed <- 16.4                 # HDDT average speed over unpaved roads (Environ 2011 study Table 3)
gosp.workers <- (4+   # Freewater knockout drums (horizontal three phase vessels)
                 1    # Vapor recovery and incinerator
                 )*2  # Design capacity 10e3 bbl (1:1 oil:water), mass flowrate of 1620 ton/day assuming API gravity = 35 deg.
gosp.capacity <- 5e3  # Design capacity in terms of BPD oil
gpp.workers <- (1+    # Feed prep - condensate and water removal
                1+    # Acid gas removal (amine treating)
                1+    # Dehydration (gylcol unit)
                1+    # Nitrogen rejection (cyrogenic process)
                1     # NGL Recovery (turbo-expander and demethanizer tower)  
                  )*2 # Double # of operators per section since any plant larger than 4.7 MMscfd > 100 ton/day limit
gpp.capacity <- 300e3 # Design capacity for gas processing plant in Mscfd

FTE.hours <- 2080/12  # Man-hours per month equivalent to one full-time employee

# Predefine matrix space for man-hour calculations
manhr.drill <- matrix(0, nrow = nrun, ncol = length(all_months))
manhr.frack <- manhr.drill
manhr.truck <- manhr.drill
manhr.gosp  <- manhr.drill
manhr.gpp   <- manhr.drill

# Calculate man-hours per time step for each process step
for (i in 1:nrun) {
  for (j in 1:length(all_months)) {
    ind.ow <- which(wsim$tDrill == j & wsim$runID == i & wsim$wellType == "OW")
    ind.gw <- which(wsim$tDrill == j & wsim$runID == i & wsim$wellType == "GW")
    ind <- length(ind.ow)+length(ind.gw)
    manhr.drill[i,j] <- ind*rig.workers*rig.duration
    manhr.frack[i,j] <- ind*frack.workers*frack.duration
    manhr.truck[i,j] <- ind*truck.milage/truck.speed
    manhr.gosp[i,j]  <- ceiling(sum(psim[ind.ow,j])/gosp.capacity)*
                        gosp.workers*FTE.hours
    manhr.gpp[i,j]   <- ceiling(sum(psim[ind.gw,j])/gpp.capacity)*
                        gpp.workers*FTE.hours
  }
}

# Predefine matrix space for annual man-hour calculations
manhr.an.drill <- matrix(0, nrow = nrun, floor(length(all_months)/12))
manhr.an.frack <- manhr.an.drill
manhr.an.truck <- manhr.an.drill
manhr.an.gosp  <- manhr.an.drill
manhr.an.gpp   <- manhr.an.drill

# Sum to annual man-hours
for (i in 1:nrun) {
  for (j in 1:ncol(manhr.an.drill)) {
    tstart <- 12*(j-1)+1
    tstop <- 12*(j-1)+12
    ind <- which(wsim$runID == i & wsim$tDrill >= tstart & wsim$tDrill <= tstop)
    manhr.an.drill[i,j] <- sum(manhr.drill[tstart:tstop])
    manhr.an.frack[i,j] <- sum(manhr.frack[tstart:tstop])
    manhr.an.truck[i,j] <- sum(manhr.truck[tstart:tstop])
    manhr.an.gosp[i,j]  <- sum(manhr.gosp[tstart:tstop])
    manhr.an.gpp[i,j]   <- sum(manhr.gpp[tstart:tstop])
  }
}

# Adjust FTE.hours back to annual hours
FTE.hours <- FTE.hours*12

# Convert from man-hours to # of FTE employees
manhr.an.drill <- manhr.an.drill/FTE.hours
manhr.an.truck <- manhr.an.truck/FTE.hours
manhr.an.frack <- manhr.an.frack/FTE.hours
manhr.an.gosp  <- manhr.an.gosp/FTE.hours
manhr.an.gpp   <- manhr.an.gpp/FTE.hours