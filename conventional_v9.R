# Script Info -------------------------------------------------------------
# conventional_v9.R (Conventional Oil and Gas Model)
# Version 9
# 05/19/14
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


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------
# PC
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"
# Functions directory
fin <- "D:/Dropbox/CLEAR/DOGM Data/Functions"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(work_root)

# # Unix
# # Prepared data directory
# data_root <- "/home/slyleaf/Dropbox/CLEAR/DOGM Data/Prepared Data"
# # Plot directory
# plot_root <- "/home/slyleafDropbox/CLEAR/DOGM Data/Plots"
# # Functions directory
# fin <- "/home/slyleaf/Dropbox/CLEAR/DOGM Data/Functions"
# # Working directory
# work_root <- "/home/slyleaf/Dropbox/CLEAR/DOGM Data"
# setwd(work_root)


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
# Load *.rda files
load(file.path(data_root, "cdf_schedule_v1.rda"))
load(file.path(data_root, "pdf_oow.rda"))
load(file.path(data_root, "cdf_oow.rda"))
load(file.path(data_root, "pdf_ggw.rda"))
load(file.path(data_root, "cdf_ggw.rda"))
load(file.path(data_root, "production.rda"))
load(file.path(data_root, "histdata.rda"))
load(file.path(data_root, "decline_field_ow.rda"))
load(file.path(data_root, "decline_field_gw.rda"))
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
                  to = as.Date("2013-12-01"),
                  by = "months")

# Field Selection (i.e. fields that will be analyzed individually). Note that
# "Field 999" is placeholder for all other fields category.
field <- c(630, 105, 72, 55, 65, 710, 665, 590, 60, 718, 999)


# MC Simulation: Well Data ------------------------------------------------

# Number of iterations
nrun <- 10^2

Drilled <- round(cdf.drill[findInterval(runif(length(all_months)*nrun),
                                        c(0, cdf.drill[,2])),1])
result <- matrix(0, nrow = sum(Drilled), ncol = 3)

# Loop counters
a <- 1 # WellID
b <- 1 # Tstep
c <- 1 # runID

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
  if (b >= 180) {
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

# Royalties
rsim <- matrix(0, nrow = nrow(wsim), ncol = length(all_months))


