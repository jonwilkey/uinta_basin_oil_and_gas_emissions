# Script Info -------------------------------------------------------------
# schedule_v1.R (Conventional Oil and Gas Model)
# Version 1
# 05/19/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1 -Loads production.rda and histdata.rda databases. Extracts actual drilling
#     schedule between 1999-01-01 and 2013-12-01 for oil and gas wells.
#     Generates CDF for drilling rates, field numbers, and landownership.
#     Determines probability of being a given well being a gas or oil well.
#     Finally, exports all of the results as set of dataframes named
#     "cdf_schedule_v1.rda".


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"
# Functions directory
fin <- "D:/Dropbox/CLEAR/DOGM Data/Functions"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(work_root)


# Functions ---------------------------------------------------------------
# List of functions used in this script to be loaded here
flst <- file.path(fin,c("write_excel.R"))

# Load each function in list
for (f in flst) source(f)

# Remove temporary variables
remove(fin, flst, f)


# Libraries ---------------------------------------------------------------
library(sqldf)
library(zoo)


# Load required data files ------------------------------------------------
# Load *.rda files
load(file.path(data_root, "production.rda"))
load(file.path(data_root, "histdata.rda"))

# Rename dataframes for brevity
p <- production
h <- histdata
remove(production, histdata)


# Other Inputs ------------------------------------------------------------
# Create a complete set of months between 1999-01-01 and 2012-01-01.
all_months <- seq(from = as.Date("1999-01-01"),
                  to = as.Date("2012-12-01"),
                  by = "months")

# Field Selection (i.e. fields that will be analyzed individually). Note that
# Field "999" is catch-all other category for every field not listed
# individually.
field <- c(630, 105, 72, 55, 65, 710, 665, 590, 60, 718, 999)


# Drilling Schedule: Actual -----------------------------------------------

# Data Prep ********************************************************************

# Merge p and h
p <- subset(p, w_county == "UINTAH" | w_county == "DUCHESNE")
m <- merge(p, h, by.x = "p_api", by.y = "h_api")

# Add column 'prod_date' as truncated-to-month version of h_first_prod
m$prod_date <- as.Date(as.yearmon(m[,"h_first_prod"]))

# Create dataframe containing dates of 1st production for each unique APD # and
# the field it is located in
well <- sqldf("select distinct(p_api), prod_date, w_field_num, w_well_type,
              w_surfowntyp, h_td_md, w_surfowntyp
              from m
              order by prod_date")

# Drop NA observations
well <- na.omit(well)

# Only wells between 1999-01-01 and 2013-12-01, and with depths > 0
well <- subset(well, prod_date >= as.Date("1999-01-01") &
                     prod_date <= all_months[length(all_months)] &
                     h_td_md > 0)


# wsim & psim for actual data ---------------------------------------------
# This segment shuffles the actual DOGM data into the same format used in
# conventional_v10.R's "wsim" & "psim" data.table/matrix.

# === psim ===

# Initial definition of psim
psim <- data.frame(all_months)

# WARNING - very slow loop. This loop creates a subset of m for each unique well
# in "well," then picks which columns to keep based on well type. Finally, the 
# loop merges the results from the current well with the results from all
# previous iterations and renames the columns as it goes.
for (i in 1:nrow(well)) {
  temp <- unique(subset(m,
                        subset = m$p_api == well$p_api[i],
                        select = c("p_rpt_period", "p_oil_prod", "p_gas_prod",
                                   "w_well_type")))
  if (temp$w_well_type[1] == "OW") {
    temp <- temp[,c("p_rpt_period", "p_oil_prod")]
  } else {
    temp <- temp[,c("p_rpt_period", "p_gas_prod")]
  }  
  psim <- merge(x = psim, y = temp,
                by.x = "all_months", by.y = "p_rpt_period",
                all.x = TRUE)
  names(psim)[i+1] <- i
}

# Transform from data.frame into matrix
psim <- as.matrix(psim[,2:(nrow(well)+1)])

# Transpose so that matrix rows = wells and columns = timesteps
psim <- t(psim)

# === wsim ===

# Relabel p_api as wellID number
well$p_api <- seq(1:nrow(well))

# Define new column "runID" as runID == 1 for all wells
runID <- rep(1, times = nrow(well))

# Conventiently, use runID to predefine space for "tDrill", which holds timestep
# that each well is drilled in
tDrill <- runID

# Match indices where wells were drilled (prod_date) with equivalent timesteps
# in "all_months"
for (i in 1:length(all_months)) {
  ind <- which (well$prod_date == all_months[i])
  tDrill[ind] <- i
}

# Create new vector of field numbers
fieldnum <- well$w_field_num

# Find the indices of any wells not located in one of the 10 fields being
# tracked individually as defined in the "field" vector
ind <- which(well$w_field_num != field[1] &
             well$w_field_num != field[2] &
             well$w_field_num != field[3] &
             well$w_field_num != field[4] &
             well$w_field_num != field[5] &
             well$w_field_num != field[6] &
             well$w_field_num != field[7] &
             well$w_field_num != field[8] &
             well$w_field_num != field[9] &
             well$w_field_num != field[10])

# Replace all such fields with the catch-all field "999"
fieldnum[ind] <- 999

# Create new vector for landownership and retype as numeric vector
landown <- as.numeric(well$w_surfowntyp.1)

# Get maximum production rate for each well
acoef <- apply(psim.actual, MARGIN = 1, FUN = max)

# Generate dataframe for export
wsim.actual <- data.frame(well$p_api, tDrill, runID, well$w_well_type, fieldnum,
                          well$h_td_md, landown, acoef)

# Rename to match wsim format
names(wsim.actual) <- c("wellID", "tDrill", "runID", "wellType", "fieldnum",
                        "depth", "landOwner", "acoef")

# ********************************************************************

# Select only those wells in "well" dataframe which are oil wells
well.ow <- subset(well, w_well_type == "OW")

# Predefine space for matrix with oil well drilling schedule (last column is for
# "other" field category)
schedule.ow <- matrix(0, nrow = length(all_months), ncol = length(field)-1)

# Create matrix with # of APDs approved by month (rows) in each field (columns)
for (i in 1:length(all_months)) {
  temp <- subset(well.ow, prod_date == all_months[i])
  for (j in 1:(length(field)-1)) {
    schedule.ow[i,j] <- length(which(temp$w_field_num == field[j]))
  }
  schedule.ow[i,ncol(schedule.ow)] <- nrow(temp) - sum(schedule.ow[i,1:(length(field)-1)])
}

# Gas Wells ********************************************************************

# Select only those wells in "well" dataframe which are gas wells
well.gw <- subset(well, w_well_type == "GW")

# Predefine space for matrix with gas well drilling schedule
schedule.gw <- matrix(0, nrow = length(all_months), ncol = length(field)-1)

# Create matrix with # of APDs approved by month (rows) in each field (columns)
for (i in 1:length(all_months)) {
  temp <- subset(well.gw, prod_date == all_months[i])
  for (j in 1:(length(field)-1)) {
    schedule.gw[i,j] <- length(which(temp$w_field_num == field[j]))
  }
  schedule.gw[i,ncol(schedule.gw)] <- nrow(temp) - sum(schedule.gw[i,1:(length(field)-1)])
}


# Drilling Rate -----------------------------------------------------------

# Determine total number of wells drilled each month
drill.hist <- sqldf("select prod_date, count(p_api) from well group by prod_date")

# Create PDF from density()
pdf.drill <- density(drill.hist[,2], from = 0)

# Generate CDF from cumsum()
cdf.drill <- cumsum(pdf.drill$y * diff(pdf.drill$x[1:2]))

# Normalize
cdf.drill <- cdf.drill / max(cdf.drill)

# Save as dataframe and rename columns
cdf.drill <- data.frame(pdf.drill$x, cdf.drill)
names(cdf.drill) <- c("x", "y")


# Fraction Gas/Oil --------------------------------------------------------

prob.gas <- sum(rowSums(schedule.gw))/nrow(well)


# Field Fractions ---------------------------------------------------------

# Oil Wells ********************************************************************

# Field count oil (fco) = # of unique well APIs in each field that are oil wells
fco <- sqldf("select w_field_num, count(p_api) from well where w_well_type = 'OW' group by w_field_num")

# Predefine field fraction oil (ffo)
ffo <- rep(0, length.out = length(field))

# Pick out wells from fco that match given field number
for (i in 1:(length(field)-1)) {
  ffo[i] <- fco[which(fco[,1] == field[i]),2]
}
ffo[length(ffo)] <- sum(fco[,2]) - sum(ffo[1:(length(ffo)-1)])

# From counts to fraction
ffo <- ffo/sum(fco[,2])

# Calculate CDF
cdf.ffo <- cumsum(ffo)

# Normalize
cdf.ffo <- cdf.ffo/max(cdf.ffo)

# Turn into dataframe for export
cdf.ffo <- data.frame(field, cdf.ffo)
names(cdf.ffo) <- c("Field", "CDF")

# Gas Wells ********************************************************************

# Repeat above steps for gas wells
fcg <- sqldf("select w_field_num, count(p_api) from well where w_well_type = 'GW' group by w_field_num")
ffg <- rep(0, length.out = length(field))

# since some fields don't exist in fcg that do in vector "field", manually
# define loop steps
step <- c(1, 2, 3, 4, 6, 7, 8)
for (i in step) {
  ffg[i] <- fcg[which(fcg[,1] == field[i]),2]
}
ffg[length(ffg)] <- sum(fcg[,2]) - sum(ffg[1:(length(ffg)-1)])
ffg <- ffg/sum(fcg[,2])
cdf.ffg <- cumsum(ffg)
cdf.ffg <- cdf.ffg/max(cdf.ffg)
cdf.ffg <- data.frame(field, cdf.ffg)
names(cdf.ffg) <- c("Field", "CDF")


# Landownership -----------------------------------------------------------

# Pull landownership stats from well to get counts of unique wells in each type 
# of land category in each field. Land types are (1) Federal, (2) Indian, (3) 
# State, (4) Fee. Fraction surfacte land (fsl) matrix has rows = field # and
# columns = landowner type.
fsl <- matrix(0, nrow = length(field), ncol = 4)
for (i in 1:(nrow(fsl)-1)) {
  temp <- subset(well, w_field_num == field[i])
  for (j in 1:ncol(fsl)) {
    fsl[i,j] <- length(which(temp$w_surfowntyp == j))
  }
}

# To handle the "other" field category
temp <- subset(well, subset = (w_field_num != field[1] &
                               w_field_num != field[2] &
                               w_field_num != field[3] &
                               w_field_num != field[4] &
                               w_field_num != field[5] &
                               w_field_num != field[6] &
                               w_field_num != field[7] &
                               w_field_num != field[8] &
                               w_field_num != field[9] &
                               w_field_num != field[10]))
for (j in 1:ncol(fsl)) {
  fsl[nrow(fsl),j] <- length(which(temp$w_surfowntyp == j))
}

# Divide by row sums to turn counts into fractions
fsl <- fsl/rowSums(fsl)

# Apply cumulative sum to each row and normalize
for (i in 1:nrow(fsl)) {
  fsl[i,] <- cumsum(fsl[i,])
  fsl[i,] <- fsl[i,]/max(fsl[i,])
}

# Transform to dataframe for export
cdf.fsl <- data.frame(field, fsl)
names(cdf.fsl) <- c("Field", "Federal", "Indian", "State", "Fee")


# Export Results ----------------------------------------------------------

# Save PDF & CDF results to "cdf_schedule_v1.rda"
save(file=file.path(data_root, "cdf_schedule_v1.rda"),
     list=c("cdf.drill",
            "cdf.ffg",
            "cdf.ffo",
            "cdf.fsl",
            "prob.gas"))

# Save wsim.actual data.frame to "wsim_actual.rda"
save(file=file.path(data_root, "wsim_actual_v1.rda"), list=c("wsim.actual"))

# Save psim.actual matrix to "psim_actual.rda"
save(file=file.path(data_root, "psim_actual_v1.rda"), list=c("psim.actual"))