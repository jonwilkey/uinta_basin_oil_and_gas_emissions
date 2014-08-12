# Script Info -------------------------------------------------------------
# drillSchedModel.R (Drilling Schedule Model Fitting Tool)
# Jon Wilkey


# Inputs ------------------------------------------------------------------

# All hard-coded:

# basis - CPI value for inflation adjustment

# inc - Time interval over which wells drilled / energy prices are summed /
# averaged, respectively

# test - dummy name for set of data to be tested, enter either analysis.aw,
# analysis.ow, or analysis.gw for analyzing all wells, oil wells, or gas wells.


# Outputs -----------------------------------------------------------------

# lm() summaries of linear regression on selected dataset using (1) the full 
# dataset, and (2) the first half of the dataset (for cross-validation
# purposes). Additionally, exports well counts and energy prices to clipboard.


# Description -------------------------------------------------------------

# This script was written for the purposes of analyzing the relationship between
# the # of wells drilled and energy prices, specifically:

# [1] (wells drilled) = a*(oil price)+b*(gas price)+c

# This script provides the functionality for fitting equation [1] to datasets of
# (1) all wells, (2) oil wells, and (3) gas wells. Finally, the script allows
# for getting this information any whole numbered time increment (quarterly,
# biannually, annually).

# Instructions: Run the first segment of the code at start, then only run the 
# "Analysis" segment of the code from line 170 on. Note that input options are
# hard-coded.


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------

# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Functions directory
fin <- "C:/Users/Jon/Documents/R/ub_oilandgas/Functions"
# Working directory
work_root <- "C:/Users/Jon/Documents/R/ub_oilandgas/"

# Set working directory
setwd(work_root)


# Functions ---------------------------------------------------------------
# List of functions used in this script to be loaded here
flst <- file.path(fin,c("inf_adj.R",
                        "write_excel.R"))

# Load each function in list
for (f in flst) source(f)


# Libraries ---------------------------------------------------------------
library(sqldf)
library(zoo)

# Load required data files ------------------------------------------------

# Oil & gas price history
load(file.path(data_root, "oil_and_gas_price_history_1999_to_2012.rda"))

# Load DOGM *.rda files
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

# CPI value for inflation adjustment
basis <- 233.049


# Drilling Schedule: Actual -----------------------------------------------

# Merge p and h
p <- subset(p, w_county == "UINTAH" | w_county == "DUCHESNE")
m <- merge(p, h, by.x = "p_api", by.y = "h_api")

# Add column 'prod_date' as truncated-to-month version of h_first_prod
m$prod_date <- as.Date(as.yearmon(m[,"h_first_prod"]))

# Create dataframe containing dates of 1st production for each unique APD # and
# the field it is located in
well <- sqldf("select distinct(p_api), prod_date, w_field_num, w_well_type,
              w_surfowntyp, h_td_md
              from m
              order by prod_date")

# Drop NA observations
well <- na.omit(well)

# Only wells between 1999-01-01 and 2012-12-01, and with depths > 0
well <- subset(well, prod_date >= as.Date("1999-01-01") &
                 prod_date <= all_months[length(all_months)] &
                 h_td_md > 0)

# Determine total number of wells drilled each month for all wells (aw), oil
# wells (ow), and gas wells (gw)
drill.aw <- sqldf("select prod_date, count(p_api) from well group by prod_date")
drill.gw <- sqldf("select prod_date, count(p_api) from well where w_well_type = 'GW' group by prod_date")


# Energy price data prep --------------------------------------------------

# Define prices and adjust for inflation - op = oil price, gp = gas price. Given
# basis (233.049) inflation adjusts to 2013-12-01.
op <- inf_adj(OGprice$bw, OGprice$cpi, basis)
gp <- inf_adj(OGprice$uswp, OGprice$cpi, basis)


# Analysis ----------------------------------------------------------------
# Create dataframes with columns of (1) wells drilled, (2) oil prices, and (3) 
# gas prices for each of the different drill.xx queries. Note that there is a 
# problem with the drill.ow query being shorter than the others (there are ~7 
# months in which no oil wells were drilled and the rows for those months have 
# been omitted). The current workaround is to get oil wells drilled by taking
# difference between aw and gw well counts.
analysis.aw <- data.frame(drill.aw[,2], op, gp)
analysis.gw <- data.frame(drill.gw[,2], op, gp)
analysis.ow <- data.frame(drill.aw[,2]-drill.gw[,2], op, gp)

# Rename
names(analysis.aw) <- c("wells", "op", "gp")
names(analysis.gw) <- c("wells", "op", "gp")
names(analysis.ow) <- c("wells", "op", "gp")

# Enter time interval for fitting over (i.e. # of months to sum wells drilled
# over and take mean energy prices over)
inc <- 12

# Enter which "analysis" data.frame to use
test <- analysis.aw

# Predefine result matrix
result <- NULL

# Get data in desired inc intervals
for (i in 1:(168/inc)) {
  if (i == 1) {
    temp <- c(sum(test$wells[(inc*(i-1)+1):(inc*i)]),
              mean(test$op[(inc*(i-1)+1):(inc*i)]),
              mean(test$gp[(inc*(i-1)+1):(inc*i)]),
              0)
    result <- rbind(result, temp)
  } else{
    temp <- c(sum(test$wells[(inc*(i-1)+1):(inc*i)]),
              mean(test$op[(inc*(i-1)+1):(inc*i)]),
              mean(test$gp[(inc*(i-1)+1):(inc*i)]))
    temp <- c(temp, result[(i-1),1])
    result <- rbind(result, temp)
  }
}

# Have to manually edit results data.frame to deal with first prior row:
#         AW  OW GW
# Month [ 17  1  16 ]
# Quart [ 56  8  48 ]
# Biann [ 109 21 88 ]
# Annua [ 214 51 163]

result[1,4] <- 214

# Convert to data.frame
result <- as.data.frame(result, row.names=as.character(1:nrow(result)))

# Add column names
names(result) <- c("wells", "op", "gp", "prior")

# Fit and print summary of linear regression
summary(lm(wells~op+gp+prior,result))                      # Full Fit
summary(lm(wells~op+gp+prior,result[1:(nrow(result)/2),])) # Cross-Validation Fit

# Copy result data.frame to clipboard for export to spreadsheet
write.excel(result)