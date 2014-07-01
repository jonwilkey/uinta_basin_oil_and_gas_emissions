# Script Info -------------------------------------------------------------
# gasPlantDataPrep_v1.R (Gas Plant Data Preparation)
# Version 1
# 06/30/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1 -Loads *.dbf files from DOGM in raw data folder as data.tables, sets keys
#     sets keys in each table, merges, converts to dataframe, exports result as
#     *.rda file.

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
# Raw data directory
raw_root <- "D:/Dropbox/CLEAR/DOGM Data/Raw Data"


# Libraries ---------------------------------------------------------------

library(foreign)
library(data.table)

# Load .dbf files ---------------------------------------------------------

# summ  <- data.table(read.dbf(file.path(raw_root, "PLNTSUMM.dbf"), as.is = TRUE))
# liq   <- data.table(read.dbf(file.path(raw_root, "PLNTPROD.dbf"), as.is = TRUE))
# alloc <- data.table(read.dbf(file.path(raw_root, "PLNTALOC.dbf"), as.is = TRUE))
# loc   <- data.table(read.dbf(file.path(raw_root, "PLNTLOC.dbf"),  as.is = TRUE))
# op    <- data.table(read.dbf(file.path(raw_root, "PLNTOPER.dbf"), as.is = TRUE))

summ  <- read.dbf(file.path(raw_root, "PLNTSUMM.dbf"), as.is = TRUE)
liq   <- read.dbf(file.path(raw_root, "PLNTPROD.dbf"), as.is = TRUE)
alloc <- read.dbf(file.path(raw_root, "PLNTALOC.dbf"), as.is = TRUE)
loc   <- read.dbf(file.path(raw_root, "PLNTLOC.dbf"),  as.is = TRUE)
op    <- read.dbf(file.path(raw_root, "PLNTOPER.dbf"), as.is = TRUE)

# Set keys ----------------------------------------------------------------

setkey(summ,  PLANT_CD)
setkey(liq,   PLANT_CD)
setkey(alloc, PLANT_CD)
setkey(loc,   PLANT_CD, ACCT_NUM, ALT_ADDRES)
setkey(op,    ACCT_NUM, ALT_ADDRES)


# Merge data.tables -------------------------------------------------------


