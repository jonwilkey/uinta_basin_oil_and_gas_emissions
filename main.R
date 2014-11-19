# Script Info -------------------------------------------------------------
# Name:      main.R (Conventional Oil and Gas Simulation Driver Script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Description -------------------------------------------------------------

# This script does everything. Structure is as follows:

# 1. Set simulation environment, including directory paths, functions, libraries
#    and loading user input/output options.
# 2. Process DOGM *.dbf database files


# Paths -------------------------------------------------------------------

# Predefine list object "path" for holding directory path listings
path <- NULL

# Path switch - uncomment and/or replace with the path directory for your local
# copy of the Git repository and Dropbox files.
  pwd.drop <- "D:/"                                  # Windows
  pwd.git  <- "C:/Users/Jon/Documents/R/"
# pwd.drop <- "/Users/john/"                         # Mac
# pwd.git  <- "/Users/john/Documents/ub_oilandgas/"
# pwd.drop <- "/home/slyleaf/"                       # Linux
# pwd.git  <- "/home/slyleaf/Documents/"
  
# Define paths.
# "raw"  is raw data (*.dbf files from DOGM, *.csv files, etc.). 
# "data" is prepared data files (typically *.rda).
# "look" is lookup tables. 
# "plot" is the directory for saving plot *.pdf files.
# "work" is the working directory where main.R and IO_options.R are located.
# "fun"  is the directory for all *.R functions.
path$raw  <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Raw Data", sep = "")
path$data <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Prepared Data", sep = "")
path$look <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Lookup Tables", sep = "")
path$plot <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Plots", sep = "")
path$work <- paste(pwd.git,  "ub_oilandgas/", sep = "")
path$fun  <- paste(pwd.git,  "ub_oilandgas/Functions", sep = "")

# Remove temporary path objects
remove(pwd.drop, pwd.git)

# Set working directory
setwd(path$work)


# Functions ---------------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(path$fun, c("dogmDataUpdate.R",
                              "scheduleUpdate.R",
                              "corpIncomeUpdate.R",
                              "leaseOpCostUpdate.R",
                              "drillingModel.R",
                              "GBMfit.R",
                              "welldata.R",
                              "productionsim.R",
                              "inf_adj.R",
                              "royalty.R",
                              "stax.R",
                              "ptax.R",
                              "RIMS.R",
                              "workload.R",
                              "GHG.R",
                              "water.R",
                              "GBMsim.R",
                              "clipboard.R"))

# Load each function in list then remove temporary file list variables
for (f in flst) source(f); remove(f, flst)


# Libraries ---------------------------------------------------------------
library(foreign)
library(plyr)
library(zoo)
library(data.table)
library(sqldf)
library(minpack.lm)


# Options -----------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

# Run script "IO_options.R" to load user defined input/output options
source("IO_options.R")


# Load Data Files ---------------------------------------------------------

# Load price history for oil and gas in the Uinta Basin according to Chevron 
# price history: (http://crudemarketing.chevron.com/posted_pricing_daily.asp). 
# These prices have been collected by a web-scraper, sorted by date, and matched
# with the CPI index values for each month into a single data.frame called 
# OGprice, which is used in several economic fits (drilling activity, corporate 
# income tax conversion factors, etc.). Presently there is no script that
# updates this data.frame...
load(file.path(path$data, "oil_and_gas_price_history_1999_to_2012.rda"))


# 2.1 DOGM *.dbf database file processing ---------------------------------

# Run function if opt$DOGM.update flag is set to "TRUE"
if(opt$DOGM.update == TRUE) {
  dogmDataUpdate(path = path,
                 version = opt$file_ver)
}

# Load production.rda and rename as "p" for brevity
load(file.path(path$data,
               paste("production_", opt$file_ver, ".rda", sep = "")))

# Make subset of "production" called "p" based on criteria in opt$keeps
p <- production
p <- subset(p, subset = switch(opt$psub,
                               a = {(w_county == "UINTAH" |
                                     w_county == "DUCHESNE")}),
            select = opt$p.keep)


# 2.2 Drilling schedule CDF generation ------------------------------------

# Run function if opt$schedule.update flag is set to "TRUE"
if(opt$schedule.update == TRUE) {
  scheduleUpdate(path = path,
                 p = p,
                 field = opt$field,
                 tsteps = opt$tsteps,
                 min.depth = opt$min.well.depth,
                 max.depth = opt$max.well.depth,
                 well.depth.step = opt$well.depth.step,
                 version = opt$file_ver)
}


# 2.3 Corporate income tax conversion factor CDF generation ---------------

# Run function if opt$corptax.update flag is set to "TRUE"
if(opt$corptax.update == TRUE) {
  corpIncomeUpdate(production =   production,
                   NTI =          opt$NTI,
                   CIrate.state = opt$CIrate.state,
                   CIrate.fed =   opt$CIrate.fed,
                   basis =        opt$cpi,
                   CI.pdf.min =   opt$CI.pdf.min,
                   CI.pdf.max =   opt$CI.pdf.max,
                   version =      opt$file_ver,
                   path =         path)
}


# 2.4 Lease opearting cost lm() fit update --------------------------------

# Run function if opt$lease.update flag is set to "TRUE"
if(opt$lease.update == TRUE) {
  leaseOpCostUpdate(path = path,
                    version = opt$file_ver,
                    tstart = opt$tstart,
                    tstop = opt$tstop,
                    full = opt$fullDataFit)
}


# 2.5 Drilling Schedule Model lm() fit update -----------------------------

# Run function if opt$drillmodel.update flag is set to "TRUE"
if(opt$drillmodel.update == TRUE) {
  drillingModel(path = path,
                p = p,
                EP.CPI.basis = opt$EP.CPI.basis,
                cpi = opt$cpi,
                min.depth = opt$min.well.depth,
                version = opt$file_ver)
}


# 2.6 GBM parameter fit update --------------------------------------------

# Run function if opt$GBMfit.update flag is set to "TRUE"
if(opt$GBMfit.update == TRUE) {
  GBMfit(path = path,
         EP.CPI.basis = opt$EP.CPI.basis,
         cpi = opt$cpi,
         version = opt$file_ver)
}
