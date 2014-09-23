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

# Windows
path$raw  <- "D:/Dropbox/CLEAR/DOGM Data/Raw Data"             # Raw data
path$data <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"        # Prepared data
path$plot <- "D:/Dropbox/CLEAR/DOGM Data/Plots"                # Plots
path$fun  <- "C:/Users/Jon/Documents/R/ub_oilandgas/Functions" # Functions
path$work <- "C:/Users/Jon/Documents/R/ub_oilandgas/"          # Working dir.
path$look <- "D:/Dropbox/CLEAR/DOGM Data/Lookup Tables"        # Lookup Tables

# # Mac
# path$raw  <- "/Users/john/Dropbox/CLEAR/DOGM Data/Raw Data"
# path$data <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"
# path$plot <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"
# path$fun  <- "/Users/john/Documents/ub_oilandgas/ub_oilandgas/Functions"
# path$work <- "/Users/john/Documents/ub_oilandgas/ub_oilandgas"
# path$look <- "/Users/john/Dropbox/CLEAR/DOGM Data/Lookup Tables"

# Set working directory
setwd(path$work)


# Functions ---------------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(path$fun, c("dogmDataUpdate.R",
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
                              "write_excel.R"))

# Load each function in list then remove temporary file list variables
for (f in flst) source(f); remove(f, flst)


# Libraries ---------------------------------------------------------------
library(foreign)
library(plyr)
library(zoo)
library(data.table)
library(sqldf)


# Options -----------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

# Run script "IO_options.R" to load user defined input/output options
source("IO_options.R")


# 2.1 DOGM *.dbf database file processing ---------------------------------

# Run function if opt$DOGM.update flag is set to "TRUE"
if(opt$DOGM.update == TRUE) {
  dogmDataUpdate(path = path)
}

# Load production.rda and rename as "p" for brevity
load(file.path(path$data, "production.rda"))
p <- production
remove(production)

# Pullout subset of p based on criteria in opt$keeps
p <- subset(p, subset = (w_county == "UINTAH" | w_county == "DUCHESNE"),
            select = opt$p.keep)


# 2.2 Drilling schedule CDF generation ------------------------------------

# Run function if opt$schedule.update flag is set to "TRUE"
if(opt$schedule.update == TRUE) {
  scheduleUpdate(path = path,
                 p = p,
                 tsteps = opt$tsteps)
}
