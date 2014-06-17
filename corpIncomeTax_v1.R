# Script Info -------------------------------------------------------------
# corpIncomeTax_v1.R (Corporate income taxes conversion factor)
# Version 1
# 05/29/14
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


# Load required data files ------------------------------------------------

# DOGM data
load(file.path(data_root, "production.rda"))

# Oil & gas price history
load(file.path(data_root, "oil_and_gas_price_history_1999_to_2012.rda"))


# User Inputs -------------------------------------------------------------

# Net taxable income (2011, 2010, 2009)
net <- c(220215146, 209171843, 66341510)

# 