# declineCurve.R

# Inputs ------------------------------------------------------------------

# 


# Outputs -----------------------------------------------------------------

# 


# Description -------------------------------------------------------------

# blah


# Options -----------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------

# # Windows
# data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"        # Prepared data
# plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"                # Plots
# fin <-       "C:/Users/Jon/Documents/R/ub_oilandgas/Functions" # Functions
# work_root <- "C:/Users/Jon/Documents/R/ub_oilandgas/"          # Working dir.

# Mac
data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"        # Prepared data
plot_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"                # Plots
fin <-       "/Users/john/Documents/ub_oilandgas/ub_oilandgas/Functions" # Functions
work_root <- "/Users/john/Documents/ub_oilandgas/ub_oilandgas"          # Working dir.

# Set working directory
setwd(work_root)


# Functions ---------------------------------------------------------------
# # List of functions used in this script to be loaded here
# flst <- file.path(fin,c("write_excel.R"))
# 
# # Load each function in list
# for (f in flst) source(f)


# Libraries ---------------------------------------------------------------
library(minpack.lm)
library(sqldf)


# Other Inputs ------------------------------------------------------------

# Field Listing
field <- c(630, 105, 72, 55, 65, 710, 665, 590, 60, 718)


# Load required data files ------------------------------------------------

# Load the dataframe named 'production'
load(file.path(data_root, "production.rda"))


# Subset data -------------------------------------------------------------

# Pull desired columns out of production, remove original from workspace
p <- subset(production,
            subset = (time != 0 &                # First month can be partial
                      (w_county == "UINTAH" |    # Only Uintah & Duchesne
                       w_county == "DUCHESNE")), # counties
            select = c("p_api",
                       "p_oil_prod",
                       "p_gas_prod",
                       "p_water_prod",
                       "time",
                       "w_county",
                       "w_well_type",
                       "w_field_num"))

# Omit NAs (none at present, but just in case)
p <- na.omit(p)