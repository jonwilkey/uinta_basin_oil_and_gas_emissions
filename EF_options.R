# Script Info -------------------------------------------------------------
# Name:      EF_options.R (Emission factor options script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com

# Description -------------------------------------------------------------

# This script creates a list object named "eopt" that contains the options for 
# all the inputs/outputs that control the execution of emissions calculations
# scripts and functions. Review each input/output below and change as desired
# from their base values.


# Global Options ----------------------------------------------------------

# Define "eopt" list object - this must exist in order to set any other options
eopt <- NULL


# Well Completions --------------------------------------------------------

# Data Analysis

# Flare control types - possible entries in this column (from drop down list)
# and their corresponding EF reduction
eopt$wc.ctrl <- data.frame(type = c("No Control",
                                    "Flare Efficiency - 60%",
                                    "Flare Efficiency - 70%",
                                    "Flare Efficiency - 80%",
                                    "Flare Efficiency - 90%",
                                    "Flare Efficiency - 95%"),
                           red = c(0, 0.6, 0.7, 0.8, 0.9, 0.95))

# Emission Factors (lb/gal)
eopt$wc.EF <- data.frame(EF.pm10 = 0.00683829365079365,
                         EF.pm25 = 0.00683829365079365,
                         EF.nox =  0.314561507936508,
                         EF.voc =  0.218825396825397,
                         EF.co =   0.118530423280423)