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

# -- Data Analysis --
# No inputs required

# -- Calculation Inputs --
# Emission Factors (lb/gal)
eopt$wc.EF <- data.frame(pm10 = 0.00683829365079365,
                         pm25 = 0.00683829365079365,
                         nox =  0.314561507936508,
                         voc =  0.218825396825397,
                         co =   0.118530423280423)


# RICE & Turbines ---------------------------------------------------------

# No inputs or calculation options (modeled directly from CPT table)


# Separators and Heaters --------------------------------------------------

# Data Analysis
# No inputs required

# Emission Factors (lb/MMCF)
eopt$sh.EF <- data.frame(pm10 = 7.6,
                         pm25 = 7.6,
                         sox =  0.6,
                         nox =  100,
                         voc =  5.5,
                         co =   84)


# Dehydrators -------------------------------------------------------------

# Data Analysis


# Emission Factors (lb/MMCF)
eopt$dh.EF <- data.frame(nox =       0.068,
                         co =        0.31,
                         voc.pilot = 5.5,
                         nox.pilot = 100,
                         co.pilot =  84)


# Tanks -------------------------------------------------------------------

# Data Analysis


# Emission Factors (lb/MMCF)
eopt$tank.EF <- data.frame(nox =       0.068,
                           co =        0.31,
                           voc.pilot = 5.5,
                           nox.pilot = 100,
                           co.pilot =  84)


# Truck Loading -----------------------------------------------------------

# No inputs or calculation options (modeled directly from CPT table)


# Pneumatic Controllers ---------------------------------------------------

# Emission factors (ton/hr)
eopt$pctrl.EF <- data.frame(HB.voc = 1.92  / 365 / 24,
                            IB.voc = 0.698 / 365 / 24,
                            LB.voc = 0.072 / 365 / 24)


# Pneumatic Pumps ---------------------------------------------------------

# No inputs or calculation options (modeled directly from CPT table)


# Fugitive emissions ------------------------------------------------------

# Emission factors
#..............................................................................................
#                    Service Type   Valves     Pumps    Others  Connectors  Flanges  Open-Ended Lines
#..............................................................................................
eopt$fug.EF <- data.frame(gas =  c(9.90E-03, 5.30E-03, 1.94E-02, 4.40E-04, 8.60E-04, 4.40E-03),
                          hoil = c(1.90E-05, 0.00E+00, 7.10E-05, 1.70E-05, 8.60E-07, 3.10E-04),
                          loil = c(5.50E-03, 2.87E-02, 1.65E-02, 4.60E-04, 2.40E-04, 3.10E-03),
                          woil = c(2.20E-04, 5.30E-05, 3.09E-02, 2.40E-04, 6.40E-06, 5.50E-04),
                          row.names = c("Valves",
                                        "Pumps",
                                        "Others",
                                        "Connectors",
                                        "Flanges",
                                        "Open-Ended Lines"))

# Default equipment counts
#..........................................................................................
#                        Service Type  m.VOC Valve Pumps Other Conn. Flan. Open-Ended Lines
#..........................................................................................
eopt$fug.def.eq <- data.frame(gas =  c(0.25,   15,    0,   25, 1000,   25,    1),
                              hoil = c(0.10,   10,    0,   10,  600,   10,    5),
                              loil = c(1.00,   10,    0,   10,  600,   10,    5),
                              woil = c(1.00,    5,    0,    5,  200,    0,    2),
                              row.names = c("m.voc",
                                            "Valves",
                                            "Pumps",
                                            "Others",
                                            "Connectors",
                                            "Flanges",
                                            "Open-Ended Lines"))
