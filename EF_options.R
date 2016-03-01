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
#.........................................................
#                       Control Type            Reduction
#.........................................................
eopt$wc.ctrl <- rbind(c("No Control",             0.00),
                      c("Flare Efficiency - 60%", 0.60),
                      c("Flare Efficiency - 70%", 0.70),
                      c("Flare Efficiency - 80%", 0.80),
                      c("Flare Efficiency - 90%", 0.90),
                      c("Flare Efficiency - 95%", 0.95))

# Reformat into data.frame
eopt$wc.ctrl <- data.frame(type = eopt$wc.ctrl[,1],
                           red =  as.integer(eopt$wc.ctrl[,2]))

# Emission Factors (lb/gal)
eopt$wc.EF <- data.frame(pm10 = 0.00683829365079365,
                         pm25 = 0.00683829365079365,
                         nox =  0.314561507936508,
                         voc =  0.218825396825397,
                         co =   0.118530423280423)


# RICE & Turbines ---------------------------------------------------------

# Emission Factors
#.................................................................................................................................................
#                     Turbine/Compressor/Engine Type                      Type #   PM-10    PM-2.5     SOx       NOx       VOC       CO        CH2O
#.................................................................................................................................................
eopt$rt.EF <- rbind(c("Natural Gas-Fired Turbine",                           1,  6.60E-03, 6.60E-03, 3.40E-03, 3.20E-01, 2.10E-03, 8.20E-02, 7.10E-04),
                    c("Distillate Oil-Fired Turbine",                        2,  1.20E-02, 1.20E-02, 3.30E-02, 8.80E-01, 4.10E-04, 3.30E-03, 2.80E-04),
                    c("Natural Gas 2-Stroke Lean Burn",                      3,  3.84E-02, 3.84E-02, 5.88E-04, 3.17E+00, 1.20E-01, 3.86E-01, 5.52E-02),
                    c("Natural Gas 4-Stroke Lean Burn",                      4,  7.71E-05, 7.71E-05, 5.88E-04, 4.08E+00, 1.18E-01, 3.17E-01, 5.28E-02),
                    c("Natural Gas 4-Stroke Rich Burn",                      5,  9.50E-03, 9.50E-03, 5.88E-04, 2.21E+00, 2.96E-02, 3.72E+00, 2.05E-02),
                    c("Diesel Industrial Engine <600 hp",                    6,  3.10E-01, 3.10E-01, 2.90E-01, 4.41E+00, 3.60E-01, 9.50E-01, 1.18E-03),
                    c("Gasoline Industrial Engine <250 hp",                  7,  1.00E-01, 1.00E-01, 8.40E-02, 1.63E+00, 3.03E+00, 6.27E+01, NA),
                    c("Diesel Large Industrial Engine >600 hp",              8,  1.00E-01, 1.00E-01, 3.30E-02, 3.20E+00, 9.00E-02, 8.50E-01, 7.89E-05),
                    c("Duel Fuel Large Industrial Engine >600 hp",           9,  0.00E+00, 0.00E+00, 4.87E-03, 2.70E+00, 8.00E-01, 1.16E+00, NA),
                    c(">12/1/2011, 100 ≤ hp < 500",                          10, NA,       NA,       NA,       1,        0.7,      2,        NA),
                    c(">7/1/2010, hp ≥ 500",                                 11, NA,       NA,       NA,       1,        0.7,      2,        NA),
                    c( ">7/1/2008 to 12/31/2010, 100 ≤ hp < 500",            12, NA,       NA,       NA,       2,        1,        4,        NA),
                    c(">7/1/2007 to 7/30/2010, hp ≥ 500",                    13, NA,       NA,       NA,       2,        1,        4,        NA),
                    c("Emergency >1/1/2009, hp > 500",                       14, NA,       NA,       NA,       2,        1,        4,        NA),
                    c("Emergency >1/1/2009, 130 ≤ hp < 500",                 15, NA,       NA,       NA,       2,        1,        4,        NA),
                    c("Mod/Reconst: >6/12/2006 to <7/1/2007, 25 < hp < 500", 16, NA,       NA,       NA,       3,        1,        4,        NA),
                    c("Mod/Reconst: >6/12/2006 to <7/1/2007, hp ≥ 500",      17, NA,       NA,       NA,       3,        1,        4,        NA),
                    c(">7/1/2008, 25 < hp < 100",                            18, NA,       NA,       NA,       3,        1,        4,        NA),
                    c("Emergency > 1/1/2009, 25 < hp < 100",                 19, NA,       NA,       NA,       10,       NA,       387,      NA),
                    c("Emergency >1/1/2009, 100 ≤ hp < 130",                 20, NA,       NA,       NA,       10,       NA,       387,      NA))

# Reformat into data.frame
eopt$rt.EF <- data.frame(name = eopt$rt.EF[,1],
                         type = as.integer(eopt$rt.EF[,2]),
                         pm10 = as.numeric(eopt$rt.EF[,3]),
                         pm25 = as.numeric(eopt$rt.EF[,4]),
                         sox =  as.numeric(eopt$rt.EF[,5]),
                         nox =  as.numeric(eopt$rt.EF[,6]),
                         voc =  as.numeric(eopt$rt.EF[,7]),
                         co =   as.numeric(eopt$rt.EF[,8]),
                         ch2o = as.numeric(eopt$rt.EF[,9]))


# Separators and Heaters --------------------------------------------------

# Data Analysis


# Emission Factors (lb/MMCF)
eopt$sh.EF <- data.frame(pm10 = 7.6,
                         pm25 = 7.6,
                         sox =  0.6,
                         nox =  100,
                         voc =  5.5,
                         co =   84)

# Low-NOx burner emissions reduction fraction
eopt$sh.nox.red <- 0.5


# Dehydrators -------------------------------------------------------------

# Data Analysis


# Emission Factors (lb/MMCF)
eopt$dh.EF <- data.frame(nox =       0.068,
                         co =        0.31,
                         voc =       5.2118, # Might need to replace with data analysis CDF
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

# Data Analysis


# Truck loading rack S factors
#...................................................................................
#                                   Mode of Operation                       S Factor
#...................................................................................
eopt$truck.s <- rbind(c("Submerged Loading of a clean cargo tank",            0.50),
                      c("Submerged loading: dedicated normal service",        0.60),
                      c("Submerged loading: dedicated vapor balance service", 1.00),
                      c("Splash loading of a clean cargo tank",               1.45),
                      c("Splash loading: dedicated normal service",           1.45),
                      c("Splash loading: dedicated vapor balance service",    1.00))

# Reformat into data.frame
eopt$truck.s <- data.frame(mode =     eopt$truck.s[,1],
                           s_factor = as.integer(eopt$truck.s[,2]))


# Pneumatic controllers ---------------------------------------------------

# Emission factors (ton/hr)
eopt$pctrl.EF <- data.frame(HB.voc = 1.92  / 365 / 24,
                            IB.voc = 0.698 / 365 / 24,
                            LB.voc = 0.072 / 365 / 24)

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
