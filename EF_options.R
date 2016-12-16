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

# Emission Factors (lb/gal)
eopt$wc.EF <- data.frame(pm10 = 0.00683829365079365,
                         pm25 = 0.00683829365079365,
                         nox  = 0.314561507936508,
                         voc  = 0.218825396825397,
                         co   = 0.118530423280423)

# -- Emission reductions --

# Explaination of terms (applies to all eopt$r$... data.frames):
#
# tDrill   - time step that well is drilled
#
# tstep    - time step that reduction is implemented
#
# wellType - required well type, options are:
#               "OW"  for oil wells
#               "GW"  for gas wells
#               "all" for both types
#
# juris    - required jurisdiction of surface lease, options are:
#               "federal" for Federal jurisdiction
#               "state"   for State jurisdiction
#               "fee"     for private fee lands jurisdiction
#               "indian"  for Indian jurisdiction
#               "all"     for any jurisdiction
#
# county   - required county, options are:
#               "UINTAH"   for Uintah county
#               "DUCHESNE" for Duchesne county
#               "all"      for any county
#
# a...     - threshold annual emissions of pollutant ... (e.g. pm10, voc, etc.)
#
# moil     - Minimum value of maximum oil production rate (bbl/month)
#
# mgas     - Minimum value of maximum gas production rate (MCF/month)

eopt$r$wc <- data.frame(tDrill   = calTstep(as.Date("2012-06-01")),
                        tstep    = calTstep(as.Date("2012-06-01")),
                        wellType = "all",
                        juris    = "all",
                        county   = "all",
                        apm10    = 0,
                        apm25    = 0,
                        anox     = 0,
                        avoc     = 0,
                        aco      = 0,
                        moil     = 0,   # Should be left at 0
                        mgas     = 0,   # Should be left at 0
                        red.pm10 = 0.5,
                        red.pm25 = 0.5,
                        red.nox  = 0.5,
                        red.voc  = 0.5,
                        red.co   = 0.5)


# RICE & Turbines ---------------------------------------------------------

# Emission reductions
eopt$r$rt <- data.frame(tDrill   = calTstep(as.Date("2012-06-01")),
                        tstep    = calTstep(as.Date("2012-06-01")),
                        wellType = "all",
                        juris    = "all",
                        county   = "all",
                        apm10    = 0,
                        apm25    = 0,
                        asox     = 0,
                        anox     = 0,
                        avoc     = 0,
                        aco      = 0,
                        ach2o    = 0,
                        moil     = 100,
                        mgas     = 1000,
                        red.pm10 = 0.5,
                        red.pm25 = 0.5,
                        red.sox  = 0.5,
                        red.nox  = 0.5,
                        red.voc  = 0.5,
                        red.co   = 0.5,
                        red.ch2o = 0.5)


# Separators and Heaters --------------------------------------------------

# Emission Factors (lb/MMCF)
eopt$sh.EF <- data.frame(pm10 = 7.6,
                         pm25 = 7.6,
                         sox  = 0.6,
                         nox  = 100,
                         voc  = 5.5,
                         co   = 84)

# Emission reductions
eopt$r$sh <- data.frame(tDrill   = calTstep(as.Date("2012-06-01")),
                        tstep    = calTstep(as.Date("2012-06-01")),
                        wellType = "all",
                        juris    = "all",
                        county   = "all",
                        apm10    = 0,
                        apm25    = 0,
                        asox     = 0,
                        anox     = 0,
                        avoc     = 0,
                        aco      = 0,
                        moil     = 100,
                        mgas     = 1000,
                        red.pm10 = 0.5,
                        red.pm25 = 0.5,
                        red.sox  = 0.5,
                        red.nox  = 0.5,
                        red.voc  = 0.5,
                        red.co   = 0.5)


# Dehydrators -------------------------------------------------------------

# Emission Factors (lb/MMCF)
eopt$dh.EF <- data.frame(nox       = 0.068,
                         co        = 0.31,
                         voc.pilot = 5.5,
                         nox.pilot = 100,
                         co.pilot  = 84)

# Emission reductions
eopt$r$dh <- data.frame(tDrill   = calTstep(as.Date("2012-06-01")),
                        tstep    = calTstep(as.Date("2012-06-01")),
                        wellType = "all",
                        juris    = "all",
                        county   = "all",
                        anox     = 0,
                        avoc     = 0,
                        aco      = 0,
                        moil     = 100,
                        mgas     = 1000,
                        red.nox  = 0.5,
                        red.voc  = 0.5,
                        red.co   = 0.5)


# Tanks -------------------------------------------------------------------

# Emission Factors (lb/MMCF)
eopt$tank.EF <- data.frame(nox       = 0.068,
                           co        = 0.31,
                           voc.pilot = 5.5,
                           nox.pilot = 100,
                           co.pilot  = 84)

# Emission reductions
eopt$r$tank <- data.frame(tDrill   = calTstep(as.Date("2012-06-01")),
                          tstep    = calTstep(as.Date("2012-06-01")),
                          wellType = "all",
                          juris    = "all",
                          county   = "all",
                          anox     = 0,
                          avoc     = 0,
                          aco      = 0,
                          moil     = 100,
                          mgas     = 1000,
                          red.nox  = 0.5,
                          red.voc  = 0.5,
                          red.co   = 0.5,
                          avoc     = 6) # Average annual VOC emissions (ton/yr) for reductions to apply


# Truck Loading -----------------------------------------------------------

# Emission reductions
eopt$r$truck <- data.frame(tDrill   = calTstep(as.Date("2012-06-01")),
                           tstep    = calTstep(as.Date("2012-06-01")),
                           wellType = "all",
                           juris    = "all",
                           county   = "all",
                           avoc     = 0,
                           moil     = 100,
                           mgas     = 1000,
                           red.voc  = 0.5)


# Pneumatic Controllers ---------------------------------------------------

# Emission factors (ton/hr)
eopt$pctrl.EF <- data.frame(HB.voc = 1.92  / 365 / 24,
                            IB.voc = 0.698 / 365 / 24,
                            LB.voc = 0.072 / 365 / 24)

# Emission reductions
eopt$r$pctrl <- data.frame(tDrill   = calTstep(as.Date("2012-06-01")),
                           tstep    = calTstep(as.Date("2012-06-01")),
                           wellType = "all",
                           juris    = "all",
                           county   = "all",
                           avoc     = 0,
                           moil     = 100,
                           mgas     = 1000,
                           red.voc  = 0.5)


# Pneumatic Pumps ---------------------------------------------------------

# Emission reductions
eopt$r$ppump <- data.frame(tDrill   = calTstep(as.Date("2012-06-01")),
                           tstep    = calTstep(as.Date("2012-06-01")),
                           wellType = "all",
                           juris    = "all",
                           county   = "all",
                           avoc     = 0,
                           moil     = 100,
                           mgas     = 1000,
                           red.voc  = 0.5)

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

# Emission reductions
eopt$r$fug <- data.frame(tDrill   = calTstep(as.Date("2012-06-01")),
                         tstep    = calTstep(as.Date("2012-06-01")),
                         wellType = "all",
                         juris    = "all",
                         county   = "all",
                         avoc     = 0,
                         moil     = 100,
                         mgas     = 1000,
                         red.voc  = 0.5)
