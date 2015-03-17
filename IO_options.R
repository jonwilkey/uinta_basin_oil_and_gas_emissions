# Script Info -------------------------------------------------------------
# Name:      IO_options.R (Conventional Oil and Gas Simulation Options Script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com

# Description -------------------------------------------------------------

# This script creates a list object named "opt" that contains the options for 
# all the inputs/outputs that control the execution of the main.R script. Review
# each input/output below and change as desired from their base values.


# 1.0 Global Options ------------------------------------------------------

# Define "opt" list object - this must exist in order to set any other options
opt <- NULL

# Enter number of overall simulation iterations
opt$nrun <- 1e2

# Number of simulation time steps (in months)
opt$MC.tsteps <- 59

# Enter start and stop point for simulation in "YYYY-MM-DD" format by changing 
# values inside quotations of opt$tstart and opt$tstop. These values will be the
# default time range for all analysis and update functions unless over-ridden by
# options set for a specific function.
opt$tstart <- as.Date("2010-01-01")
opt$tstop  <- as.Date("2014-11-01")
opt$tsteps <- seq(from = opt$tstart, to = opt$tstop, by = "months")

# CPI value for inflation adjustment and it's associated date (as character
# string). Current value is for average 2014 USD.
opt$cpi <-     236.736
opt$cpiDate <- "2014"

# Version filename. If any of the update flags above is set to "TRUE", change
# the version number below so that previous *.rda versions will be retained.
opt$file_ver <- "v4"

# Quantiles vector (sequence of quantile values at which to estimate CDF). Used
# everywhere quantile() or qnorm() is used to generate CDF.
opt$xq <- seq(from = 0.0001, to = 0.9999, by = 0.0001)

# Conversion factor for switching from MCF of gas to MMBtu of gas. Equation: 
# (Factor in MMBtu/MCF) = (Median HV of gas in Btu/SCF) * (1e3 SCF / 1 MCF) * (1
# MMBtu / 1e6 Btu)
opt$cf.MCF.to.MMBtu <- (1081)*(1e3)*(1/1e6)

# Minimum well depth in feet. Used in scheduleUpdate and drillingModelUpdate for
# subsetting.
opt$min.well.depth <- 5e3

# Initial prices for EIA/GBM price path simulation (from last recorded EIA FPP). 
# Sources:
# [Gas 1] http://tonto.eia.gov/dnav/ng/hist/na1140_sut_3a.htm
# [Gas 2] http://www.eia.gov/dnav/ng/hist/n9190us3A.htm
# [Oil 1] http://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=f004049__3&f=a

# Note that gas FPP are calculated by converting US national average price (UT 
# gas prices are only recorded by EIA on annual basis; nation gas FPPs are 
# recorded on monthly basis). Values are in 'basis' inflation adjusted $ per bbl
# or MCF (for gas). Values below are for 2009-12-01.
opt$oil.fpp.init <- 72.10
opt$gas.fpp.init <- 4.90

# FPP date - enter month here associated with FPPs above
opt$FPPdate <- as.Date("2009-12-01")


# 1.1 Flags for updating prepared data files ----------------------------------

# This segment contains flags for updating the files contained in the prepared 
# data folder (typically *.rda files, probability distributions, linear 
# regression fits, etc.). If new data is available, save the raw data to the raw
# data folder, update the flag(s) for the appropriate script(s) below, save this
# script, and rerun main.R
#...............................................................................
#  Flag Name           Value                          Notes
#...............................................................................
opt$DOGM.update         <- F # Turns *.dbf files from DOGM () into single file (production.rda) used for all subsequent analysis
opt$schedule.update     <- F # Generates CDF for field numbers, lease type, well type, and well depth. Extracts actual drilling and production history from production.rda.
opt$lease.update        <- F # Fits lease operating cost model to EIA lease operating cost data.
opt$EIAprice.update     <- F # Converts *.csv file with historical EIA prices into data.frame and adjusts prices for inflation
opt$corptax.update      <- F # Generates corporate income tax coversion factor CDFs
opt$ptax.update         <- F # Updates property tax statistics
opt$drillmodel.update   <- F # Fits drilling schedule model to energy prices
opt$GBMfit.update       <- F # Fits GBM parameters "v" and "mu" to energy prices
opt$EIAforecast.update  <- F # Adjusts EIA forecast for inflation and converts to monthly basis, set to true if opt$forecast input is changed
opt$EIAerror.update     <- F # Generates CDFs for error in EIA AEO forecasts
opt$DCA.update          <- F # Fits decline curves
opt$field.DCA.update    <- F # Fits field level decline curves
opt$DCA.CDF.update      <- F # Generates CDFs from decline curve fits
opt$drillCapCost.update <- F # Runs regression fit on drilling and completion capital cost data
opt$water.update        <- F # Generates all CDFs and linear regression models for water balance terms


# 1.2 Subsetting options for production.rda file ------------------------------

# The following is a list of all unique column names in production.rda. Comment
# in/out as desired. Only uncommented columns will be to kept.
#...............................................................................
#                Column Name                          Notes
#...............................................................................
opt$p.keep <- c("p_api",        # API well number. All API numbers (American Petroleum Institute standard format for numbering) for Utah wells begin with 43 (the API state code for Utah). The next 3 digits represent the county. Digits 6 thru 10 are a sequential number assigned when a new well is permitted for drilling.
                "p_rpt_period", # Year and month of the report. The date is always shown as the first day of each report month (e.g., 1984-07-01).
               #"p_acct_num",   # Account number of the well operator for the report period. This is a KEY data field for linking to the Operator Data table.
               #"p_prod_zone",  # Geologic zone (formation) of production as reported by the operator. This is a KEY data field for linking to the Producing Zone table.
               #"p_entity",     # Entity number assignment for the given report period. An "entity" is an identifying number used mainly for product disposition tracking by the Division of Oil, Gas and Mining, the Utah State Tax Commission, and the School and Institutional Trust Lands Administration. It usually represents a well or group of wells that have identical division of interest, have the same operator, produce from the same formation, and have product sales from a common tank, tank battery, LACT meter, gas meter, or are in the same participating area of a properly designated unit.
               #"p_wellstatus", # Status of the well for the report period as reported by the well operator. Well statuses were not reported on the production report prior to April 1993. See following URL for description of types: (https://oilgas.ogm.utah.gov/Data_Center/DataDown/Read_Prod.htm)
               #"p_well_type",  # Type of well for the report period as reported by the well operator, either OW (oil well) or GW (gas well).
                "p_days_prod",  # Number of days the well operated during the report period.
                "p_oil_prod",   # Oil production. Oil volumes are reported in Barrels (1 Barrel = 42 U.S. Gallons).
                "p_gas_prod",   # Natural gas production. Gas volumes are reported in MCF (1 MCF = 1,000 cubic feet).
                "p_water_prod", # Water production. Water volumes are reported in Barrels (1 Barrel = 42 U.S. Gallons).
               #"w_acct_num",   # Account number of the current well operator. This is a KEY data field for linking to the Operator Data table.
                "w_field_num",  # Field number. This is a KEY data field for linking to the Field Data table.
               #"w_utm_surf_n", # Universal Transverse Mercator (UTM) surface coordinates - Northings (y coordinate); UTM Zone 12, NAD83. Note- used NAD27 prior to Oct. 18, 2011.
               #"w_utm_surf_e", # Universal Transverse Mercator (UTM) surface coordinates - Eastings (x coordinate); UTM Zone 12, NAD83. Note- used NAD27 prior to Oct. 18, 2011.
               #"w_qtr_qtr",    # Location - quarter/quarter section.
               #"w_section",    # Location - section.
               #"w_township",   # Location - township.
               #"w_range",      # Location - range.
               #"w_meridian",   # Location - meridian
                "w_county",     # Location - county
               #"w_dir_horiz",  # Flag showing if well is directionally drilled (D) or Horizontally drilled (H).
               #"w_conf_flag",  # Flag ('T' = yes; 'F' = no) showing if well is currently in a confidential status.
               #"w_conf_date",  # Expiration date of confidential status.
               #"w_lease_num",  # Current mineral lease number assigned to a well.
                "w_lease_type", # Number that shows the type of mineral lease (0-Unknown, 1-federal, 2-Indian, 3-state, 4-fee).
               #"w_abndondate", # Abandonment date of the APD or well -- Location Abandoned (permit rescinded; WellStatus = 'LA') or Plugged and Abandoned (well plugged; WellStatus = 'PA').
               #"w_wellstatus", # Most current status on record for the well or permit to drill. See following URL for description of types (https://oilgas.ogm.utah.gov/Data_Center/DataDown/Read_Well.htm)
                "w_well_type",  # Most current well type on record. See above URL for description of types
                "w_totcum_oil", # Total cumulative oil production. Oil volumes are reported in Barrels (1 Barrel = 42 U.S. Gallons).
                "w_totcum_gas", # Total cumulative natural gas production. Gas volumes are reported in MCF (1 MCF = 1,000 cubic feet).
                "w_totcum_wtr", # Total cumulative water production. Water volumes are reported in Barrels (1 Barrel = 42 U.S. Gallons).
               #"w_ind_tribe",  # Designates Native American Indian allottee or tribe if applicable ('Navajo' or 'Ute').
               #"w_multi_lats", # Count from completion report(s) showing how many horizontal 'laterals' a well has.
               #"w_cbmethflag", # Flag ('T' = yes; 'F' = no) showing if well is a coalbed methane gas well.
               #"w_surfowntyp", # Number that shows the type of surface ownership (0-Unknown, 1-Federal, 2-Indian, 3-State, 4-Fee).
               #"w_bond_num",   # Number representing well-plugging bond.
               #"w_bond_type",  # Number that shows the type of well-plugging bond (1-Federal, 2-Indian, 3-State, 4-Fee, 5-State & Fee Combined).
               #"w_ca_number",  # Communitization Agreement number.
               #"w_field_type", # Flag that shows field type when well was first drilled (D-development, E-Extension, W-Wildcat)
               #"w_unit_name",  # The name of the unit if applicable.
               #"w_lat_surf",   # Latitude surface coordinates in degrees; UTM Zone 12, NAD83. Note- used NAD27 prior to Oct. 26, 2011.
               #"w_long_surf",  # Longitude surface coordinates in degrees; UTM Zone 12, NAD83. Note- used NAD27 prior to Oct. 26, 2011.
                "h_apd_aprovd", # Date DOGM approved the well's APD
               #"h_work_type",  # Work type code. Go here for list: (https://oilgas.ogm.utah.gov/Data_Center/DataDown/Read_Hist.htm)
                "h_spud_dry",   # Date drilling commenced with a dry hole auger or other surface rig
                "h_spud_rotry", # Date drilling commenced with a rotary rig
               #"h_prod_zone",  # The producing zone as shown in well completion report
                "h_compl_date", # Date that drilling operations were completed per the operators well completion report
               #"h_intent_rec", # The date a Sundry Notice of Intent to do the identified work was received by DOGM
               #"h_work_compl", # The date that the identified work was completed, usually according to the operator's Sundry Subsequent Report.
                "h_td_md",      # Total Depth of the Well -- Measured Depth
               #"h_pbtd_md",    # Plug Back Total Depth of the Well -- Measured Depth
               #"h_wellstatus", # The status of the well at the time the identified work was completed. See above URL on h_work_type for meaning of abbreviations.
                "h_well_type",  # The type of the well at the time the proposed work was completed. See above URL on h_work_type for meaning of abbreviations.
                "h_first_prod", # The date of first production from the well as reported by the operator.
               #"h_testmethod", # The method used for testing the well: Flow, pump, swab, other.
               #"h_choke",      # Well choke during initial production test.
               #"h_tubng_prs",  # Well tubing pressure during initial production test.
               #"h_casng_prs",  # Well casing pressure during initial production test.
                "h_oil_24hr",   # Oil volume produced during initial 24 hour production test.
                "h_gas_24hr",   # Gas volume produced during initial 24 hour production test.
                "h_water_24hr", # Water volume produced during initial 24 hour production test.
               #"h_dir_survey", # Directional Survey flag ('Y' = yes; 'N' = no).
               #"h_cored",      # Core flag ('Y' = yes; 'N' = no).
               #"h_dst",        # Drill Stem Test flag ('Y' = yes; 'N' = no).
               #"h_comp_type",  # Completion method: Perforated, open hole, slotted liner, other.
               #"h_direction",  # Directional drilling flag ('H' = horizontal; 'D' = directional)
               #"h_lat_count",  # The number of horizontal laterals drilled on the permit.
                "h_rec_seq",    # Record sequence number
               #"h_conf_flag",  # Confidentiality Flag ('T' = confidential; 'F' = not confidential)
                "nrec",         # Number of records for given well in proddata
               #"maxtime",      # Maximum value of "time" column for given well
               #"lastrecord",   # Boolean indicating whether or not given row contains a maxtime record. Currently broken, all values are NA
               #"complete",     # Boolean for whether records are complete.  Records are complete IFF actual number of records equals maxtime.
               #"everproduce",  # Boolean for whether well ever produced; note this is not exactly right when have wells drilled prior to 1984 since they may have produced before coming into proddata.
               #"orphan",       # Boolean for whether well was ever orphaned
               #"everpa",       # Boolean for whether well was ever plugged and abandoned
               #"padiff",       # Number of months between w_abndondate and p_rpt_period
                "time")         # Months since given well first appeared in proddata (i.e. since well's h_first_prod date)


# 2.1 dogmDataUpdate Options ----------------------------------------------

# "production.rda" subsetting options for "p". Each string represents a possible
# argument for subsetting the DOGM database. Current valid options are:
#
#   a  - Selects only wells located in Uintah or Duchesne counties
#  ... - Others must be coded first in main.R section 2.1
#
opt$psub <- "a"


# 2.2 scheduleUpdate Options ----------------------------------------------

# Time step options
opt$SU.tp <- T # Set to T to use global tstep range, or F to use alternative specified on next line
opt$SU.tsteps <- seq(from = as.Date("2005-01-01"), to = as.Date("2014-01-01"), by = "months")

# Fraction of wells located in a given field compared to total number of wells 
# in the Uinta Basin that is required for a field to be analyzed individually.
# If a field's well count fraction is lower than this threshold, all of its
# wells will be accounted for in the catch-all "Field 999."
opt$field.cutoff <- 0.03

# Well depth criteria (minimum, maximum, and resolution of well depth CDF) in 
# feet. Minimum well depth is set in global options (Section 1.0). Note that one
# important use of these values is to generate well depth probability
# distributions. In that context, the # of bins generated is 
# (max.depth-min.depth)/depth.step. This result **MUST** be a whole number.
opt$max.well.depth <-  20e3
opt$well.depth.step <- 10


# 2.3 leaseOpCostUpdate Options -------------------------------------------

# Time step options
opt$LU.tp <- F # Set to T to use global tstep range, or F to use alternative specified on next line
opt$LU.tstart <- as.Date("1994-01-01") # Beginning of LOC data range (annual data)
opt$LU.tstop  <- as.Date("2009-01-01") # End of LOC data range (annual data)

# CPI basis for LOC cost values (2009 USD)
opt$LOCbasis <- 214.537


# 2.4 EIApriceUpdate Options ----------------------------------------------

# EIA Historical Energy Prices CPI Basis (i.e. the CPI index value for the year
# to which all oil/gas prices in the EIA_HistPrice.csv file have been adjusted
# to for use in drillingModel.R function). Value below is for 2014 annual avg.
opt$EP.CPI.basis <- 236.736


# 2.5 corpIncomeUpdate Options --------------------------------------------

# Input data for corpIncomeUpdate function
NTI  <- c(66341510, 209171843, 220215146) # Net taxable income (NTI) from UT State Tax Comission.
year <- c(2009, 2010, 2011)               # Year associated with each element in NTI (2009-2011)
cpi  <- c(214.537, 218.056, 224.939)      # CPI annual average values from 2004 to 2012

# Make NTI data.frame and remove component vectors
opt$NTI <- data.frame(year, NTI, cpi); remove(NTI, year, cpi)


# 2.6 propertyTaxUpdate Options -------------------------------------------

# Property taxes collected (Duchesne + Uintah) - by year
year <- c(2004:2012)
PTI  <- c(2407040+5985003,   # 2004
          3640044+8241224,   # 2005
          5358662+12895362,  # 2006
          5209014+13235218,  # 2007
          5801276+19261688,  # 2008
          6266650+20711119,  # 2009
          6196678+21712696,  # 2010
          8755478+24128270,  # 2011
          11784048+27819523) # 2012

# CPI annual average values from 2004 to 2012
cpi <- c(188.900, 195.300, 201.600, 207.342, 215.303, 214.537, 218.056, 224.939,
         229.594)

# Make PTI data.frame and remove component vectors
opt$PTI <- data.frame(year, PTI, cpi); remove(year, PTI, cpi)


# 2.7 drillingModelUpdate Options -----------------------------------------

# Note - uses opt$min.well.depth option set in Section 1.0

# Time step options
opt$DMU.tp <- F # Set to T to use global tstep range, or F to use alternative specified on next line
opt$DMU.tstart <- as.Date("1977-07-01") # Values here are equivalent to full range of eia.hp data
opt$DMU.tstop  <- as.Date("2014-11-01")


# 2.8 GBMfitUpdate Options ------------------------------------------------

# Time step options
opt$GBM.tp <- F # Set to T to use global tstep range, or F to use alternative specified on next line
opt$GBM.tstart <- as.Date("1977-07-01") # Values here are equivalent to full range of eia.hp data
opt$GBM.tstop  <- as.Date("2014-11-01")


# 2.9 EIAforecastUpdate Options -------------------------------------------

# EIA Forecast data.frame creation. Enter EIA forecast here - the AEO forecast 
# used should be the same as the year used for the starting point of the modeled
# time period. The number of rows in the data.frame should also be equal to the 
# value used above in opt$EIAtsteps. Finally, the "year" column assumes that the
# EIA price given is for the middle of each year of the EIA forecast (i.e. 
# June). The final date of the modeled time period should be equal to or greater
# than the last date in the year column.
year <- seq(as.Date("2010-06-01"), as.Date("2014-06-01"), by = "year")           # Year (time step for EIA price forecasts)
oil  <- c(73.90, 74.13, 81.39, 87.98, 93.26) # Rocky Mountain wellhead oil price forecast in 2008 $/bbl from Table 101
gas  <- c( 4.16,  5.33,  5.65,  5.48,  5.44) # Rocky Mountain wellhead gas price forecast in 2008 $/MCF from Table 102
opt$forecast <- data.frame(year, oil, gas)
remove(year, oil, gas)

# EIA CPI basis
opt$EIAbasis  <- 215.303 # Annual average CPI for 2008

# Time step options
opt$EFU.tp <- T # Set to T to use global tstep range, or F to use alternative specified on next line
opt$EFU.tsteps <- seq(from = as.Date("2005-01-01"), to = as.Date("2014-01-01"), by = "months")


# 2.10 EIAerrorUpdate Options ---------------------------------------------

# Time step options
opt$EEU.tsteps <- 5 # Number of years into the future for which you want EIA error % CDFs


# 2.11 DCAupdate Options --------------------------------------------------

# General DCA Fitting Options
opt$minProdRec      <- 12   # Minimum number of non-zero production records
opt$minDayProd      <- 28   # Minimum number of days of a well produced in a given month required to include production data point
opt$diff.bin.cutoff <- 0.15 # Minimum production differential on normalized scale required to consider a well as being restarted
opt$bin             <- 12   # Bin size
opt$DCAplot         <- T    # True/False flag indicating whether or not to print
opt$n.stopB.min     <- 4    # Any stop points identified that are lower than this value will be ignored
opt$n.startT.search <- 3    # Look at the top "n" number of production points and pick the one with the lowest time value
opt$DCAtcutoff      <- opt$tstart # Any production records >= this date will be excluded from fits

# Hyperbolic DC Options
opt$b.start.oil     <- 1.78            # Initial guess value for b coefficient for oil decline curve
opt$Di.start.oil    <- 1.16            # Initial guess value for Di coefficient for oil decline curve
opt$lower.oil       <- c(0, 0, 0)      # Lower limits for NLS for oil decline curve for (qo, b, Di) coefficients
opt$upper.oil       <- c(Inf, 10, Inf) # Upper limits for NLS for oil decline curve for (qo, b, Di) coefficients
opt$b.start.gas     <- 1.32            # Same as above but for gas
opt$Di.start.gas    <- 0.24            # Same as above but for gas
opt$lower.gas       <- c(0, 0, 0)      # Same as above but for gas
opt$upper.gas       <- c(Inf, 10, Inf) # Same as above but for gas

# Cumulative DC Options
opt$Cp.start.oil    <- 1e3         # Initial guess value for Cp coefficient for oil cumulative production curve
opt$c1.start.oil    <- 0           # Initial guess value for c1 constant for oil cumulative production curve
opt$Qlower.oil      <- c(0,  -Inf) # Lower limits for NLS for oil cumulative production curve for (Cp, c1) coefficients
opt$Qupper.oil      <- c(Inf, Inf) # Upper limits for NLS for oil cumulative production curve for (Cp, c1) coefficients
opt$Cp.start.gas    <- 1e4         # Same as above but for gas
opt$c1.start.gas    <- 0           # Same as above but for gas
opt$Qlower.gas      <- c(0,  -Inf) # Same as above but for gas
opt$Qupper.gas      <- c(Inf, Inf) # Same as above but for gas


# 2.12 DCA Field Update Options -------------------------------------------



# 2.13 DCA CDF Update Options ---------------------------------------------

# Character string for switch funtion, valid options are either "Density" or
# "Quantile"
opt$DCA.CDF.type <- "Quantile"

# Time step options
opt$DCA.tp <- T # Set to T to use global tstep range, or F to use alternative specified on next line
opt$DCA.tstart  <- as.Date("1999-01-01")
opt$DCA.tstop   <- as.Date("2012-12-01")

# DCA CDF Generation - Hyperbolic DC
opt$cdf.oil.from    <- c(0,0,0,0)             # Lower limit for CDF function for (qo, b, Di, tdelay)
opt$cdf.oil.to      <- c(2e3, Inf, Inf, Inf)  # Upper limit for CDF function for (qo, b, Di, tdelay) - was 3e7, 4, 15e3, Inf
opt$cdf.oil.np      <- c(3e5, 4e3, 15e4, 360) # Number of points at which to estimate CDF for (qo, b, Di, tdelay)
opt$cdf.gas.from    <- c(0,0,0,0)             # Same as above but for gas
opt$cdf.gas.to      <- c(4e4, Inf, Inf, Inf)  # Same as above but for gas - was 4e7, 4, 4e3, 360
opt$cdf.gas.np      <- c(4e5, 4e3, 4e4, 360)  # Same as above but for gas

# DCA CDF Generation - Cumulative DC
opt$Q.cdf.oil.from  <- c(0,    -30e3)  # Lower limit in cumulative fit for CDF function for (Cp, c1)
opt$Q.cdf.oil.to    <- c(30e3,  30e3)  # Upper limit in cumulative fit for CDF function for (Cp, c1)
opt$Q.cdf.oil.np    <- c(10e3,  10e3)  # Number of points in cumulative fit at which to estimate CDF for (Cp, c1)
opt$Q.cdf.gas.from  <- c(0,    -400e3) # Same as above but for gas
opt$Q.cdf.gas.to    <- c(200e3, 100e3) # Same as above but for gas
opt$Q.cdf.gas.np    <- c(10e3,  10e3)  # Same as above but for gas


# 2.14 drillCapCostUpdate Options -----------------------------------------


# 2.15 waterUpdate Options ------------------------------------------------

opt$f_mud       <- 1    # Assumed volume ratio of (water used to mix with drilling mud) / (volume of borehole)
opt$f_cem       <- 4.97 # Water to cement ratio in (gal/sack), value of 4.97 is used for API Class G cement
opt$rcut.pw.oil <- 100  # Maximum ratio of produced water to oil production to be included in CDF for produced water from oil wells
opt$rcut.pw.gas <- 10   # Maximum ratio of produced water to gas production to be included in CDF for produced water from gas wells
opt$rcut.disp   <- 0.6  # Maximum ratio of disposal water to produced water to be included in CDF for disposal water

# Time step options
opt$WU.tp <- T # Set to T to use global tstep range, or F to use alternative specified on next line
opt$WU.tstart  <- as.Date("1999-01-01")
opt$WU.tstop   <- as.Date("2012-12-01")


# 3.1 Energy Price Path Simulation Options --------------------------------

# Energy price path simulation method. Valid options are:
#  a - GBM price paths
#  b - EIA forecast with error propagation
opt$ep.type <- "a"


# 3.2 drillSim Options ----------------------------------------------------

# Initial number of wells drilled during prior time step at start of simulated 
# time. Some specific values (all in Dec.):
# Year: 1998 | 1999 | 2004 | 2009 |2012 |
# Num:    18 |   11 |   50 |   43 |  63 |
opt$drilled.init <- 43


# 3.3.1-2 welldata and productionsim Options ------------------------------

# Select drilling schedule type. Valid options are:
#  a - Simulated drilling schedule
#  b - Actual drilling schedule
opt$sched.type <- "a"

# Select production type. Valid options are:
#  a - Simulated production from decline curve coefficients
#  b - Actual production volumes (note: should only be used with actual drilling
#      schedule)
opt$prod.type <- "a"

# Decline curve type. Valid options are:
#  a - Hyperbolic decline curve q(t) = qo * (1 + b * Di * t) ^ (-1 / b)
#  b - Cumulative production curve Q(t) = Cp * t ^ 0.5 + c1
opt$mc.decline.type <- "b"


# 3.3.3 royalty Options ---------------------------------------------------

# Royatly rates for Federal, Indian, State, and Fee type leases (in that order).
# Second command adds names to each element to match.
opt$royaltyRate <- c(0.1250, 0.1667, 0.1250, 0.1250)
names(opt$royaltyRate) <- c("federal", "indian", "state", "fee")


# 3.3.4 stax Options ------------------------------------------------------

# Severance Tax Inputs
opt$st.con    <- 0.002 # Conservation fee rate
opt$st.low    <- 0.030 # Low ST rate for oil/gas for values <= cutoff value threshold
opt$st.high   <- 0.050 # High ST rate for oil/gas for values > cutoff value threshold
opt$st.ocut   <- 13    # Oil cutoff value threshold ($/bbl) for switch from low to high ST rate
opt$st.gcut   <- 1.5   # Gas cutoff value threshold ($/MCF) for switch from low to high ST rate
opt$st.skip   <- 6     # Number of timesteps from date well is drilled to exempt from ST
opt$strip.oil <- 20*30 # Oil production volume (bbl/month) below which a well is classified as a stripper well
opt$strip.gas <- 60*30 # Gas production volume (MCF/month) below which a well is classified as a stripper well


# 3.3.6 ctax Options ------------------------------------------------------

# Corporate income tax rates (fraction of net earnings paid in corporate income
# taxes)
opt$CIrate.state <- 0.05 # State
opt$CIrate.fed   <- 0.35 # Federal


# Emission Factors --------------------------------------------------------

# Emission factor category names and units
EF.names <- c("site",    # Site preparation                          (metric tons / well)
              "Tdrill",  # Transporation of materials for drilling   (metric tons / well)
              "Tcompl",  # Transporation of materials for completion (metric tons / well)
              "Trework", # Transporation of materials for rework     (metric tons / well)
              "Tprod",   # Transporation of materials for production (metric tons / well)
              "drill",   # Drilling and fracturing                   (metric tons / well)
              "compl",   # Completion                                (metric tons / well)
              "prod",    # Production                                (metric tons / well year)
              "proc",    # Processing                                (metric tons / 10^9 CF gas)
              "transm")  # Transmission and distribution             (% of CH4 produced over lifecycle of well)

# Input EF mean and standard deviation values here in order shown above in EF.names vector
#...................................................................................................................
# Term      site  Tdrill   Tcompl   Trework  Tprod   drill compl  prod  proc   transm     Notes
#...................................................................................................................
m.co2 <-  c(208,  0.4,     0.21,    3.05,    1.36,    569, 1940,  43,   901,    0)     # Mean CO2e EFs
m.ch4 <-  c(9.9,  8.6e-6,  4.36e-6, 7.71e-5, 3.29e-5, 0,   92.4,  2.07, 5.58,   199)   # Mean CH4 EFs
m.voc <-  c(1.58, 0,       0,       0,       0,       0,   14.78, 0.78, 0.8928, 31.82) # Mean VOC EFs
sd.co2 <- c(79,   0.56,    0.29,    4.31,    1.93,    326, 967,   40,   46,     0)     # Standard deviation CO2e EFs
sd.ch4 <- c(3.37, 1.22e-5, 6.16e-6, 1.01e-4, 4.65e-5, 0,   46,    1.9,  3.91,   163)   # Standard deviation CH4 EFs
sd.voc <- c(0.6,  0,       0,       0,       0,       0,   7.37,  0.73, 0.62,   26)    # Standard deviation VOC EFs

# If any EF has a standard deviation of zero, set it to the fraction of the mean
# as specfied below
sdFracMean <- 1
sd.co2 <- ifelse(sd.co2 == 0, m.co2*sdFracMean, sd.co2)
sd.ch4 <- ifelse(sd.ch4 == 0, m.ch4*sdFracMean, sd.ch4)
sd.voc <- ifelse(sd.voc == 0, m.voc*sdFracMean, sd.voc)

# Make EF statistics data.frame
opt$EF <- data.frame(m.co2, m.ch4, m.voc, sd.co2, sd.ch4, sd.voc, row.names = EF.names)

# Unit conversions
opt$EF["prod",] <-   opt$EF["prod",]/12    # from (well year) to (well month)
opt$EF["proc",] <-   opt$EF["proc",]/1e6   # from ( / 10^9 CF gas) to ( / MCF gas)
opt$EF["transm",] <- opt$EF["transm",]/1e6 # from ( / 10^9 CF gas) to ( / MCF gas)

# Remove component vectors
remove(EF.names, m.co2, m.ch4, m.voc, sd.co2, sd.ch4, sd.voc, sdFracMean)


# 4.1 postProcess Options -------------------------------------------------

# Export options
opt$exportFlag <- F                  # If true, will plot to PDF located in path$plot directory
opt$prefix <-     "pp "              # Any text here will be added in front of the name given in the table below
opt$affix  <-     " -1e3run -EIA -2010to2015 -v4.pdf" # Any text here will be added to the end " " " "...

#...............................................................................
#                      File Name              Plot? T/F          Description
#...............................................................................

opt$plist <- rbind(c("Oil Price",                  T), # Oil prices simulated vs actual
                   c("Gas Price",                  T), # Gas prices simulated vs actual
                   c("Drilling Schedule",          T), # Drilling schedule simulated vs actual
                   c("Drilling Model Fit",         F), # Drilling fit vs actual
                   c("DCA Coefficients - Boxplot", F), # Boxplot of DCA coefficients
                   c("DCA Coefficients - CDF",     F), # CDF DCA coefficients
                   c("Total Oil Production",       T), # Total oil production simulated vs actual
                   c("Total Gas Production",       T), # Total gas production simulated vs actual
                   c("CO2e Emissions",             T), # CO2 emissions
                   c("CH4 Emissions",              T), # CH4 emissions
                   c("VOC Emissions",              T), # VOC emissions
                   c("Field Fractions",            F), # Pie chart of bar chart or something showing number of wells located in each distinct field during the data fitting period
                   c("Field Fractions -OW",        F), # Same but just oil wells
                   c("Field Fractions -GW",        F), # Same but just gas wells
                   c("Well Capital Cost",          F), # Drilling and completion capital cost data and fit
                   c("Surface Lease Ownership",    F), # Surface lease ownership by field
                   c("CDFs for Well Depth",        F), # CDFs for well depth by well type
                   c("LOC Model Fit",              F), # Lease operating costs model fit for oil wells and gas wells
                   c("Enery Price History",        F), # FPP history for oil and gas from EIA data
                   c("NTI CDF",                    F), # CDF for net taxable income as fraction of revenue
                   c("Property Taxes CDF",         F), # CDF for property taxes as fraction of revenue
                   c("EIA AEO Error CDFs",         F), # CDFs for error % in EIA AEO forecasts for oil and gas
                   c("Models for Water Terms",     F), # CDFs and linear regression models for water balance terms
                   c("Water Balance Results",      F) # Results of water balance calculations for each term in WB eq.
)
                   #c("?", 1),    # stack area plot or something v taxes and royalties

# Convert to data.frame and adjust column names
opt$plist <- data.frame(opt$plist[,1], as.logical(opt$plist[,2]))
names(opt$plist) <- c("name", "plot")

# Quantiles to use to show uncertainty in simulated results
opt$quant <- c(0.9, 0.7, 0.5, 0.3, 0.1)