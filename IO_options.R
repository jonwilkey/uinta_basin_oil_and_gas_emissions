# Script Info -------------------------------------------------------------
# IO_options.R (Conventional Oil and Gas Simulation Options Script)


# Description -------------------------------------------------------------

# This script creates a list object named "opt" that contains the options for 
# all the inputs/outputs that control the execution of the main.R script. Review
# each input/output below and change as desired from their base values.



# Define "opt" list object ------------------------------------------------

opt <- NULL


# 1. Inputs ------------------------------------------------------------------


# 1.1 Flags for updating prepared data files ----------------------------------

# This segment contains flags for updating the files contained in the prepared 
# data folder (typically *.rda files, probability distributions, linear 
# regression fits, etc.). If new data is available, save the raw data to the raw
# data folder, update the flag(s) for the appropriate script(s) below, save this
# script, and rerun main.R
#...............................................................................
#  Flag Name           Value                          Notes
#...............................................................................
opt$DOGM.update       <- FALSE  # Turns *.dbf files from DOGM () into single file (production.rda) used for all subsequent analysis
opt$schedule.update   <- FALSE  # Generates CDF for field numbers, lease type, well type, and well depth. Extracts actual drilling and production history from production.rda.
opt$water.update      <- FALSE  # Generates all CDFs and linear regression models for water balance terms
opt$corptax.update    <- FALSE  # Generates corporate income tax coversion factor CDFs
opt$DCA.update        <- FALSE  # Generates CDFs for decline curves
opt$emission.update   <- FALSE  # Generates CDFs for emission factors
opt$lease.update      <- FALSE  # Fits lease operating cost model to EIA lease operating cost data.
opt$drillmodel.update <- FALSE  # Fits drilling schedule model to energy prices
opt$GBMfit.update     <- FALSE  # Fits GBM parameters "v" and "mu" to energy prices
opt$EIAprice.update   <- FALSE  # Generates CDFs for EIA price forecasts

# Version filename. If any of the update flags above is set to "TRUE", change
# the version number below so that previous *.rda versions will be retained.
opt$file_ver <- "v2"

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
               #"p_days_prod",  # Number of days the well operated during the report period.
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
               #"nrec",         # Number of records for given well in proddata
               #"maxtime",      # Maximum value of "time" column for given well
               #"lastrecord",   # Boolean indicating whether or not given row contains a maxtime record. Currently broken, all values are NA
               #"complete",     # Boolean for whether records are complete.  Records are complete IFF actual number of records equals maxtime.
               #"everproduce",  # Boolean for whether well ever produced; note this is not exactly right when have wells drilled prior to 1984 since they may have produced before coming into proddata.
               #"orphan",       # Boolean for whether well was ever orphaned
               #"everpa",       # Boolean for whether well was ever plugged and abandoned
               #"padiff",       # Number of months between w_abndondate and p_rpt_period
                "time")         # Months since given well first appeared in proddata (i.e. since well's h_first_prod date)


# 1.3 Monte-Carlo simulation options --------------------------------------

# Enter number of overall simulation iterations
opt$nrun <- 10

# Select drilling schedule type. Valid options are:
#
#  1 - Simulated drilling schedule
#  2 - Actual drilling schedule
#
opt$sched.type <- 1

# Select production type. Valid options are:
#
#  1 - Simulated production from decline curve coefficients
#  2 - Actual production volumes (note: should only be used with actual drilling
#      schedule)
#
opt$prod.type <- 1


# 1.4 Time related options ------------------------------------------------

# Enter start and stop point for simulation in "YYYY-MM-DD" format by changing
# values inside quotations of opt$tstart and opt$tstop.
opt$tstart <- as.Date("1999-01-01")
opt$tstop  <- as.Date("2012-12-01")
opt$tsteps <- seq(from = opt$tstart,
                     to = opt$tstop,
                     by = "months")

# Flag for whether or not to fit all data or just data that lies within 
# tstart/tstop time period. If set to "TRUE" then model uses all data, otherwise
# the data is subsetted to just the modeled time period. Effected datasets:
#
# (1) leaseOpCostUpdate.R
#
opt$fullDataFit <- TRUE

# 1.5 Geography related options -------------------------------------------

# Field Selection (i.e. fields that will be analyzed individually). Note that 
# "Field 999" is placeholder for all other fields (i.e. every field other than
# the fields listed).
opt$field <- c(630, 105, 72, 55, 65, 710, 665, 590, 60, 718, 999)

# "production.rda" subsetting options for "p". Each string represents a possible
# argument for subsetting the DOGM database. Options are:
#
#   a  - Selects only wells located in Uintah or Duchesne counties
#  ... - Others must be coded first in main.R section 2.1
#
opt$psub <- "a"

# Well depth criteria (minimum, maximum, and resolution of well depth CDF) in 
# feet. Note that one important use of these values is to generate well depth 
# probability distributions. In that context, the # of bins generate is 
# (max.depth-min.depth)/depth.step. This result **MUST** be a whole number.
opt$min.well.depth <- 1e3
opt$max.well.depth <- 20e3
opt$well.depth.step <- 20


# 1.6 Finance related options ---------------------------------------------

# Corporate income tax rates (fraction of net earnings paid in corporate income
# taxes)
opt$CIrate.state <- 0.05 # State
opt$CIrate.fed   <- 0.35 # Federal

# EIA Historical Energy Prices CPI Basis (i.e. the CPI index value for the year
# to which all oil/gas prices in the EIA_HistPrice.csv file have been adjusted
# to for use in drillingModel.R function).
opt$EP.CPI.basis <- 229.594 # Annual CPI index for 2012

# Enter CPI value for inflation adjustment
opt$cpi <- 233.049

# Price differentials between UT first purchase price (FPP) and spot prices for
# (1) WTI crude price, and (2) Henry Hub natural gas, such that
# (ratio)*(spot)=(FPP)
opt$WTI.to.Oilfpp <- 0.9086103492
opt$HH.to.Gasfpp  <- 0.7446302868

# Initial prices for GBM price path simulation (from last recorded EIA FPP).
# Sources:
# Gas [1] http://tonto.eia.gov/dnav/ng/hist/na1140_sut_3a.htm
#     [2] http://www.eia.gov/dnav/ng/hist/n9190us3A.htm
# Oil [3] http://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=f004049__3&f=a
# Note that gas FPP are calculated by converting US national average price (UT
# gas prices are only recorded by EIA on annual basis, by nation gas FPPs are
# recorded on monthly basis)
opt$oil.fpp.init <- 76.66766251 # Both values are from 2012-12-15 in 2012 $ per
opt$gas.fpp.init <- 2.925932004 # bbl (for oil) or MMBtu (for gas)

# 1.7 Hard-coded data input -----------------------------------------------

# Net taxable income (NTI) from UT State Tax Comission.
NTI <- c(66341510, 209171843, 220215146)
year <- c(2009, 2010, 2011)
opt$NTI <- data.frame(year, NTI); remove(NTI, year)

# Min/max values for setting range of $/bbl or $/MCF oil/gas corporate income
# tax conversion factors in corporate income tax probability distribution
# function
opt$CI.pdf.min <- 0
opt$CI.pdf.max <- 3

# Outputs -----------------------------------------------------------------

# Export plot results as PDF? Valid options are TRUE/FALSE
opt$exportFlag <- FALSE

