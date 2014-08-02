# Script Info -------------------------------------------------------------
# simDriver.R (Conventional Oil and Gas Simulation Driver Script)
# Jon Wilkey


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------

# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Functions directory
fin <- "C:/Users/Jon/Documents/R/ub_oilandgas/Functions"
# Working directory
work_root <- "C:/Users/Jon/Documents/R/ub_oilandgas/"

# Set working directory
setwd(work_root)


# Functions ---------------------------------------------------------------
# List of functions used in this script to be loaded here
flst <- file.path(fin,c("welldata.R",
                        "productionsim.R",
                        "inf_adj.R",
                        "royalty.R",
                        "stax.R",
                        "ptax.R",
                        "RIMS.R",
                        "workload.R",
                        "GHG.R",
                        "write_excel.R"))

# Load each function in list
for (f in flst) source(f)


# Libraries ---------------------------------------------------------------
library(data.table)  # For wsim data.table (maybe others in the future)


# Load required data files ------------------------------------------------

# Oil & gas price history
load(file.path(data_root, "oil_and_gas_price_history_1999_to_2012.rda"))


# Other Inputs ------------------------------------------------------------
# Create a complete set of months between 1999-01-01 and 2012-12-01.
timesteps <- seq(from = as.Date("1999-01-01"),
                  to = as.Date("2012-12-01"),
                  by = "months")

# Field Selection (i.e. fields that will be analyzed individually). Note that
# "Field 999" is placeholder for all other fields category.
field <- c(630, 105, 72, 55, 65, 710, 665, 590, 60, 718, 999)

# CPI value for inflation adjustment
basis <- 233.049

# Prompt user for number of iterations
nrun <- as.numeric(readline(prompt =
"\n How many times would you like the simulation to run? \n"))

# Prompt user for drilling schedule type
schedule.type <- readline(prompt =
"\n Please select the method for determining the drilling schedule: \n
(a) Simulated drilling schedule
(b) Actual drilling schedule \n
Selection (a or b): \n")

# Prompt user for production type
production.type <- readline(prompt =
"\n Please select the method for determining the production volumes: \n
(a) Simulated production from decline curve coefficients
(b) Actual production volumes (note: should only be used with actual drilling schedule) \n
Selection (a or b): \n")

# Prompt for exporting results
exportFlag <- readline(prompt = "\n Export results (yes or no)? \n")


# Well Data Simulation ----------------------------------------------------

wsim <- welldata(nrun = nrun,
                 data_root = data_root,
                 timesteps = timesteps,
                 basis = basis,
                 field = field,
                 schedule.type = schedule.type,
                 production.type = production.type)


# Production Simulation ---------------------------------------------------

psim <- productionsim(nrun = nrun,
                      wsim = wsim,
                      data_root = data_root,
                      timesteps = timesteps,
                      production.type = production.type)


# Royalties ---------------------------------------------------------------

# Define prices and adjust for inflation - op = oil price, gp = gas price. Given
# basis (233.049) inflation adjusts to 2013-12-01.
op <- inf_adj(OGprice$bw, OGprice$cpi, basis)
gp <- inf_adj(OGprice$uswp, OGprice$cpi, basis)

# Run royalty calculation
rsim <- royalty(op = op,
                gp = gp,
                wsim = wsim,
                psim = psim)


# Severance Taxes ---------------------------------------------------------

# Get indices of oil and gas wells
ind.ow <- which(wsim$wellType == "OW")
ind.gw <- which(wsim$wellType == "GW")

# Run severance tax calculation
stsim <- stax(wsim = wsim,
              psim = psim,
              rsim = rsim,
              op = op,
              gp = gp,
              ind.ow = ind.ow,
              ind.gw = ind.gw)


# Property Taxes ----------------------------------------------------------

# Run property tax calculation. Right now there are issues with the NPV < 0 in
# many cases, so only return from ptax function is lease operating costs (LOC).
LOC <- ptax(data_root = data_root,
            wsim = wsim,
            psim = psim,
            op = op,
            gp = gp,
            ind.ow = ind.ow,
            ind.gw = ind.gw,
            basis = basis,
            rsim = rsim,
            stsim = stsim)


# Corporate Income Taxes --------------------------------------------------

# Predefine matrices for calculation results
ciSO <- matrix(0, nrow = nrow(psim), ncol = ncol(psim))
ciSG <- ciSO
ciFO <- ciSO
ciFG <- ciSO

# Calculate corporate income taxes for on-type (OOW & GGW) production
for (i in 1:ncol(psim)) {
  ciSO[ind.ow,i] <- wsim$cirSO[ind.ow]*psim[ind.ow,i]
  ciSG[ind.gw,i] <- wsim$cirSG[ind.gw]*psim[ind.gw,i]
  ciFO[ind.ow,i] <- wsim$cirFO[ind.ow]*psim[ind.ow,i]
  ciFG[ind.gw,i] <- wsim$cirFG[ind.gw]*psim[ind.gw,i]
}


# Employment --------------------------------------------------------------

# RIMS II model employment estimate
jobs.RIMS <- RIMS(multiplier = 2.2370,
                  wsim = wsim,
                  LOC = LOC,
                  nrun = nrun)

# Workload-based model employment estimate. See workload.R to change model
# constants (too many to pass as input arguments here).
jobs.workload <- workload(wsim = wsim,
                          psim = psim,
                          nrun = nrun,
                          timesteps = timesteps)


# GHG Emissions -----------------------------------------------------------

# Determine GHG emissions as (1) 1e3 kg CO2e and (2) MCF of CH4
emissions <- GHG(wsim = wsim,
                 psim = psim,
                 nrun = nrun,
                 timesteps = timesteps,
                 ind.ow = ind.ow,
                 ind.gw = ind.gw,
                 truckload = 200)       # Capacity of oil trucks in bbl


# Save results ------------------------------------------------------------

# Export as *.rda file to Prepared Data file path if exportFlag == "yes"
if(exportFlag == "yes") {
  save(file=file.path(data_root, "simResults.rda"),
       list=c("wsim",
              "psim",
              "rsim",
              "stsim",
              "ciFG",
              "ciFO",
              "ciSG",
              "ciSO",
              "jobs.RIMS",
              "jobs.workload",
              "emissions",
              "timesteps"))
}