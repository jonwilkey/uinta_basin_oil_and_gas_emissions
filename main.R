# Script Info -------------------------------------------------------------
# Name:      main.R (Conventional Oil and Gas Simulation Driver Script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Description -------------------------------------------------------------

# This script does everything. Structure is as follows:

# 1. Set simulation environment, including directory paths, functions, libraries
#    and loading user input/output options.
# 2. Process DOGM *.dbf database files


# 1.1 Paths ---------------------------------------------------------------

# Predefine list object "path" for holding directory path listings
path <- NULL

# Path switch - uncomment and/or replace with the path directory for your local
# copy of the Git repository and Dropbox files.
  pwd.drop <- "D:/"                                  # Windows
  pwd.git  <- "C:/Users/Jon/Documents/R/"
# pwd.drop <- "/Users/john/"                         # Mac
# pwd.git  <- "/Users/john/Documents/ub_oilandgas/"
# pwd.drop <- "/home/slyleaf/"                       # Linux
# pwd.git  <- "/home/slyleaf/Documents/"
  
# Define paths.
# "raw"  is raw data (*.dbf files from DOGM, *.csv files, etc.). 
# "data" is prepared data files (typically *.rda).
# "look" is lookup tables. 
# "plot" is the directory for saving plot *.pdf files.
# "work" is the working directory where main.R and IO_options.R are located.
# "fun"  is the directory for all *.R functions.
path$raw  <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Raw Data", sep = "")
path$data <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Prepared Data", sep = "")
path$look <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Lookup Tables", sep = "")
path$plot <- paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Plots", sep = "")
path$work <- paste(pwd.git,  "ub_oilandgas/", sep = "")
path$fun  <- paste(pwd.git,  "ub_oilandgas/Functions", sep = "")

# Remove temporary path objects
remove(pwd.drop, pwd.git)

# Set working directory
setwd(path$work)


# 1.2 Functions -----------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(path$fun, c("GBMsim.R",
                              "drillsim.R",
                              "welldata.R",
                              "productionsim.R",
                              "royalty.R",
                              "stax.R",
                              "ctax.R",
                              #"ptax.R",
                              #"RIMS.R",
                              #"workload.R",
                              #"GHG.R",
                              #"water.R",
                              "clipboard.R",
                              "inf_adj.R",
                              "CDF.R"))

# Load each function in list then remove temporary file list variables
for (f in flst) source(f); remove(f, flst)


# 1.3 Libraries -----------------------------------------------------------

library(foreign)
library(plyr)
library(zoo)
library(data.table)
library(sqldf)
library(minpack.lm)


# 1.4 Options -------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

# Run script "IO_options.R" to load user defined input/output options
source("IO_options.R")


# 2.1 DOGM *.dbf database file processing ---------------------------------

# Run function if opt$DOGM.update flag is set to "TRUE"
if(opt$DOGM.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "dogmDataUpdate.R"))
  
  # Function call
  dogmDataUpdate(path = path,
                 ver = opt$file_ver)
}

# Load production.rda and rename as "p" for brevity
load(file.path(path$data, paste("production_", opt$file_ver, ".rda", sep = "")))

# Make subset of "production" called "p" based on criteria in opt$keeps
p <- production
p <- subset(p, subset = switch(opt$psub,
                               a = {(w_county == "UINTAH" |
                                     w_county == "DUCHESNE")}),
            select = opt$p.keep)


# 2.2 Drilling schedule CDF generation ------------------------------------

# Run function if opt$schedule.update flag is set to "TRUE"
if(opt$schedule.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "scheduleUpdate.R"))
  
  # Function call
  scheduleUpdate(path =            path,
                 p =               p,
                 field =           opt$field,
                 tsteps =          opt$tsteps,
                 min.depth =       opt$min.well.depth,
                 max.depth =       opt$max.well.depth,
                 well.depth.step = opt$well.depth.step,
                 ver =             opt$file_ver)
}


# 2.3 Corporate income tax conversion factor CDF generation ---------------

# Run function if opt$corptax.update flag is set to "TRUE"
if(opt$corptax.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "corpIncomeUpdate.R"))
  
  # Function call
  corpIncomeUpdate(production =   production,
                   path =         path,
                   NTI =          opt$NTI,
                   CIrate.state = opt$CIrate.state,
                   CIrate.fed =   opt$CIrate.fed,
                   basis =        opt$cpi,
                   CI.pdf.min =   opt$CI.pdf.min,
                   CI.pdf.max =   opt$CI.pdf.max,
                   ver =          opt$file_ver)
}


# 2.4 Lease opearting cost lm() fit update --------------------------------

# Run function if opt$lease.update flag is set to "TRUE"
if(opt$lease.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "leaseOpCostUpdate.R"))
  
  # Function call
  leaseOpCostUpdate(path =    path,
                    ver =     opt$file_ver,
                    tstart =  opt$tstart,
                    tstop =   opt$tstop,
                    full =    opt$fullDataFit)
}


# 2.x EIA energy price history --------------------------------------------

# Run function if opt$EIAprice.update is set to "TRUE"
if(opt$EIAprice.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "EIApriceUpdate.R"))
  
  # Function call
  EIApriceUpdate(path =         path,
                 EP.CPI.basis = opt$EP.CPI.basis,
                 cpi =          opt$cpi,
                 ver =          opt$file_ver)
}

# Load EIAprices_v*.rda to load eia.hp (EIA historical energy prices) data.frame
load(file.path(path$data, paste("EIAprices_", opt$file_ver, ".rda", sep = "")))


# 2.5 Drilling Schedule Model lm() fit update -----------------------------

# Run function if opt$drillmodel.update flag is set to "TRUE"
if(opt$drillmodel.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "drillingModelUpdate.R"))
  
  # Function call
  drillingModelUpdate(path =      path,
                      p =         p,
                      min.depth = opt$min.well.depth,
                      ver =       opt$file_ver)
}


# 2.6 GBM parameter fit update --------------------------------------------

# Run function if opt$GBMfit.update flag is set to "TRUE"
if(opt$GBMfit.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "GBMfitUpdate.R"))
  
  # Function call
  GBMfitUpdate(path =   path,
               eia.hp = eia.hp,
               ver =    opt$file_ver)
}


# 2.7 Decline curve analysis update ---------------------------------------

# Run function if opt$GBMfit.update flag is set to "TRUE"
if(opt$DCA.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "DCAupdate.R"))
  
  # Function call
  DCAupdate(minProdRec =      opt$minProdRec,
            minDayProd =      opt$minDayProd,
            diff.bin.cutoff = opt$diff.bin.cutoff,
            bin =             opt$bin,
            DCAplot =         opt$DCAplot,
            n.stopB.min =     opt$n.stopB.min,
            n.startT.search = opt$n.startT.search,
            b.start.oil =     opt$b.start.oil,
            Di.start.oil =    opt$Di.start.oil,
            lower.oil =       opt$lower.oil,
            upper.oil =       opt$upper.oil,
            b.start.gas =     opt$b.start.gas,
            Di.start.gas =    opt$Di.start.gas,
            lower.gas =       opt$lower.gas,
            upper.gas =       opt$upper.gas,
            field =           opt$field,
            ver =             opt$file_ver,
            cdf.oil.from =    opt$cdf.oil.from,
            cdf.oil.to =      opt$cdf.oil.to,
            cdf.oil.np =      opt$cdf.oil.np,
            cdf.gas.from =    opt$cdf.gas.from,
            cdf.gas.to =      opt$cdf.gas.to,
            cdf.gas.np =      opt$cdf.gas.np,
            path =            path,
            p =               p)
}


# 2.8 Drilling and Completion Capital Cost Model Update -------------------

# WRITE ME - AFTERWARDS UPDATE "welldata.R" RELATED CODE 
# 
# # Run function if opt$drillCapCost.update flag is set to "TRUE"
# if(opt$drillCapCost.update == TRUE) {
#   source(file.path(path$fun, "drillCapCostUpdate.R"))
#   drillCapCostUpdate(blah)
# }


# 2.9 Water balance data analysis and update ------------------------------

# WRITE ME - AFTERWARDS UPDATE "welldata.R" RELATED CODE 
#
# Use "convWater.R as basis for function
#
# # Run function if opt$water.update flag is set to "TRUE"
# if(opt$water.update == TRUE) {
#   source(file.path(path$fun, "waterUpdate.R"))
#   waterUpdate(blah)
# }

# 2.10 Emission Factors Update --------------------------------------------

# WRITE ME - AFTERWARDS UPDATE "welldata.R" RELATED CODE
# 
# # Run function if opt$emission.update flag is set to "TRUE"
# if(opt$emission.update == TRUE) {
#   source(file.path(path$fun, "EFupdate.R"))
#   EFupdate(blah)
# }


# 3.x Energy price path simulation ----------------------------------------

# Run Geometric Brownian Motion (GBM) price path simulation
epsim <- GBMsim(path =         path,
                oil.fpp.init = opt$oil.fpp.init,
                gas.fpp.init = opt$gas.fpp.init,
                timesteps =    opt$MC.tsteps,
                nrun =         opt$nrun,
                ver =          opt$file_ver)

# Extract individual data.frames from list object
opsim <- epsim[[1]]
gpsim <- epsim[[1]]

# Remove list
remove(epsim)


# 3.x Well drilling simulation --------------------------------------------

# Run well drilling simulation given price paths opsim and gpsim
Drilled <- drillsim(path =         path,
                    GBMsim.OP =    opsim,
                    GBMsim.GP =    gpsim,
                    nrun =         opt$nrun,
                    drilled.init = opt$drilled.init,
                    ver =          opt$file_ver)


# 3.1 Well data simulation ------------------------------------------------

# Run well data simulation function
wsim <- welldata(path =            path,
                 sched.type =      opt$sched.type,
                 Drilled =         Drilled,
                 timesteps =       opt$MC.tsteps,
                 nrun =            opt$nrun,
                 field =           opt$field,
                 ver =             opt$file_ver,
                 production.type = opt$prod.type,
                 basis =           opt$cpi)


# 3.2 Production simulation -----------------------------------------------

# Run production simulation function
psim <- productionsim(path =            path,
                      nrun =            opt$nrun,
                      timesteps =       opt$MC.tsteps,
                      production.type = opt$prod.type,
                      ver =             opt$file_ver)

# # 3.3 Royalties -----------------------------------------------------------
# 
# # Run royalty calculation
# rsim <- royalty(op =   op,
#                 gp =   gp)
# 
# 
# # 3.4 Severance Taxes -----------------------------------------------------
# 
# # Get indices of oil and gas wells
# ind.ow <- which(wsim$wellType == "OW")
# ind.gw <- which(wsim$wellType == "GW")
# 
# # Run severance tax calculation
# stsim <- stax(wsim =   wsim,
#               psim =   psim,
#               rsim =   rsim,
#               op =     op,
#               gp =     gp,
#               ind.ow = ind.ow,
#               ind.gw = ind.gw,
#               API =    opt$API)
# 
# 
# # 3.5 Property taxes ------------------------------------------------------
# 
# # Run property tax calculation. Right now there are issues with the NPV < 0 in
# # many cases, so only return from ptax function is lease operating costs (LOC).
# LOC <- ptax(data_root = data_root,
#             wsim = wsim,
#             psim = psim,
#             op = op,
#             gp = gp,
#             ind.ow = ind.ow,
#             ind.gw = ind.gw,
#             basis = basis,
#             rsim = rsim,
#             stsim = stsim)
# 
# 
# # 3.6 Corporate income taxes ----------------------------------------------
# 
# # Run corporate income tax calculation
# corp.tax <- ctax(ind.ow = ind.ow,
#                  ind.gw = ind.gw)
# 
# # Split out individual matrices from list
# ciSO <- corp.tax[[1]] # Corporate state income taxes for oil
# ciSG <- corp.tax[[2]] # Corporate state income taxes for gas
# ciFO <- corp.tax[[3]] # Corporate federal income taxes for oil
# ciFG <- corp.tax[[4]] # Corporate federal income taxes for gas
# 
# # Remove list
# remove(corp.tax)
# 
# # 3.7 Employment ----------------------------------------------------------
# 
# # RIMS II model employment estimate
# jobs.RIMS <- RIMS(multiplier = 2.2370,
#                   wsim = wsim,
#                   LOC = LOC,
#                   nrun = nrun)
# 
# # Workload-based model employment estimate. See workload.R to change model
# # constants (too many to pass as input arguments here).
# jobs.workload <- workload(wsim = wsim,
#                           psim = psim,
#                           nrun = nrun,
#                           timesteps = timesteps)
# 
# 
# # 3.8 Emissions -----------------------------------------------------------
# 
# # Determine GHG emissions as (1) 1e3 kg CO2e and (2) MCF of CH4
# emissions <- GHG(wsim = wsim,
#                  psim = psim,
#                  nrun = nrun,
#                  timesteps = timesteps,
#                  ind.ow = ind.ow,
#                  ind.gw = ind.gw,
#                  truckload = 200)       # Capacity of oil trucks in bbl
# 
# 
# # 3.9 Water Balance -------------------------------------------------------
# 
# # Determine water balance
# water.balance <- water(wsim = wsim,
#                        psim = psim,
#                        data_root = data_root)


# 3.10 Energy Balance -----------------------------------------------------

# blah


# 4.1 Post processing -----------------------------------------------------

# blah

