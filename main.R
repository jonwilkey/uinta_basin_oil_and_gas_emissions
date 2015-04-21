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
path$raw  <-    paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Raw Data", sep = "")
path$data <-    paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Prepared Data", sep = "")
path$look <-    paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Lookup Tables", sep = "")
path$plot <-    paste(pwd.drop, "Dropbox/CLEAR/DOGM Data/Plots", sep = "")
path$work <-    paste(pwd.git,  "ub_oilandgas/", sep = "")
path$fun  <-    paste(pwd.git,  "ub_oilandgas/Functions", sep = "")
path$plotfun <- paste(pwd.git,  "ub_oilandgas/Functions/Plotting", sep = "")

# Remove temporary path objects
remove(pwd.drop, pwd.git)

# Set working directory
setwd(path$work)


# 1.2 Functions -----------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(path$fun, c("GBMsim.R",
                              "EIAsim.R",
                              "drillsim.R",
                              "priorProd.R",
                              "priorInfo.R",
                              "sim_tdelay.R",
                              "sim_DCC.R",
                              "sim_depth.R",
                              "sim_EF.R",
                              "sim_fieldnum.R",
                              "sim_lease.R",
                              "sim_rework.R",
                              "sim_dupRework.R",
                              "sim_NTIfrac.R",
                              "sim_pTaxfrac.R",
                              "sim_tdrill.R",
                              "sim_water.R",
                              "sim_wcost.R",
                              "sim_wellType.R",
                              "productionsim.R",
                              "royalty.R",
                              "stax.R",
                              "ctax.R",
                              "ptax.R",
                              "Ecalc.R",
                              #"RIMS.R",
                              #"workload.R",
                              "water.R",
                              "clipboard.R",
                              "inf_adj.R",
                              "CDFd.R",
                              "CDFq.R"))

# Load each function in list then remove temporary file list variables
for (f in flst) source(f); remove(f, flst)


# 1.3 Libraries -----------------------------------------------------------

library(foreign)
library(plyr)
library(zoo)
library(data.table)
library(sqldf)
library(minpack.lm)
library(scatterplot3d)


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
                 ver =  opt$file_ver)
}

# Load production.rda and rename as "p" for brevity
load(file.path(path$data, paste("production_", opt$file_ver, ".rda", sep = "")))

# Make subset of "production" called "p" based on criteria in opt$keeps
p <- production
p <- subset(p, subset = switch(opt$psub,
                               a = {(w_county == "UINTAH" |
                                     w_county == "DUCHESNE")}),
            select = opt$p.keep)


# 2.2 Schedule update and CDF generation ----------------------------------

# Run function if opt$schedule.update flag is set to "TRUE"
if(opt$schedule.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "scheduleUpdate.R"))
  
  # Function call
  scheduleUpdate(path =            path,
                 p =               p,
                 tsteps =          opt$SU.tsteps,
                 min.depth =       opt$min.well.depth,
                 max.depth =       opt$max.well.depth,
                 well.depth.step = opt$well.depth.step,
                 ver =             opt$file_ver,
                 field.cutoff =    opt$field.cutoff)
}

# Load data.frames from scheduleUpdate function:
# - well.actual:      actual well data information
# - cdf.depth.ow/gw:  CDFs for well depth based on well type
# - cdf.ff:           CDFs for probability of a well being located in a given field
# - cdf.flt:          CDFs for surface lease types (federal, state, etc.) by field
# - prob:             Probability that a well located in a given field is dry well or gas well
# - field:            Vector of field numbers to be considered individually
load(file.path(path$data, paste("well_actual_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("cdf_schedule_", opt$file_ver, ".rda", sep = "")))


# 2.3 Lease operating cost lm() fit update --------------------------------

# Run function if opt$lease.update flag is set to "TRUE"
if(opt$lease.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "leaseOpCostUpdate.R"))
  
  # Function call
  leaseOpCostUpdate(path =     path,
                    ver =      opt$file_ver,
                    tstart =   opt$LU.tstart,
                    tstop =    opt$LU.tstop,
                    basis =    opt$cpi,
                    LOCbasis = opt$LOCbasis)
}

# Load data.frames from leaseOpCost function:
# - fit.LOC.oil/gas: lm() object with fit for oil/gas lease operating costs
# - LOC.oil/gas:     lease operating cost data for oil/gas wells
load(file.path(path$data, paste("leaseOpCost_", opt$file_ver, ".rda", sep = "")))


# 2.4 EIA energy price history --------------------------------------------

# Run function if opt$EIAprice.update is set to "TRUE"
if(opt$EIAprice.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "EIApriceUpdate.R"))
  
  # Function call
  EIApriceUpdate(path =            path,
                 EP.CPI.basis =    opt$EP.CPI.basis,
                 cpi =             opt$cpi,
                 ver =             opt$file_ver)
}

# Load EIAprices_v*.rda to load eia.hp (EIA historical energy prices) data.frame
load(file.path(path$data, paste("EIAprices_", opt$file_ver, ".rda", sep = "")))


# 2.5 Corporate income tax conversion factor CDF generation ---------------

# Run function if opt$corptax.update flag is set to "TRUE"
if(opt$corptax.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "corpIncomeUpdate.R"))
  
  # Function call
  corpIncomeUpdate(production = production,
                   path =       path,
                   basis =      opt$cpi,
                   ver =        opt$file_ver,
                   NTI =        opt$NTI,
                   eia.hp =     eia.hp)
}

# Load net taxable income statistics (corpNTIfrac)
load(file.path(path$data, paste("corpNTIfrac_", opt$file_ver, ".rda", sep = "")))


# 2.6 Property tax update -------------------------------------------------

# Run function if opt$ptax.update flag is set to "TRUE"
if(opt$ptax.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "propertyTaxUpdate.R"))
  
  # Function call
  propertyTaxUpdate(p =      p,
                    path =   path,
                    basis =  opt$cpi,
                    ver =    opt$file_ver,
                    PTI =    opt$PTI,
                    eia.hp = eia.hp)
}

# Load property tax statistics (pTaxRate)
load(file.path(path$data, paste("pTaxRate_", opt$file_ver, ".rda", sep = "")))


# 2.7 Drilling Schedule Model lm() fit update -----------------------------

# Run function if opt$drillmodel.update flag is set to "TRUE"
if(opt$drillmodel.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "drillingModelUpdate.R"))
  
  # Function call
  drillingModelUpdate(path =      path,
                      p =         p,
                      min.depth = opt$min.well.depth,
                      tstart =    opt$DMU.tstart,
                      tstop =     opt$DMU.tstop,
                      ver =       opt$file_ver,
                      eia.hp =    eia.hp)
}

# Load economic drilling model fit (drillModel) and data (drillModelData)
load(file.path(path$data, paste("drillModel_", opt$file_ver, ".rda", sep = "")))


# 2.8 GBM parameter fit update --------------------------------------------

# Run function if opt$GBMfit.update flag is set to "TRUE"
if(opt$GBMfit.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "GBMfitUpdate.R"))
  
  # Function call
  GBMfitUpdate(path =   path,
               eia.hp = eia.hp,
               tstart = opt$GBM.tstart,
               tstop =  opt$GBM.tstop,
               ver =    opt$file_ver)
}

# Load GBM parameter fits (GBMfitOP and GBMfitGP)
load(file.path(path$data, paste("GBMfit_", opt$file_ver, ".rda", sep = "")))


# 2.9 EIA Forecast Update -------------------------------------------------

# Run function if opt$EIAforecast.update flag is set to "TRUE"
if(opt$EIAforecast.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "EIAforecastUpdate.R"))
  
  # Function call
  EIAforecastUpdate(forecast <-     opt$forecast,
                    basis <-        opt$cpi,
                    EIAbasis <-     opt$EIAbasis,
                    tsteps <-       opt$tsteps,
                    oil.fpp.init <- opt$oil.fpp.init,
                    gas.fpp.init <- opt$gas.fpp.init,
                    FPPdate <-      opt$FPPdate,
                    ver =           opt$file_ver,
                    path =          path)
}

# Load EIA forecast vector
load(file.path(path$data, paste("EIAforecast_", opt$file_ver, ".rda", sep = "")))


# 2.10 EIA Error Analysis Update ------------------------------------------

# Run function if opt$EIAerror.update flag is set to "TRUE"
if(opt$EIAerror.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "EIAerrorUpdate.R"))
  
  # Function call
  EIAerrorUpdate(path =   path,
                 xq =     opt$xq,
                 tsteps = opt$EEU.tsteps,
                 ver =    opt$file_ver)
}

# Load EIA error CDF matrices (Eoil and Egas)
load(file.path(path$data, paste("EIAerror_", opt$file_ver, ".rda", sep = "")))


# 2.11 Decline curve analysis update --------------------------------------

# Run function if opt$DCA.update flag is set to "TRUE"
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
            field =           field,
            ver =             opt$file_ver,
            path =            path,
            p =               p,
            Cp.start.oil =    opt$Cp.start.oil,
            c1.start.oil =    opt$c1.start.oil,
            Qlower.oil =      opt$Qlower.oil,
            Qupper.oil =      opt$Qupper.oil,
            Cp.start.gas =    opt$Cp.start.gas,
            c1.start.gas =    opt$c1.start.gas,
            Qlower.gas =      opt$Qlower.gas,
            Qupper.gas =      opt$Qupper.gas,
            tstart =          opt$DCA.tstart,
            tstop =           opt$DCA.tstop,
            tend =            opt$tstart)
}

# Load DCA fits mo (oil) and mg (gas)
load(file.path(path$data, paste("DCA_fits_", opt$file_ver, ".rda", sep = "")))


# 2.12 Field DCA Update ---------------------------------------------------

# Run function if opt$field.DCA.update flag is set to "TRUE"
if(opt$field.DCA.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "DCAupdateField.R"))
  source(file.path(path$fun, "QfitDCAupdateField.R"))
  
  # Function call for hyperbolic Field Level DCA fits
  DCAupdateField(path =         path,
                 p =            p,
                 minDayProd =   opt$minDayProd,
                 field =        field,
                 DCAplot =      opt$DCAplot,
                 ver =          opt$file_ver,
                 b.start.oil =  opt$b.start.oil,
                 Di.start.oil = opt$Di.start.oil,
                 lower.oil =    opt$lower.oil,
                 upper.oil =    opt$upper.oil,
                 b.start.gas =  opt$b.start.gas,
                 Di.start.gas = opt$Di.start.gas,
                 lower.gas =    opt$lower.gas,
                 upper.gas =    opt$upper.gas,
                 quant =        opt$quant,
                 tstart =       opt$FDC.tstart,
                 tstop =        opt$FDC.tstop)
  
  # Function call for cumulative Field Level DCA fits
  QDCAupdateField(path =         path,
                  p =            p,
                  minDayProd =   opt$minDayProd,
                  field =        field,
                  DCAplot =      opt$DCAplot,
                  ver =          opt$file_ver,
                  Cp.start.oil = opt$Cp.start.oil,
                  c1.start.oil = opt$c1.start.oil,
                  Qlower.oil =   opt$Qlower.oil,
                  Qupper.oil =   opt$Qupper.oil,
                  Cp.start.gas = opt$Cp.start.gas,
                  c1.start.gas = opt$c1.start.gas,
                  Qlower.gas =   opt$Qlower.gas,
                  Qupper.gas =   opt$Qupper.gas,
                  quant =        opt$quant,
                  tstart =       opt$FDC.tstart,
                  tstop =        opt$FDC.tstop)
}

# Load field-level hyperbolic (hypFF) and cumulative (QFF) DCA fits
load(file.path(path$data, paste("DCA_field_fits_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("DCA_field_Qfits_", opt$file_ver, ".rda", sep = "")))


# 2.13 DCA CDF Update ------------------------------------------------------

# Run function if opt$DCA.CDF.update flag is set to "TRUE"
if(opt$DCA.CDF.update == TRUE) {
  
  # Source functions to load
  source(file.path(path$fun, "DCAupdateCDF.R"))
  source(file.path(path$fun, "QfitDCAupdateCDF.R"))
  
  # Function call for hyperbolic DCA CDFs
  DCAupdateCDF(field =        field,
               ver =          opt$file_ver,
               DCA.CDF.type = opt$DCA.CDF.type,
               cdf.oil.from = opt$cdf.oil.from,
               cdf.oil.to =   opt$cdf.oil.to,
               cdf.oil.np =   opt$cdf.oil.np,
               cdf.gas.from = opt$cdf.gas.from,
               cdf.gas.to =   opt$cdf.gas.to,
               cdf.gas.np =   opt$cdf.gas.np,
               DCA.CDF.xq =   opt$xq,
               path =         path,
               tstart =       opt$DCAcdf.tstart,
               tstop =        opt$DCAcdf.tstop,
               mo =           mo,
               mg =           mg)
  
  # Function call for cumulative DCA CDFs
  QfitDCAupdateCDF(field =          field,
                   ver =            opt$file_ver,
                   DCA.CDF.type   = opt$DCA.CDF.type,
                   Q.cdf.oil.from = opt$Q.cdf.oil.from,
                   Q.cdf.oil.to =   opt$Q.cdf.oil.to,
                   Q.cdf.oil.np =   opt$Q.cdf.oil.np,
                   Q.cdf.gas.from = opt$Q.cdf.gas.from,
                   Q.cdf.gas.to =   opt$Q.cdf.gas.to,
                   Q.cdf.gas.np =   opt$Q.cdf.gas.np,
                   DCA.CDF.xq =     opt$xq,
                   path =           path,
                   tstart =         opt$DCAcdf.tstart,
                   tstop =          opt$DCAcdf.tstop,
                   mo =             mo,
                   mg =             mg)
}

# Load CDFs for DCA fits
load(file.path(path$data, paste("DCA_CDF_coef_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("Q_DCA_CDF_coef_", opt$file_ver, ".rda", sep = "")))


# 2.14 Drilling and Completion Capital Cost Model Update -------------------

# Run function if opt$drillCapCost.update flag is set to "TRUE"
if(opt$drillCapCost.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "drillCapCostUpdate.R"))
  
  drillCapCostUpdate(path =  path,
                     basis = opt$cpi,
                     ver =   opt$file_ver)
}

# Load drilling cost data (drillCost.data), fit (drillCost.fit), and completion
# cost ratio (complCR)
load(file.path(path$data, paste("drillCost_", opt$file_ver, ".rda", sep = "")))


# 2.15 Water balance data analysis and update -----------------------------

# Run function if opt$water.update flag is set to "TRUE"
if(opt$water.update == TRUE) {
  
  # Load function
  source(file.path(path$fun, "waterUpdate.R"))
  
  # Function call
  waterUpdate(path =        path,
              p =           p,
              tstart =      opt$WU.tstart,
              tstop =       opt$WU.tstop,
              xq =          opt$xq,
              f_mud =       opt$f_mud,
              f_cem =       opt$f_cem,
              rcut.pw.oil = opt$rcut.pw.oil,
              rcut.pw.gas = opt$rcut.pw.gas,
              rcut.disp =   opt$rcut.disp,
              ver =         opt$file_ver)
}

# Load water balance term CDFs (cdf.water) and regression models (water.lm)
load(file.path(path$data, paste("water_models_", opt$file_ver, ".rda", sep = "")))


# 2.16 Rework CDF analysis and update -------------------------------------

# Run function if opt$rework.update flag is set to "TRUE"
if(opt$rework.update == TRUE) {
  
  # Load function
  source(file.path(path$fun, "reworkUpdate.R"))
  
  # Function call
  reworkUpdate(path =   path,
               p =      p,
               tstart = opt$RWU.tstart,
               tstop =  opt$RWU.tstop,
               ver =    opt$file_ver)
}

# Load rework CDF (cdf.rework)
load(file.path(path$data, paste("rework_", opt$file_ver, ".rda", sep = "")))


# 3.1 Energy price path simulation ----------------------------------------

# Switch for price path simulation method.
switch(opt$ep.type,
       
       # If GBM simulation, ep.type == "a"
       a = {
         
         # Run Geometric Brownian Motion (GBM) price path simulation
         epsim <- GBMsim(path =         path,
                         oil.fpp.init = opt$oil.fpp.init,
                         gas.fpp.init = opt$gas.fpp.init,
                         timesteps =    opt$MC.tsteps,
                         nrun =         opt$nrun,
                         GBMfitOP =     GBMfitOP,
                         GBMfitGP =     GBMfitGP)
         
         # Extract individual data.frames from list object
         op <- epsim$op
         gp <- epsim$gp
         
         # Remove list
         remove(epsim)
       },
       
       # If EIA forecast with error propagation, ep.type == "b"
       b = {
         
         # Run EIAsim
         epsim <- EIAsim(nrun <-  opt$nrun,
                         Eoil <-  Eoil,
                         Egas <-  Egas,
                         op.FC <- op.FC,
                         gp.FC <- gp.FC)
         
         # Extract objects from list
         op <- epsim$op
         gp <- epsim$gp
         
         # Remove list
         remove(epsim)
       })


# 3.2 Well drilling simulation --------------------------------------------

# Run well drilling simulation given price paths opsim and gpsim
Drilled <- drillsim(path =         path,
                    GBMsim.OP =    op,
                    GBMsim.GP =    gp,
                    nrun =         opt$nrun,
                    drilled.init = opt$drilled.init,
                    drillModel =   drillModel,
                    type =         opt$DStype,
                    p =            p,
                    tstart =       opt$tstart,
                    tstop =        opt$tstop)


# 3.3 Prior production calculation ----------------------------------------

# Run prior oil and gas production calculation
ppri <- priorProd(hypFF =     hypFF,
                  mo =        mo,
                  mg =        mg,
                  MC.tsteps = opt$MC.tsteps,
                  acut =      opt$acut)

# Get prior well info
prior.Info <- priorInfo(apilist = ppri$apilist,
                        p =       p)


# 3.3 Monte-Carlo Loop ----------------------------------------------------

# The following for-loop calculates all terms for each well in a single 
# Monte-Carlo (MC) iteration and returns the results as a total to cut down on
# the memory usage for all the objects in the for-loop

# Preallocate results matrices
osim <-    matrix(0, nrow = opt$nrun, ncol = opt$MC.tsteps)
gsim <-    osim # osim/gsim total oil/gas production (bbl or MCF)
roy.oil <- osim # royalty totals from oil production
roy.gas <- osim # royalty totals from gas production
st.oil <-  osim # severance tax totals from oil production
st.gas <-  osim # severance tax totals from gas production
PT <-      osim # property tax totals
CTstate <- osim # corporate state income tax totals
CTfed <-   osim # corporate federal income tax totals
CO2 <-     osim # CO2 emission totals (metric tons)
CH4 <-     osim # CH4 emission totals (metric tons)
VOC <-     osim # VOC emission totals (metric tons)
rCO2 <-    osim # Reduced CO2 emission totals (metric tons)
rCH4 <-    osim # Reduced CH4 emission totals (metric tons)
rVOC <-    osim # Reduced VOC emission totals (metric tons)
w.pw <-    osim # Produced water totals (bbl)
w.disp <-  osim # Total water disposed of via injection wells (bbl)
w.evap <-  osim # Total water evaporated in ponds (bbl)
w.rec <-   osim # Water recycle totals (bbl)
w.dw <-    osim # Total water usage for drilling (mud and cement - bbl)
w.fw <-    osim # Total water usage for hydraulic fracturing (bbl)
w.inj <-   osim # Total water usage for water flooding (bbl)
w.in <-    osim # Total water coming into system (bbl)
w.r <-     osim # Ratio of (water in) / (oil production)

# Progress Bar (since this next for-loop takes a while)
pb <- txtProgressBar(min = 0, max = opt$nrun, width = 50, style = 3)

# For each runID
for (i in 1:opt$nrun) {
  
  # 3.3.1 Well data simulation -------------------------------------------
  
  # Get time step that each well is drilled
  wsim <- data.frame(tDrill = sim_tdrill(Drilled = Drilled[i,]))
  
  # Get field numbers
  wsim$fieldnum <- sim_fieldnum(cdf.ff = cdf.ff,
                                times =  length(wsim$tDrill))
  
  # Get well types
  wsim$wellType <- sim_wellType(fieldnum = wsim$fieldnum,
                                prob =     prob)
  
  # Get time delays for oil/gas production
  wsim <- cbind(wsim, sim_tdelay(times =            nrow(wsim),
                                 field =            field,
                                 fieldnum =         wsim$fieldnum,
                                 DCA.cdf.coef.oil = DCA.cdf.coef.oil,
                                 DCA.cdf.coef.gas = DCA.cdf.coef.gas))
  
  # Get well depth
  wsim$depth <- sim_depth(wellType =     wsim$wellType,
                          cdf.depth.ow = cdf.depth.ow,
                          cdf.depth.gw = cdf.depth.gw)
  
  # Get lease type
  wsim$lease <- sim_lease(fieldnum = wsim$fieldnum,
                          cdf.flt =  cdf.flt)
  
  # Pick well rework time steps for new wells
  wsim$rework <- sim_rework(type =       "new",
                            wellType =   wsim$wellType,
                            tDrill =     wsim$tDrill,
                            td.oil =     wsim$td.oil,
                            td.gas =     wsim$td.gas,
                            cdf.rework = cdf.rework,
                            timesteps =  opt$MC.tsteps)
  
  # Pick well rework time steps for existing wells
  wpri <- cbind(prior.Info, rework = sim_rework(type =       "prior",
                                                wellType =   prior.Info$wellType,
                                                cdf.rework = cdf.rework,
                                                timesteps =  opt$MC.tsteps,
                                                firstprod =  mo$firstprod[mo$tend > 0],
                                                tstart =     opt$tstart))
  
  # Duplicate reworked wells
  wsim <- sim_dupRework(wsim = wsim)
  
  # Pick decline curve coefficients
  wsim <- cbind(wsim, sim_DCC(decline.type =       opt$mc.decline.type,
                              times =              nrow(wsim),
                              field =              field,
                              fieldnum =           wsim$fieldnum,
                              DCA.cdf.coef.oil =   DCA.cdf.coef.oil,
                              DCA.cdf.coef.gas =   DCA.cdf.coef.gas,
                              Q.DCA.cdf.coef.oil = Q.DCA.cdf.coef.oil,
                              Q.DCA.cdf.coef.gas = Q.DCA.cdf.coef.gas))
  
  # Pick net taxable income fraction
  wsim$NTIfrac <- sim_NTIfrac(times = nrow(wsim), corpNTIfrac = corpNTIfrac)
  wpri$NTIfrac <- sim_NTIfrac(times = nrow(wpri), corpNTIfrac = corpNTIfrac)
  
  # Pick property tax fraction
  wsim$pTaxfrac <- sim_pTaxfrac(times = nrow(wsim), pTaxRate = pTaxRate)
  wpri$pTaxfrac <- sim_pTaxfrac(times = nrow(wpri), pTaxRate = pTaxRate)
  
  # Calculate well drilling and completion capital cost for new wells
  wsim <- cbind(wsim, sim_wcost(type =          "new",
                                depth =         wsim$depth,
                                drillCost.fit = drillCost.fit,
                                complCR =       complCR,
                                rework =        wsim$rework))
  
  # Calculate well drilling and completion capital cost for existing wells
  wpri <- cbind(wpri, sim_wcost(type =          "prior",
                                depth =         wpri$depth,
                                drillCost.fit = drillCost.fit,
                                complCR =       complCR,
                                rework =        wpri$rework))
  
  # Pick emission factors
  wsim <- cbind(wsim, sim_EF(times = nrow(wsim), EF = opt$EF))
  wpri <- cbind(wpri, sim_EF(times = nrow(wpri), EF = opt$EF))
  
  # Pick water factors
  wsim <- cbind(wsim, sim_water(wellType = wsim$wellType, cdf.water = cdf.water))
  wpri <- cbind(wpri, sim_water(wellType = wpri$wellType, cdf.water = cdf.water))
  
  
  # 3.3.2 Production simulation ------------------------------------------
  
  # Run production simulation function
  psim <- productionsim(wsim =            wsim,
                        timesteps =       opt$MC.tsteps,
                        production.type = opt$prod.type,
                        decline.type =    opt$mc.decline.type,
                        osim.actual =     osim.actual,
                        gsim.actual =     gsim.actual,
                        acut =            opt$acut)
  
  
  # 3.3.3 Royalties -----------------------------------------------------
  
  # Calculate royalty for oil production
  t.roy.oil <- royalty(royaltyRate = opt$royaltyRate,
                       ep =          op[i,],
                       lease =       c(wsim$lease, wpri$lease),
                       psim =        rbind(psim$osim, ppri$oil))
  
  # Calculate royalty for gas production
  t.roy.gas <- royalty(royaltyRate = opt$royaltyRate,
                       ep =          gp[i,],
                       lease =       c(wsim$lease, wpri$lease),
                       psim =        rbind(psim$gsim, ppri$gas))
  
  
  # 3.3.4 Severance Taxes ----------------------------------------------
    
  # Calculate ST for oil production
  t.st.oil <- stax(type =    "oil",
                   tDrill =  c(wsim$tDrill, wpri$tDrill),
                   psim =    rbind(psim$osim, ppri$oil),
                   rsim =    t.roy.oil,
                   ep =      op[i,],
                   st.low =  opt$st.low,
                   st.high = opt$st.high,
                   st.con =  opt$st.con,
                   st.cut =  opt$st.ocut,
                   st.skip = opt$st.skip,
                   strip =   opt$strip.oil)
  
  # Calculate ST for gas production
  t.st.gas <- stax(type =    "gas",
                   tDrill =  c(wsim$tDrill, wpri$tDrill),
                   psim =    rbind(psim$gsim, ppri$gas),
                   rsim =    t.roy.gas,
                   ep =      gp[i,],
                   st.low =  opt$st.low,
                   st.high = opt$st.high,
                   st.con =  opt$st.con,
                   st.cut =  opt$st.gcut,
                   st.skip = opt$st.skip,
                   strip =   opt$strip.gas)
  
  
  # 3.3.5 Property taxes -----------------------------------------------
  
  # Calculate property taxes
  t.PT <- ptax(OP =       op[i,],
               GP =       gp[i,],
               osim =     rbind(psim$osim, ppri$oil),
               gsim =     rbind(psim$gsim, ppri$gas),
               pTaxfrac = c(wsim$pTaxfrac, wpri$pTaxfrac))
  
  
  # 3.3.6 Corporate income taxes ---------------------------------------
  
  # Run corporate income tax calculation
  CT <- ctax(OP =           op[i,],
             GP =           gp[i,],
             osim =         rbind(psim$osim, ppri$oil),
             gsim =         rbind(psim$gsim, ppri$gas),
             NTIfrac =      c(wsim$NTIfrac, wpri$NTIfrac),
             CIrate.state = opt$CIrate.state,
             CIrate.fed =   opt$CIrate.fed)
  
  
  # 3.3.7 Emissions ----------------------------------------------------
  
  # Calculate emissions from new wells
  ETsim <- Ecalc(osim =        psim$osim,
                 gsim =        psim$gsim,
                 wsim =        wsim,
                 tstart =      opt$tstart,
                 edcut =       opt$edcut,
                 EFred.Nov12 = opt$EFred.Nov12)
  
  # Calculate emissions from existing wells
  ETpri <- Ecalc(osim =        ppri$oil,
                 gsim =        ppri$gas,
                 wsim =        wpri,
                 tstart =      opt$tstart,
                 edcut =       opt$edcut,
                 EFred.Nov12 = opt$EFred.Nov12)
  
  
  # 3.3.8 Water Balance -------------------------------------------------
  
  # Calculate water balance
  WB <- water(wsim =     rbind(wsim[,41:45], wpri[,36:40]),
              osim =     rbind(psim$osim, ppri$oil),
              gsim =     rbind(psim$gsim, ppri$gas),
              wellType = c(wsim$wellType, wpri$wellType),
              depth =    c(wsim$depth, wpri$depth),
              dw.lm =    water.lm$dw.lm)
  
  
  # 3.3.x Get totals for MC run i ---------------------------------------
  
  # Calculate column sums for each results matrix generated to get totals
  osim[i,] <-    colSums(psim$osim)
  gsim[i,] <-    colSums(psim$gsim)
  roy.oil[i,] <- colSums(t.roy.oil)
  roy.gas[i,] <- colSums(t.roy.gas)
  st.oil[i,] <-  colSums(t.st.oil)
  st.gas[i,] <-  colSums(t.st.gas)
  PT[i,] <-      colSums(t.PT)
  CTstate[i,] <- colSums(CT$state)
  CTfed[i,] <-   colSums(CT$fed)
  CO2[i,] <-     colSums(rbind(ETsim$CO2, ETpri$CO2))
  CH4[i,] <-     colSums(rbind(ETsim$CH4, ETpri$CH4))
  VOC[i,] <-     colSums(rbind(ETsim$VOC, ETpri$VOC))
  rCO2[i,] <-    colSums(rbind(ETsim$rCO2, ETpri$rCO2))
  rCH4[i,] <-    colSums(rbind(ETsim$rCH4, ETpri$rCH4))
  rVOC[i,] <-    colSums(rbind(ETsim$rVOC, ETpri$rVOC))
  w.pw[i,] <-    WB$pw
  w.disp[i,] <-  WB$disp
  w.evap[i,] <-  WB$evap
  w.rec[i,] <-   WB$recycle
  w.dw[i,] <-    WB$dw
  w.fw[i,] <-    WB$fw
  w.inj[i,] <-   WB$inj
  w.in[i,] <-    WB$wtr.in
  w.r[i,] <-     WB$wtr.r
  
  
  # 3.3.x Cleanup -------------------------------------------------------
  
#   # Remove temporary results
#   remove(wsim,
#          psim,
#          t.roy.oil,
#          t.roy.gas,
#          t.st.oil,
#          t.st.gas,
#          t.PT,
#          CT,
#          ETsim,
#          ETpri,
#          WB)
  
  # Update progress bar
  Sys.sleep(1e-3)
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)


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

# 3.10 Energy Balance -----------------------------------------------------

# blah


# 4.1 Post processing -----------------------------------------------------

# Run processing script to generate plots of results
source(file.path(path$fun, "postProcess.R"))
