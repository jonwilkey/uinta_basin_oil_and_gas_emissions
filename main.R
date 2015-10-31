# Script Info -------------------------------------------------------------
# Name:      main.R (Uinta Basin Oil and Gas Production Model -  Main Script)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Description -------------------------------------------------------------

# This script does everything. Structure is as follows:

# 1. Set simulation environment, including directory paths, functions, libraries
#    and loading user input/output options.
# 2. Perform data analysis functions on UDOGM database files.
# 3. Run Monte-Carlo (MC) simulation.
# 4. Perform post-processing calculations and generate plots.


# 1.1 Paths ---------------------------------------------------------------

# Predefine list object "path" for holding directory path listings
path <- NULL

# Path switch - replace with the path directory for your local copy of the Git
# repository and Dropbox files.
pwd.drop <- "C:/Users/jonwi/"
pwd.git  <- "C:/Users/jonwi/Documents/R/"
  
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
                              "sim_county.R",
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
                              "priorProdReworkAdjust.R",
                              "royalty.R",
                              "stax.R",
                              "ctax.R",
                              "ptax.R",
                              "Ecalc.R",
                              "LECcalc.R",
                              "LOCcalc.R",
                              "RIMS.R",
                              "workload.R",
                              "water.R",
                              "clipboard.R",
                              "inf_adj.R",
                              "CDFd.R",
                              "CDFq.R",
                              "asYear.R"))

# Load each function in list then remove temporary file list variables
for (f in flst) source(f); remove(f, flst)


# 1.3 Packages -----------------------------------------------------------

library(foreign)
library(plyr)
library(zoo)
library(data.table)
library(sqldf)
library(minpack.lm)
library(scatterplot3d)
library(beepr)
library(fitdistrplus)
library(lhs)
library(xtable)


# 1.4 Options -------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

# Run script "IO_options.R" to load user defined input/output options
source("IO_options.R")

# Set seed for random number generation (for reproducibility)
set.seed(1)

# Print and save start time for data analysis section
writeLines(c("",
           "Running data analysis functions and/or loading data analysis results",
           paste("Start time:",Sys.time())))
runstart <- Sys.time()


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
# - prob:             Probability for well type and county by field number
# - field:            Vector of field numbers to be considered individually
load(file.path(path$data, paste("well_actual_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("cdf_schedule_", opt$file_ver, ".rda", sep = "")))


# 2.3 EIA energy price history --------------------------------------------

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


# 2.4 Lease operating cost lm() fit update --------------------------------

# Run function if opt$lease.update flag is set to "TRUE"
if(opt$lease.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "leaseCostUpdate.R"))
  
  # Function call
  leaseCostUpdate(path =     path,
                  ver =      opt$file_ver,
                  tstart =   opt$LU.tstart,
                  tstop =    opt$LU.tstop,
                  basis =    opt$cpi,
                  LOCbasis = opt$LOCbasis,
                  eia.ep =   eia.hp)
}

# Load data.frames from leaseCostUpdate function:
# - LOC.oil/gas.equip/op: lm() object with fit for oil/gas lease capital/operating costs
# - LOC.data:             lease capital/operating cost data for oil/gas wells
load(file.path(path$data, paste("leaseCost_", opt$file_ver, ".rda", sep = "")))


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

# Load economic drilling model fits (drillModel) and data (drillModelData)
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
  
  # Function call - Reference Case
  EIAforecastUpdate(forecast <-     opt$forecast[opt$forecast$type == "ref",],
                    basis <-        opt$cpi,
                    EIAbasis <-     opt$EIAbasis,
                    tsteps <-       opt$tsteps,
                    oil.fpp.init <- opt$oil.fpp.init,
                    gas.fpp.init <- opt$gas.fpp.init,
                    FPPdate <-      opt$FPPdate,
                    ver =           opt$file_ver,
                    path =          path,
                    type =          "ref")
  
  # Function call - Low Oil Case
  EIAforecastUpdate(forecast <-     opt$forecast[opt$forecast$type == "low",],
                    basis <-        opt$cpi,
                    EIAbasis <-     opt$EIAbasis,
                    tsteps <-       opt$tsteps,
                    oil.fpp.init <- opt$oil.fpp.init,
                    gas.fpp.init <- opt$gas.fpp.init,
                    FPPdate <-      opt$FPPdate,
                    ver =           opt$file_ver,
                    path =          path,
                    type =          "low")
  
  # Function call - High Oil Case
  EIAforecastUpdate(forecast <-     opt$forecast[opt$forecast$type == "high",],
                    basis <-        opt$cpi,
                    EIAbasis <-     opt$EIAbasis,
                    tsteps <-       opt$tsteps,
                    oil.fpp.init <- opt$oil.fpp.init,
                    gas.fpp.init <- opt$gas.fpp.init,
                    FPPdate <-      opt$FPPdate,
                    ver =           opt$file_ver,
                    path =          path,
                    type =          "high")
}

# Load EIA forecast vector
# - op.FC.ref/low/high: Oil forecast for reference/low oil/high oil scenario
# - gp.FC.ref/low/high: Gas forecast for reference/low oil/high oil scenario
load(file.path(path$data, paste("EIAforecast_ref_",  opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("EIAforecast_low_",  opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("EIAforecast_high_", opt$file_ver, ".rda", sep = "")))


# 2.10 EIA Error Analysis Update ------------------------------------------

# Run function if opt$EIAerror.update flag is set to "TRUE"
if(opt$EIAerror.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "EIAerrorUpdate.R"))
  source(file.path(path$fun, "EIAerrorUpdateFrac.R"))
  
  # Function call - Relative error w/ directionality
  EIAerrorUpdate(path =   path,
                 xq =     opt$xq,
                 tsteps = opt$EEU.tsteps,
                 ver =    opt$file_ver)
  
  # Function call - Relative fractional error
  EIAerrorUpdateFrac(path =   path,
                 xq =     opt$xq,
                 tsteps = opt$EEU.tsteps,
                 ver =    opt$file_ver)
}

# Load EIA error CDF matrices (Eoil & Egas and EoilFrac & EgasFrac)
load(file.path(path$data, paste("EIAerror_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("EIAerrorFrac_", opt$file_ver, ".rda", sep = "")))


# 2.11 Decline curve analysis update --------------------------------------

# Run function if opt$DCA.update flag is set to "TRUE"
if(opt$DCA.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "DCAupdate.R"))
  
  # Function call - with tstart/tstop limits
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
            tend =            opt$tstart,
            full.fit =        FALSE)
  
  # Function call - without tstart/tstop limits
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
            tend =            opt$tstart,
            full.fit =        TRUE)
}

# Load DCA fits mo (oil) and mg (gas) as well as full fits (mof and mgf)
load(file.path(path$data, paste("DCA_fits_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("DCA_fits_full_", opt$file_ver, ".rda", sep = "")))


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
               ver =    opt$file_ver,
               wc.min = opt$wc.min)
}

# Load rework CDF (cdf.rework)
load(file.path(path$data, paste("rework_", opt$file_ver, ".rda", sep = "")))


# 2.17 DCA Coefficient Distribution Fitting -------------------------------

# Run function if opt$rework.update flag is set to "TRUE"
if(opt$DCAlnorm.update == TRUE) {
  
  # Load function
  source(file.path(path$fun, "DCAlnormUpdate.R"))
  
  # Function call
  DCAlnormUpdate(min.rec.count =  opt$DFmin.rec.count,
                 plot.flag =      opt$DFplot.flag,
                 mo =             mof,
                 mg =             mgf,
                 Q.cdf.oil.to =   opt$Q.cdf.oil.to,
                 Q.cdf.oil.from = opt$Q.cdf.oil.from,
                 Q.cdf.gas.to =   opt$Q.cdf.gas.to,
                 Q.cdf.gas.from = opt$Q.cdf.gas.from,
                 tstart =         opt$DF.tstart,
                 tstop =          opt$DF.tstop,
                 path =           path,
                 ver =            opt$file_ver,
                 field =          field)
}

# Load fitted distribution data.frame (DCAlnormFit)
load(file.path(path$data, paste("DCAlnormFit_", opt$file_ver, ".rda", sep = "")))



# Transition to MC simulation ---------------------------------------------

# Print stop time for data analysis section
writeLines(c("",
             paste("Finished data analysis at:", Sys.time()),
             paste("Elapsed time:", format(difftime(Sys.time(), runstart)))))

# Print and save start time for MC simulation
writeLines(c("",
             "Running Monte-Carlo Simulation",
             paste("Start time:",Sys.time())))
runstart <- Sys.time()


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
         epsim <- EIAsim(nrun =     opt$nrun,
                         Eoil =     Eoil,
                         Egas =     Egas,
                         EoilFrac = EoilFrac,
                         EgasFrac = EgasFrac,
                         op.FC =    op.FC.ref,
                         gp.FC =    gp.FC.ref,
                         type =     opt$EIA.ep.type,
                         fracProb = opt$EIA.fracProb)
         
         # Extract objects from list
         op <- epsim$op
         gp <- epsim$gp
         
         # Remove list
         remove(epsim)
       },
       
       # If using actual energy price paths, ep.type == "c"
       c = {
         
         # Get prices from eia.hp data.frame
         op <- matrix(rep(eia.hp$OP[(nrow(eia.hp)-opt$MC.tsteps+1):nrow(eia.hp)],
                          times = opt$nrun),
                      nrow = opt$nrun, ncol = opt$MC.tsteps, byrow = T)
         gp <- matrix(rep(eia.hp$GP[(nrow(eia.hp)-opt$MC.tsteps+1):nrow(eia.hp)],
                          times = opt$nrun),
                      nrow = opt$nrun, ncol = opt$MC.tsteps, byrow = T)
       })


# 3.2 Well drilling simulation --------------------------------------------

# Run well drilling simulation given price paths opsim and gpsim
Drilled <- drillsim(path =            path,
                    GBMsim.OP =       op,
                    GBMsim.GP =       gp,
                    nrun =            opt$nrun,
                    drilled.init =    opt$drilled.init,
                    drillModel =      drillModel,
                    type =            opt$DStype,
                    p =               p,
                    tstart =          opt$tstart,
                    tstop =           opt$tstop,
                    simtype =         opt$DSsimtype,
                    op.init =         opt$oil.fpp.init,
                    gp.init =         opt$gas.fpp.init)


# 3.3 Prior production calculations ---------------------------------------

# Run prior oil and gas production calculation
ppri <- priorProd(mo =        mo,
                  mg =        mg,
                  MC.tsteps = opt$MC.tsteps,
                  tend.cut =  opt$tend.cut)

# Get prior well info for wells with fits
prior.Info <- priorInfo(apilist = ppri$apilist,
                        p =       p,
                        field =   field)

# Get prior well info for wells w/o fits
pi.skip <- priorInfo(apilist = ppri$skip$api,
                     p =       p,
                     field =   field)

# Add in data from ppri function call to pi.skip
pi.skip <- cbind(pi.skip,
                 tend =      ppri$skip$tend,
                 firstprod = ppri$skip$firstprod)


# 3.3 Monte-Carlo Loop ----------------------------------------------------

# The following for-loop calculates all terms for each well in a single 
# Monte-Carlo (MC) iteration and returns the results as a total to cut down on
# the memory usage for all the objects in the for-loop

# Preallocate results matrices
osim <-    matrix(0, nrow = opt$nrun, ncol = opt$MC.tsteps)
gsim <-    osim # osim/gsim total oil/gas production (bbl or MCF)
posim <-   osim # total oil production (bbl) from prior wells
pgsim <-   osim # total gas production (MCF) from prior wells
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
nfCO2 <-   matrix(0, nrow = opt$nrun, ncol = 8) # New well emission source fractions for CO2
nfCH4 <-   nfCO2                                # New well emission source fractions for CH4
nfVOC <-   nfCO2                                # New well emission source fractions for VOC
pfCO2 <-   nfCO2                                # Prior well emission source fractions for CO2
pfCH4 <-   nfCO2                                # Prior well emission source fractions for CH4
pfVOC <-   nfCO2                                # Prior well emission source fractions for VOC
rnfCO2 <-  nfCO2                                # Reduced new well emission source fractions for CO2
rnfCH4 <-  nfCO2                                # Reduced new well emission source fractions for CH4
rnfVOC <-  nfCO2                                # Reduced new well emission source fractions for VOC
rpfCO2 <-  nfCO2                                # Reduced prior well emission source fractions for CO2
rpfCH4 <-  nfCO2                                # Reduced prior well emission source fractions for CH4
rpfVOC <-  nfCO2                                # Reduced prior well emission source fractions for VOC
foil <-    matrix(0, nrow = opt$nrun, ncol = 2) # Fraction of base emissions from oil wells
NSPSred <- matrix(0, nrow = opt$nrun, ncol = 4) # Fraction of NSPS reductions
fnvp <-    matrix(0, nrow = opt$nrun, ncol = 3) # Fraction of emissions from new wells vs. prior wells
rfnvp <-   fnvp                                 # Reduced fraction of emissions from new wells vs. prior wells

# Progress Bar (since this next for-loop takes a while)
pb <- txtProgressBar(min = 0, max = opt$nrun, width = 50, style = 3)

# For each runID
for (i in 1:opt$nrun) {
  
  # 3.3.1 Well data simulation -------------------------------------------
  
  # Get time step that each well is drilled and flag as being a new well (i.e.
  # prior = FALSE)
  wsim <- data.frame(tDrill = sim_tdrill(Drilled = Drilled[i,]),
                     prior =  FALSE,
                     tend =   0)
  
  # Get field numbers
  wsim$fieldnum <- sim_fieldnum(cdf.ff = cdf.ff,
                                times =  length(wsim$tDrill))
  
  # Get well types
  wsim$wellType <- sim_wellType(fieldnum = wsim$fieldnum,
                                prob =     prob)
  
  # Get county
  wsim$county <- sim_county(fieldnum = wsim$fieldnum,
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
  
  # Pick well rework time steps for existing wells with fits
  wpri <- cbind(prior.Info, rework = sim_rework(type =       "prior",
                                                wellType =   prior.Info$wellType,
                                                cdf.rework = cdf.rework,
                                                timesteps =  opt$MC.tsteps,
                                                firstprod =  ppri$firstprod,
                                                tstart =     opt$tstart))
  
  # Pick well rework time steps for existing wells w/o fits
  pi.skip$rework <- sim_rework(type =       "prior",
                               wellType =   pi.skip$wellType,
                               cdf.rework = cdf.rework,
                               timesteps =  opt$MC.tsteps,
                               firstprod =  pi.skip$firstprod,
                               tstart =     opt$tstart)
  
  # Combine wsim and pi.skip data.frames (note - assuming zero time delay for
  # prior wells)
  wsim <- rbind(wsim, data.frame(tDrill =   0,
                                 prior =    TRUE,
                                 tend =     pi.skip$tend,
                                 fieldnum = pi.skip$fieldnum,
                                 wellType = pi.skip$wellType,
                                 td.oil =   0,
                                 td.gas =   0,
                                 depth =    pi.skip$depth,
                                 lease =    pi.skip$lease,
                                 county =   pi.skip$county,
                                 rework =   pi.skip$rework))
  
  # Duplicate reworked wells. Reworks from wpri are tracked through wsim
  wsim <- sim_dupRework(wsim =    wsim,
                        wpri =    wpri)
  
  # Pick decline curve coefficients
  wsim <- cbind(wsim, sim_DCC(decline.type.oil =   opt$mc.DCCpick.type.oil,
                              decline.type.gas =   opt$mc.DCCpick.type.gas,
                              times =              nrow(wsim),
                              field =              field,
                              fieldnum =           wsim$fieldnum,
                              DCA.cdf.coef.oil =   DCA.cdf.coef.oil,
                              DCA.cdf.coef.gas =   DCA.cdf.coef.gas,
                              Q.DCA.cdf.coef.oil = Q.DCA.cdf.coef.oil,
                              Q.DCA.cdf.coef.gas = Q.DCA.cdf.coef.gas,
                              tsteps =             opt$tsteps,
                              tDrill =             wsim$tDrill,
                              tend =               wsim$tend,
                              DCAlnormFit =        DCAlnormFit))
  
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
                                rework =        wsim$rework,
                                prior =         wsim$prior))
  
  # Calculate well drilling and completion capital cost for existing wells
  wpri <- cbind(wpri, sim_wcost(type =          "prior",
                                depth =         wpri$depth,
                                drillCost.fit = drillCost.fit,
                                complCR =       complCR,
                                rework =        wpri$rework,
                                prior =         TRUE))
  
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
                        decline.type =    opt$mc.DCeq.type)
  
  # Calculate adjusted prior production volumes by removing production from
  # reworked wells.
  apri <- priorProdReworkAdjust(wpri =      wpri,
                                timesteps = opt$MC.tsteps,
                                ppri =      ppri)
  
  
  # 3.3.9 Lease costs --------------------------------------------------
  
  # Calculate the capital cost for lease equipment
  wsim$LEC <- LECcalc(wsim =          wsim[,c("prior","rework","tDrill","wellType","depth")],
                      LOC.oil.equip = LOC.oil.equip,
                      LOC.gas.equip = LOC.gas.equip,
                      op =            op[i,],
                      gp =            gp[i,],
                      osim =          psim$osim,
                      gsim =          psim$gsim)
  
  # Calculate the operating cost for lease equipment
  LOC <- LOCcalc(wsim =       rbind(wsim[,c("wellType", "depth")],
                                    wpri[,c("wellType", "depth")]),
                 LOC.oil.op = LOC.oil.op,
                 LOC.gas.op = LOC.gas.op,
                 op =         op[i,],
                 gp =         gp[i,],
                 osim =       rbind(psim$osim, apri$oil),
                 gsim =       rbind(psim$gsim, apri$gas))
  
  
  # 3.3.X Production correction -----------------------------------------
  
  OSIM <- rbind(psim$osim, apri$oil)
  GSIM <- rbind(psim$gsim, apri$gas)
  
  # No well should produce if its LOC is higher than some fraction of its gross 
  # revenue. Calculate gross revenue from oil and gas sales.
  gr <- op[i,]*rbind(psim$osim, apri$oil)+gp[i,]*rbind(psim$gsim, apri$gas)
  
  # Get indices of wells (rows) and time steps (columns) with LOC/gr >= cutoff
  ind <- which(LOC/gr >= opt$grFrac)
  
  # Zero out production records and LOCs for wells in ind
  LOC[ind] <- 0
  OSIM[ind] <- 0
  GSIM[ind] <- 0
  
  # Pullout production records from OSIM/GSIM
  psim$osim <- OSIM[1:nrow(psim$osim),]
  apri$oil <-  OSIM[(nrow(psim$osim)+1):nrow(OSIM),]
  psim$gsim <- GSIM[1:nrow(psim$gsim),]
  apri$gas <-  GSIM[(nrow(psim$gsim)+1):nrow(GSIM),]
  
  
  # 3.3.3 Royalties -----------------------------------------------------
  
  # Calculate royalty for oil production
  t.roy.oil <- royalty(royaltyRate = opt$royaltyRate,
                       ep =          op[i,],
                       lease =       c(wsim$lease, wpri$lease),
                       psim =        rbind(psim$osim, apri$oil))
  
  # Calculate royalty for gas production
  t.roy.gas <- royalty(royaltyRate = opt$royaltyRate,
                       ep =          gp[i,],
                       lease =       c(wsim$lease, wpri$lease),
                       psim =        rbind(psim$gsim, apri$gas))
  
  
  # 3.3.4 Severance Taxes ----------------------------------------------
    
  # Calculate ST for oil production
  t.st.oil <- stax(type =    "oil",
                   tDrill =  c(wsim$tDrill, wpri$tDrill),
                   psim =    rbind(psim$osim, apri$oil),
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
                   psim =    rbind(psim$gsim, apri$gas),
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
               osim =     rbind(psim$osim, apri$oil),
               gsim =     rbind(psim$gsim, apri$gas),
               pTaxfrac = c(wsim$pTaxfrac, wpri$pTaxfrac))
  
  
  # 3.3.6 Corporate income taxes ---------------------------------------
  
  # Run corporate income tax calculation
  CT <- ctax(OP =           op[i,],
             GP =           gp[i,],
             osim =         rbind(psim$osim, apri$oil),
             gsim =         rbind(psim$gsim, apri$gas),
             NTIfrac =      c(wsim$NTIfrac, wpri$NTIfrac),
             CIrate.state = opt$CIrate.state,
             CIrate.fed =   opt$CIrate.fed)
  
  
  # 3.3.7 Emissions ----------------------------------------------------
  
  # Calculate emissions from new wells
  ETsim <- Ecalc(osim =   psim$osim,
                 gsim =   psim$gsim,
                 wsim =   wsim,
                 tstart = opt$tstart,
                 EFred =  opt$EFred)
  
  # Calculate emissions from existing wells
  ETpri <- Ecalc(osim =   apri$oil,
                 gsim =   apri$gas,
                 wsim =   wpri,
                 tstart = opt$tstart,
                 EFred =  opt$EFred)
  
  
  # 3.3.8 Water Balance ------------------------------------------------
  
  # Calculate water balance
  WB <- water(wsim =     rbind(wsim[,c("tDrill", "pw","disp","evap","frack","inj")],
                               wpri[,c("tDrill", "pw","disp","evap","frack","inj")]),
              osim =     rbind(psim$osim, apri$oil),
              gsim =     rbind(psim$gsim, apri$gas),
              wellType = c(wsim$wellType, wpri$wellType),
              depth =    c(wsim$depth, wpri$depth),
              dw.lm =    water.lm$dw.lm)
  
  
  # 3.3.x Get totals for MC run i --------------------------------------
  
  # Calculate column sums for each results matrix generated to get totals
  osim[i, ] <-    colSums(psim$osim[which(wsim$prior == F),])
  gsim[i, ] <-    colSums(psim$gsim[which(wsim$prior == F),])
  posim[i, ] <-   colSums(apri$oil)+colSums(psim$osim[which(wsim$prior == T),])
  pgsim[i, ] <-   colSums(apri$gas)+colSums(psim$gsim[which(wsim$prior == T),])
  roy.oil[i, ] <- colSums(t.roy.oil)
  roy.gas[i, ] <- colSums(t.roy.gas)
  st.oil[i, ] <-  colSums(t.st.oil)
  st.gas[i, ] <-  colSums(t.st.gas)
  PT[i, ] <-      colSums(t.PT)
  CTstate[i, ] <- colSums(CT$state)
  CTfed[i, ] <-   colSums(CT$fed)
  CO2[i, ] <-     colSums(rbind(ETsim$CO2, ETpri$CO2))
  CH4[i, ] <-     colSums(rbind(ETsim$CH4, ETpri$CH4))
  VOC[i, ] <-     colSums(rbind(ETsim$VOC, ETpri$VOC))
  rCO2[i, ] <-    colSums(rbind(ETsim$rCO2, ETpri$rCO2))
  rCH4[i, ] <-    colSums(rbind(ETsim$rCH4, ETpri$rCH4))
  rVOC[i, ] <-    colSums(rbind(ETsim$rVOC, ETpri$rVOC))
  nfCO2[i, ] <-   ETsim$fET$co2
  nfCH4[i, ] <-   ETsim$fET$ch4
  nfVOC[i, ] <-   ETsim$fET$voc
  pfCO2[i, ] <-   ETpri$fET$co2
  pfCH4[i, ] <-   ETpri$fET$ch4
  pfVOC[i, ] <-   ETpri$fET$voc
  rnfCO2[i, ] <-  ETsim$rfET$co2
  rnfCH4[i, ] <-  ETsim$rfET$ch4
  rnfVOC[i, ] <-  ETsim$rfET$voc
  rpfCO2[i, ] <-  ETpri$rfET$co2
  rpfCH4[i, ] <-  ETpri$rfET$ch4
  rpfVOC[i, ] <-  ETpri$rfET$voc
  foil[i, ] <-    c(ETsim$foil, ETpri$foil)
  fnvp[i, ] <-    c(sum(ETsim$CO2)/sum(CO2[i, ]),   sum(ETsim$CH4)/sum(CH4[i, ]),   sum(ETsim$VOC)/sum(VOC[i, ]))
  rfnvp[i, ] <-   c(sum(ETsim$rCO2)/sum(rCO2[i, ]), sum(ETsim$rCH4)/sum(rCH4[i, ]), sum(ETsim$rVOC)/sum(rVOC[i, ]))
  NSPSred[i, ] <- c(ETsim$NSPSred, ETpri$NSPSred)
  w.pw[i, ] <-    WB$pw
  w.disp[i, ] <-  WB$disp
  w.evap[i, ] <-  WB$evap
  w.rec[i, ] <-   WB$recycle
  w.dw[i, ] <-    WB$dw
  w.fw[i, ] <-    WB$fw
  w.inj[i, ] <-   WB$inj
  w.in[i, ] <-    WB$wtr.in
  w.r[i, ] <-     WB$wtr.r
  
  # Update progress bar
  Sys.sleep(1e-3)
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)


# # 3.7 Employment ----------------------------------------------------------
# 
# # RIMS II model employment estimate
# jobs.RIMS <- RIMS(multiplier = opt$RIMSmultiplier,
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

# Print stop time for MC simulation section
writeLines(c("",
             paste("Finished Monte-Carlo simulation at:", Sys.time()),
             paste("Elapsed time:", format(difftime(Sys.time(), runstart)))))

# Print finished message and play sound - feel free to replace with your
# preferred sound, see help for function by typing "?beep" in console
beep(4)
writeLines(c("",
             "Model run complete"))

