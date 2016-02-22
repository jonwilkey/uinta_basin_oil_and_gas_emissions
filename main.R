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
path$raw  <-    paste(pwd.drop, "Dropbox/UBOG/Raw Data", sep = "")
path$data <-    paste(pwd.drop, "Dropbox/UBOG/Prepared Data", sep = "")
path$look <-    paste(pwd.drop, "Dropbox/UBOG/Lookup Tables", sep = "")
path$plot <-    paste(pwd.drop, "Dropbox/UBOG/Plots", sep = "")
path$work <-    paste(pwd.git,  "ub_o-g_emissions", sep = "")
path$fun  <-    paste(pwd.git,  "ub_o-g_emissions/Functions", sep = "")
path$plotfun <- paste(pwd.git,  "ub_o-g_emissions/Functions/Plotting", sep = "")

# Remove temporary path objects
remove(pwd.drop, pwd.git)

# Set working directory
setwd(path$work)


# 1.2 Functions -----------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(path$fun, c("GBMsim.R",
                              "EIAsim.R",
                              "EIAsimLT.R",
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
                              "sim_tdrill.R",
                              "sim_wellType.R",
                              "productionsim.R",
                              "priorProdReworkAdjust.R",
                              "Ecalc.R",
                              "LOCcalc.R",
                              "sim_E_wc.R",
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

# Run scripts to load user defined input/output options
source("IO_options.R")
source("EF_options.R")

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


# 2.5 Drilling Schedule Model lm() fit update -----------------------------

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


# 2.6 GBM parameter fit update --------------------------------------------

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


# 2.7 EIA Forecast Update -------------------------------------------------

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


# 2.8 EIA Error Analysis Update ------------------------------------------

# Run function if opt$EIAerror.update flag is set to "TRUE"
if(opt$EIAerror.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "EIAerrorUpdate.R"))
  source(file.path(path$fun, "EIAerrorUpdateFrac.R"))
  source(file.path(path$fun, "EIAerrorUpdateLT.R"))
  
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
  
  # # Function call - Long Term projections
  # EIAerrorUpdateLT(op.FC.high = op.FC.high,
  #                    op.FC.ref =  op.FC.ref,
  #                    op.FC.low =  op.FC.low,
  #                    gp.FC.high = gp.FC.high,
  #                    gp.FC.ref =  gp.FC.ref,
  #                    gp.FC.low =  gp.FC.low,
  #                    path =       path,
  #                    xq =         opt$xq,
  #                    ver =        opt$file_ver)
}

# Load EIA error CDF matrices (Eoil & Egas and EoilFrac & EgasFrac)
load(file.path(path$data, paste("EIAerror_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("EIAerrorFrac_", opt$file_ver, ".rda", sep = "")))
# load(file.path(path$data, paste("EIAerrorLT_", opt$file_ver, ".rda", sep = "")))


# 2.9 Decline curve analysis update --------------------------------------

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


# 2.10 DCA CDF Update ------------------------------------------------------

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


# 2.11 Rework CDF analysis and update -------------------------------------

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


# 2.12 DCA Coefficient Distribution Fitting -------------------------------

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


# 2.13 UDAQ Emissions Inventory Data --------------------------------------

# Run function if opt$rework.update flag is set to "TRUE"
if(opt$emission.update == TRUE) {
  
  # Load function
  source(file.path(path$fun, "emissionUpdate.R"))
  
  # Function call
  emissionUpdate(path =    path,
                 ver =     opt$file_ver,
                 wc.ctrl = eopt$wc.ctrl)
}

# Load emissions inventory data.frame (OGEI)
# - wc.fuel.CDF: CDF for fuel usage during well completion
# - wc.ctrl.prob: Probability and efficiency of well completion flaring
load(file.path(path$data, paste("emissionUpdate_", opt$file_ver, ".rda", sep = "")))


# Transition to MC simulation ---------------------------------------------

# Print stop time for data analysis section
writeLines(c("",
             paste("Finished data analysis at:", Sys.time()),
             paste("Elapsed time:", format(difftime(Sys.time(), runstart)))))

# If loading results of previous analysis
if (opt$load.prior == TRUE) {
  
  # Load prior results
  load(file.path(path$data, opt$load.name))
  
} else {
  
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
         },
         
         # If using long-term EIA based forecasts, ep.type = "d"
         d = {
           
           # Run EIAsim
           epsim <- EIAsimLT(nrun =    opt$nrun,
                             Eoil.LT = Eoil.LT,
                             Egas.LT = Egas.LT)
           
           # Extract objects from list
           op <- epsim$op
           gp <- epsim$gp
           
           # Remove list
           remove(epsim)
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
  CO2 <-     osim # CO2 emission totals (metric tons)
  CH4 <-     osim # CH4 emission totals (metric tons)
  VOC <-     osim # VOC emission totals (metric tons)
  rCO2 <-    osim # Reduced CO2 emission totals (metric tons)
  rCH4 <-    osim # Reduced CH4 emission totals (metric tons)
  rVOC <-    osim # Reduced VOC emission totals (metric tons)
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
    
    # Pick emission factors
    wsim <- cbind(wsim, sim_EF(times = nrow(wsim), EF = opt$EF))
    wpri <- cbind(wpri, sim_EF(times = nrow(wpri), EF = opt$EF))
    
    # Pick well completion emissions
    wsim <- cbind(wsim, sim_E_wc(wc.fuel.CDF =  wc.fuel.CDF,
                                 wc.ctrl.prob = wc.ctrl.prob,
                                 wc.EF =        eopt$wc.EF,
                                 times =        nrow(wsim)))
    
    wpri <- cbind(wpri, sim_E_wc(wc.fuel.CDF =  wc.fuel.CDF,
                                 wc.ctrl.prob = wc.ctrl.prob,
                                 wc.EF =        eopt$wc.EF,
                                 times =        nrow(wpri)))
    
    
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
    
    
    # 3.3.3 Lease operating costs -----------------------------------------
    
    # Calculate the operating cost for lease equipment
    LOC <- LOCcalc(wsim =       rbind(wsim[,c("wellType", "depth")],
                                      wpri[,c("wellType", "depth")]),
                   LOC.oil.op = LOC.oil.op,
                   LOC.gas.op = LOC.gas.op,
                   op =         op[i,],
                   gp =         gp[i,],
                   osim =       rbind(psim$osim, apri$oil),
                   gsim =       rbind(psim$gsim, apri$gas))
    
    
    # 3.3.4 Production correction -----------------------------------------
    
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
    
    
    # 3.3.5 Emissions ----------------------------------------------------
    
    # Calculate emissions from new wells
    ETsim <- Ecalc(osim =      psim$osim,
                   gsim =      psim$gsim,
                   wsim =      wsim,
                   tstart =    opt$tstart,
                   EFred =     opt$EFred,
                   MC.tsteps = opt$MC.tsteps)
    
    # Calculate emissions from existing wells
    ETpri <- Ecalc(osim =      apri$oil,
                   gsim =      apri$gas,
                   wsim =      wpri,
                   tstart =    opt$tstart,
                   EFred =     opt$EFred,
                   MC.tsteps = opt$MC.tsteps)
    
    
    # 3.3.6 Get totals for MC run i --------------------------------------
    
    # Calculate column sums for each results matrix generated to get totals
    osim[i, ] <-    colSums(psim$osim[which(wsim$prior == F),])
    gsim[i, ] <-    colSums(psim$gsim[which(wsim$prior == F),])
    posim[i, ] <-   colSums(apri$oil)+colSums(psim$osim[which(wsim$prior == T),])
    pgsim[i, ] <-   colSums(apri$gas)+colSums(psim$gsim[which(wsim$prior == T),])
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
    
    # Update progress bar
    Sys.sleep(1e-3)
    setTxtProgressBar(pb, i)
  }
  
  # Close progress bar
  close(pb)
  
  # If specified, save results
  if (opt$save == T) {
    
    # Save results
    save(file = file.path(path$data, opt$save.name),
         list = c("osim",
                  "gsim",
                  "posim",
                  "pgsim",
                  "CO2",
                  "CH4",
                  "VOC",
                  "rCO2",
                  "rCH4",
                  "rVOC",
                  "nfCO2",
                  "nfCH4",
                  "nfVOC",
                  "pfCO2",
                  "pfCH4",
                  "pfVOC",
                  "rnfCO2",
                  "rnfCH4",
                  "rnfVOC",
                  "rpfCO2",
                  "rpfCH4",
                  "rpfVOC",
                  "foil",
                  "fnvp",
                  "rfnvp",
                  "NSPSred",
                  "op",
                  "gp",
                  "Drilled",
                  "ppri",
                  "prior.Info",
                  "pi.skip"))
  }
  
  # Print stop time for MC simulation section
  writeLines(c("",
               paste("Finished Monte-Carlo simulation at:", Sys.time()),
               paste("Elapsed time:", format(difftime(Sys.time(), runstart)))))
  
  # Print finished message and play sound - feel free to replace with your
  # preferred sound, see help for function by typing "?beep" in console
  beep(3)
  writeLines(c("",
               "Model run complete"))
}


# 4.1 Post processing -----------------------------------------------------

# Run processing script to generate plots of results
source("postProcess.R")
