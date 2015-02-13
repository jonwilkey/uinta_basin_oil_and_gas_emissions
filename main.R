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
                              "drillsim.R",
                              "welldata.R",
                              "productionsim.R",
                              "royalty.R",
                              "stax.R",
                              "ctax.R",
                              "ptax.R",
                              "Ecalc.R",
                              #"RIMS.R",
                              #"workload.R",
                              #"water.R",
                              "postProcess.R",
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
                 field =           opt$field,
                 tsteps =          opt$tsteps,
                 min.depth =       opt$min.well.depth,
                 max.depth =       opt$max.well.depth,
                 well.depth.step = opt$well.depth.step,
                 ver =             opt$file_ver)
}

# Load data.frames from sheduleUpdate function:
# - osim/gsim.actual: actual oil/gas production
# - wsim.actual:      actual well data (formatted for simulation)
# - well.actual:      actual well data information
# - cdf.depth.ow/gw:  CDFs for well depth based on well type
# - cdf.ff:           CDFs for probability of a well being located in a given field
# - cdf.flt:          CDFs for surface lease types (federal, state, etc.) by field
# - prob:             Probability that a well located in a given field is dry well or gas well
load(file.path(path$data, paste("wsim_actual_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("psim_actual_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("well_actual_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("cdf_schedule_", opt$file_ver, ".rda", sep = "")))


# 2.4 Lease opearting cost lm() fit update --------------------------------

# Run function if opt$lease.update flag is set to "TRUE"
if(opt$lease.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "leaseOpCostUpdate.R"))
  
  # Function call
  leaseOpCostUpdate(path =   path,
                    ver =    opt$file_ver,
                    tstart = opt$tstart,
                    tstop =  opt$tstop,
                    full =   opt$fullDataFit)
}


# 2.x EIA energy price history --------------------------------------------

# Run function if opt$EIAprice.update is set to "TRUE"
if(opt$EIAprice.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "EIApriceUpdate.R"))
  
  # Function call
  EIApriceUpdate(path =            path,
                 EP.CPI.basis =    opt$EP.CPI.basis,
                 cpi =             opt$cpi,
                 ver =             opt$file_ver,
                 cf.MCF.to.MMBtu = opt$cf.MCF.to.MMBtu)
}

# Load EIAprices_v*.rda to load eia.hp (EIA historical energy prices) data.frame
load(file.path(path$data, paste("EIAprices_", opt$file_ver, ".rda", sep = "")))


# 2.3 Corporate income tax conversion factor CDF generation ---------------

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


# 2.x Property tax update -------------------------------------------------

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


# 2.5 Drilling Schedule Model lm() fit update -----------------------------

# Run function if opt$drillmodel.update flag is set to "TRUE"
if(opt$drillmodel.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "drillingModelUpdate.R"))
  
  # Function call
  drillingModelUpdate(path =      path,
                      p =         p,
                      min.depth = opt$min.well.depth,
                      ver =       opt$file_ver,
                      eia.hp =    eia.hp)
}

# Load economic drilling model fit (drillModel) and data (drillModelData)
load(file.path(path$data, paste("drillModel_", opt$file_ver, ".rda", sep = "")))


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

# Load GBM parameter fits (GBMfitOP and GBMfitGP)
load(file.path(path$data, paste("GBMfit_", opt$file_ver, ".rda", sep = "")))


# 2.7 Decline curve analysis update ---------------------------------------

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
            field =           opt$field,
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
            Qupper.gas =      opt$Qupper.gas)
}

# Load DCA fits mo (oil) and mg (gas)
load(file.path(path$data, paste("DCA_fits_", opt$file_ver, ".rda", sep = "")))


# 2.8 DCA CDF Update ------------------------------------------------------

# Run function if opt$DCA.CDF.update flag is set to "TRUE"
if(opt$DCA.CDF.update == TRUE) {
  
  # Source functions to load
  source(file.path(path$fun, "DCAupdateCDF.R"))
  source(file.path(path$fun, "QfitDCAupdateCDF.R"))
  
  # Function call for hyperbolic DCA CDFs
  DCAupdateCDF(field =        opt$field,
               ver =          opt$file_ver,
               DCA.CDF.type = opt$DCA.CDF.type,
               cdf.oil.from = opt$cdf.oil.from,
               cdf.oil.to =   opt$cdf.oil.to,
               cdf.oil.np =   opt$cdf.oil.np,
               cdf.gas.from = opt$cdf.gas.from,
               cdf.gas.to =   opt$cdf.gas.to,
               cdf.gas.np =   opt$cdf.gas.np,
               DCA.CDF.xq =   opt$DCA.CDF.xq,
               path =         path,
               tstart =       opt$DCA.CDF.tstart,
               tstop =        opt$DCA.CDF.tstop,
               mo =           mo,
               mg =           mg)
  
  # Function call for cumulative DCA CDFs
  QfitDCAupdateCDF(field =          opt$field,
                   ver =            opt$file_ver,
                   DCA.CDF.type   = opt$DCA.CDF.type,
                   Q.cdf.oil.from = opt$Q.cdf.oil.from,
                   Q.cdf.oil.to =   opt$Q.cdf.oil.to,
                   Q.cdf.oil.np =   opt$Q.cdf.oil.np,
                   Q.cdf.gas.from = opt$Q.cdf.gas.from,
                   Q.cdf.gas.to =   opt$Q.cdf.gas.to,
                   Q.cdf.gas.np =   opt$Q.cdf.gas.np,
                   DCA.CDF.xq =     opt$DCA.CDF.xq,
                   path =           path,
                   tstart =         opt$DCA.CDF.tstart,
                   tstop =          opt$DCA.CDF.tstop,
                   mo =             mo,
                   mg =             mg)
}

# Load CDFs for DCA fits
load(file.path(path$data, paste("DCA_CDF_coef_", opt$file_ver, ".rda", sep = "")))
load(file.path(path$data, paste("Q_DCA_CDF_coef_", opt$file_ver, ".rda", sep = "")))


# 2.8 Drilling and Completion Capital Cost Model Update -------------------

# Run function if opt$drillCapCost.update flag is set to "TRUE"
if(opt$drillCapCost.update == TRUE) {
  
  # Source function to load
  source(file.path(path$fun, "drillCapCostUpdate.R"))
  
  drillCapCostUpdate(path =  path,
                     basis = opt$cpi,
                     ver =   opt$file_ver)
}

# Load drilling cost data (drillCost.data) and fit (drillCost.fit)
load(file.path(path$data, paste("drillCost_", opt$file_ver, ".rda", sep = "")))


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


# 3.1 Energy price path simulation ----------------------------------------

# Run Geometric Brownian Motion (GBM) price path simulation
epsim <- GBMsim(path =         path,
                oil.fpp.init = opt$oil.fpp.init,
                gas.fpp.init = opt$gas.fpp.init,
                timesteps =    opt$MC.tsteps,
                nrun =         opt$nrun,
                GBMfitOP =     GBMfitOP,
                GBMfitGP =     GBMfitGP)

# Extract individual data.frames from list object
op <- epsim[[1]]
gp <- epsim[[2]]

# Remove list
remove(epsim)


# 3.2 Well drilling simulation --------------------------------------------

# Run well drilling simulation given price paths opsim and gpsim
Drilled <- drillsim(path =         path,
                    GBMsim.OP =    op,
                    GBMsim.GP =    gp,
                    nrun =         opt$nrun,
                    drilled.init = opt$drilled.init,
                    drillModel =   drillModel)


# 3.3 Monte-Carlo Loop ----------------------------------------------------

# The following for-loop calculates all terms for each well in a single 
# Monte-Carlo (MC) iteration and returns the results as a total to cut down on
# the memory usage for all the objects in the for-loop

# Preallocate results matrices
osim <-    matrix(0, nrow = opt$nrun, ncol = opt$MC.tsteps)
gsim <-    osim
roy.oil <- osim
roy.gas <- osim
st.oil <-  osim
st.gas <-  osim
PT <-      osim
CTstate <- osim
CTfed <-   osim
CO2 <-     osim
CH4 <-     osim
VOC <-     osim

# Progress Bar (since this next for-loop takes a while)
pb <- txtProgressBar(min = 0, max = opt$nrun, width = 50, style = 3)

# For each runID
for (i in 1:opt$nrun) {
  
  # 3.3.1 Well data simulation -------------------------------------------
  
  # Run well data simulation function
  wsim <- welldata(path =               path,
                   sched.type =         opt$sched.type,
                   Drilled =            Drilled[i,],
                   timesteps =          opt$MC.tsteps,
                   nrun =               1,
                   field =              opt$field,
                   ver =                opt$file_ver,
                   production.type =    opt$prod.type,
                   basis =              opt$cpi,
                   decline.type =       opt$mc.decline.type,
                   EF =                 opt$EF,
                   cdf.ff =             cdf.ff,
                   cdf.flt =            cdf.flt,
                   prob =               prob,
                   cdf.depth.ow =       cdf.depth.ow,
                   cdf.depth.gw =       cdf.depth.gw,
                   wsim.actual =        wsim.actual,
                   DCA.cdf.coef.oil =   DCA.cdf.coef.oil,
                   DCA.cdf.coef.gas =   DCA.cdf.coef.gas,
                   Q.DCA.cdf.coef.oil = Q.DCA.cdf.coef.oil,
                   Q.DCA.cdf.coef.gas = Q.DCA.cdf.coef.gas,
                   corpNTIfrac =        corpNTIfrac,
                   pTaxRate =           pTaxRate)
  
  
  # 3.3.2 Production simulation ------------------------------------------
  
  # Run production simulation function
  psim <- productionsim(wsim =            wsim,
                        timesteps =       opt$MC.tsteps,
                        production.type = opt$prod.type,
                        decline.type =    opt$mc.decline.type,
                        osim.actual =     osim.actual,
                        gsim.actual =     gsim.actual)
  
  # Pullout list components osim (oil production records) and gsim (gas
  # production records)
  t.osim <- psim[[1]]
  t.gsim <- psim[[2]]
  
  # Remove original
  remove(psim)
  
  
  # 3.3.3 Royalties -----------------------------------------------------
  
  # Calculate royalty for oil production
  t.roy.oil <- royalty(royaltyRate = opt$royaltyRate,
                       ep =          op[i,],
                       lease =       wsim$lease,
                       psim =        t.osim)
  
  # Calculate royalty for gas production
  t.roy.gas <- royalty(royaltyRate = opt$royaltyRate,
                       ep =          gp[i,],
                       lease =       wsim$lease,
                       psim =        t.gsim)
  
  
  # 3.3.4 Severance Taxes ----------------------------------------------
    
  # Calculate ST for oil production
  t.st.oil <- stax(type =    "oil",
                   tDrill =  wsim$tDrill,
                   psim =    t.osim,
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
                   tDrill =  wsim$tDrill,
                   psim =    t.gsim,
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
  t.PT <- ptax(OP = op[i,],
               GP = gp[i,],
               osim = t.osim,
               gsim = t.gsim,
               pTaxfrac = wsim$pTaxfrac)
  
  
  # 3.3.6 Corporate income taxes ---------------------------------------
  
  # Run corporate income tax calculation
  CT <- ctax(OP =           op[i,],
             GP =           gp[i,],
             osim =         t.osim,
             gsim =         t.gsim,
             NTIfrac =      wsim$NTIfrac,
             CIrate.state = opt$CIrate.state,
             CIrate.fed =   opt$CIrate.fed)
  
  # Split out individual matrices from list
  t.CTstate <- CT[[1]] # Corporate state income taxes
  t.CTfed <-   CT[[2]] # Corporate federal income taxes
  
  # Remove list
  remove(CT)
  
  
  # 3.3.7 Emissions ----------------------------------------------------
  
  # Calculate emissions
  ET <- Ecalc(osim = t.osim,
              gsim = t.gsim,
              wsim = wsim)
  
  # Extract component matrices from ET
  t.CO2 <- ET[[1]]
  t.CH4 <- ET[[2]]
  t.VOC <- ET[[3]]
  
  # Remove list
  remove(ET)
  
  
  # 3.3.x Get totals for MC run i ---------------------------------------
  
  # Calculate column sums for each results matrix generated to get totals
  osim[i,] <-    colSums(t.osim)
  gsim[i,] <-    colSums(t.gsim)
  roy.oil[i,] <- colSums(t.roy.oil)
  roy.gas[i,] <- colSums(t.roy.gas)
  st.oil[i,] <-  colSums(t.st.oil)
  st.gas[i,] <-  colSums(t.st.gas)
  PT[i,] <-      colSums(t.PT)
  CTstate[i,] <- colSums(t.CTstate)
  CTfed[i,] <-   colSums(t.CTfed)
  CO2[i,] <-     colSums(t.CO2)
  CH4[i,] <-     colSums(t.CH4)
  VOC[i,] <-     colSums(t.VOC)
  
  
  # 3.4.x Cleanup -------------------------------------------------------
  
  # Remove temporary matrices
  remove(t.osim,
         t.gsim,
         t.roy.oil,
         t.roy.gas,
         t.st.oil,
         t.st.gas,
         t.PT,
         t.CTstate,
         t.CTfed,
         t.CO2,
         t.CH4,
         t.VOC)
  
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
# # 3.9 Water Balance -------------------------------------------------------
# 
# # Determine water balance
# water.balance <- water(wsim = wsim,
#                        psim = psim,
#                        data_root = data_root)


# 3.10 Energy Balance -----------------------------------------------------

# blah


# 4.1 Post processing -----------------------------------------------------

# Call post processing function to generate plots of results
postProcess(quant <-              opt$quant,
            tstart <-             opt$tstart,
            tstop <-              opt$tstop,
            tsteps <-             opt$tsteps,
            eia.hp <-             eia.hp,
            wsim.actual <-        wsim.actual,
            osim.actual <-        osim.actual,
            gsim.actual <-        gsim.actual,
            path <-               path,
            export <-             opt$exportFlag,
            plist <-              opt$plist,
            prefix <-             opt$prefix,
            affix <-              opt$affix,
            osim <-               osim,
            gsim <-               gsim,
            field <-              opt$field,
            CO2 <-                CO2,
            CH4 <-                CH4,
            VOC <-                VOC,
            op <-                 op,
            gp <-                 gp,
            drillModel <-         drillModel,
            drillModelData <-     drillModelData,
            mo <-                 mo,
            mg <-                 mg,
            DCA.cdf.coef.gas <-   DCA.cdf.coef.gas,
            DCA.cdf.coef.oil <-   DCA.cdf.coef.oil,
            Q.DCA.cdf.coef.gas <- Q.DCA.cdf.coef.gas,
            Q.DCA.cdf.coef.oil <- Q.DCA.cdf.coef.oil,
            cdf.ff <-             cdf.ff,
            well.actual <-        well.actual,
            cpiDate <-            opt$cpiDate,
            drillCost.data <-     drillCost.data,
            drillCost.fit <-      drillCost.fit)

