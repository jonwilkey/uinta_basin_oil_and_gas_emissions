# Function Info -----------------------------------------------------------
# Name:      eqEcalc.R (Equipment-based emissions calculation function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# eci - list containing cumulative probability tables (CPT) for each type of
# equipment

# wsim - using the eqEF.* columns, which indicate which row of each CPT to 
# cross-reference when performing emissions calculations

# eopt - emissions input options list (for emission factors, % reductions, etc.)

# MC.tsteps - number of time steps in simulation


# Outputs -----------------------------------------------------------------

# E - emissions results list, containing one matrix for each type of pollutant
# (PM10, SOx, NOx, etc.) with rows = wells and columns = simulation time steps


# Description -------------------------------------------------------------

# This function calculates emissions for each well using equipment-based
# emission factors and calculation procedures.


# Function ----------------------------------------------------------------

eqEcalc <- function(wsim, MC.tsteps, osim, gsim, eci, eopt) {
  
  # 0.0 Setup ---------------------------------------------------------------
  
  # Create production identity matrix. Since none of the equipment-based emission 
  # factors depend on the amount of oil/gas an individual well is producing, 
  # ongoing emissions from each well are either "on" or "off". The identity matrix
  # is 1 where a well is producing (either oil or gas) and 0 otherwise.
  Eid <- ifelse(test = osim > 0 | gsim > 0, yes = 1, no = 0)
  
  
  # 1.0 Well Completions (New Wells and Reworks) ----------------------------
  
  # Well completions are one-time emissions events. Calculation procedure is to 
  # reference fuel usage from eci$wc CPT and apply emission factors from eopt. 
  # Calculation is handled by calc_E_wc function, which returns PM10, PM25, NOx,
  # VOC, and CO emissions. These emissions for each well need to be matched with
  # their corresponding tDrill time from wsim to pick the correct column of each
  # matrix in E for assigning emissions. It is assumed that there is no
  # difference in the well completion emissions between new wells and reworked
  # wells.
  
  # Create matrix with 1 where a well is completed and 0 otherwise
  
  # Predefine 0 matrix
  Ewc <- matrix(0, nrow = nrow(wsim), ncol = MC.tsteps)
  
  # For each time step (i.e. column) in Ewc
  for (i in 1:ncol(Ewc)) {
    
    # Find row numbers of wells completed in time step i
    ind <- which(wsim$tDrill == i)
    
    # Set those wells values to 1
    Ewc[ind, i] <- 1
  }
  
  # Calculation
  calc <- calc_E_wc(fuel  = eci$wc$annual_diesel_usage[wsim$eqEF.wc],
                    red   = eci$wc$pct_control[wsim$eqEF.wc],
                    wc.EF = eopt$wc.EF)
  
  # Assign emissions to results list E
  E <- list(wc = list(pm10 = calc$em.wc.pm10 * Ewc,
                      pm25 = calc$em.wc.pm25 * Ewc,
                      nox  = calc$em.wc.nox  * Ewc,
                      voc  = calc$em.wc.voc  * Ewc,
                      co   = calc$em.wc.co   * Ewc))
  
  
  # 2.0 RICE and Turbines ---------------------------------------------------
  
  # RICE and turbines have ongoing emissions. Reference eci$rt CPT and calculate 
  # emissions using calc_E_rt function. Annual total emissions are converted to a 
  # montly basis and applied to each month that each well is in operation using
  # the Eid matrix.
  
  # Calculation
  calc <- calc_E_rt(rt = eci$rt[wsim$eqEF.rt,])
  
  # Assign emissions
  E$rt$pm10 <- (calc$em.rt.pm10 / 12) * Eid
  E$rt$pm25 <- (calc$em.rt.pm25 / 12) * Eid
  E$rt$sox  <- (calc$em.rt.sox  / 12) * Eid
  E$rt$nox  <- (calc$em.rt.nox  / 12) * Eid
  E$rt$voc  <- (calc$em.rt.voc  / 12) * Eid
  E$rt$co   <- (calc$em.rt.co   / 12) * Eid
  E$rt$ch2o <- (calc$em.rt.ch2o / 12) * Eid
  
  
  # 3.0 Separators and Heaters ----------------------------------------------
  
  # Separators and heaters have ongoing emissions. Reference eci$sh CPT and
  # calculate emissions using calc_E_sh function. Annual total emissions are
  # converted to a montly basis and applied to each month using Eid.
  
  # Calculation - some NA values in input columns (as of 2016-03-30 version of
  # OGEI database), overwrite NAs as 0
  calc <- calc_E_sh(heat.duty  = NA.overwrite(eci$sh$heat_duty[wsim$eqEF.sh]),
                    op         = NA.overwrite(eci$sh$hours_operation[wsim$eqEF.sh]),
                    heat.value = NA.overwrite(eci$sh$fuel_heat[wsim$eqEF.sh]),
                    sh.EF      = eopt$sh.EF,
                    nox.red    = eci$sh$percent_control[wsim$eqEF.sh],
                    wfrac      = NA.overwrite(eci$sh$wfrac[wsim$eqEF.sh]))
  
  # Overwrite remaining NA's as zeros (can be induced by 0/0 division since NA
  # values in heat.value input vector are written as zero, and heat.value is
  # placed in the denominator of calc_E_sh function)
  calc <- NA.overwrite(calc)
  
  # Assign emissions
  E$sh$pm10 <- (calc$em.sh.pm10 / 12) * Eid
  E$sh$pm25 <- (calc$em.sh.pm25 / 12) * Eid
  E$sh$sox  <- (calc$em.sh.sox  / 12) * Eid
  E$sh$nox  <- (calc$em.sh.nox  / 12) * Eid
  E$sh$voc  <- (calc$em.sh.voc  / 12) * Eid
  E$sh$co   <- (calc$em.sh.co   / 12) * Eid
  
  
  # 4.0 Dehydrators ---------------------------------------------------------
  
  # Dehydrators have ongoing emissions. Reference eci$dh CPT and calculate 
  # emissions using calc_E_dh function. Annual total emissions are converted to a 
  # monthly basis and applied to each month using Eid.
  
  # Calculation - some NA values in input columns (as of 2016-03-30 version of
  # OGEI database), overwrite NAs as 0
  calc <- calc_E_dh(voc.factor = eci$dh$factor_voc[wsim$eqEF.dh],
                    op         = eci$dh$hours_operation[wsim$eqEF.dh],
                    vgas.pilot = NA.overwrite(eci$dh$pilot_volume[wsim$eqEF.dh]),
                    dh.EF      = eopt$dh.EF,
                    wfrac      = NA.overwrite(eci$dh$wfrac[wsim$eqEF.dh]),
                    heat.duty  = eci$dh$heat_input_rate[wsim$eqEF.dh])
  
  # Assign emissions
  E$dh$nox <- (calc$em.dh.nox / 12) * Eid
  E$dh$voc <- (calc$em.dh.voc / 12) * Eid
  E$dh$co <-  (calc$em.dh.co  / 12) * Eid
  
  
  # 5.0 Tanks ---------------------------------------------------------------
  
  # Tanks have ongoing emissions. Reference eci$tank CPT and calculate emissions 
  # using calc_E_tank function. Tank VOC emissions are assumed to be constant and
  # are not a function of oil production rates. Annual total emissions are
  # converted to a monthly basis and applied to each month using Eid.
  
  # Calculation
  calc <- calc_E_tank(heat.duty  = eci$tank$combustor_heat_input[wsim$eqEF.tank],
                      vgas.pilot = NA.overwrite(eci$tank$pilot_volume[wsim$eqEF.tank]),
                      tank.EF    = eopt$tank.EF,
                      wfrac      = NA.overwrite(eci$tank$wfrac[wsim$eqEF.tank]),
                      tank.voc   = eci$tank$total_voc[wsim$eqEF.tank])
  
  # Assign emissions
  E$tank$nox <- (calc$em.tank.nox / 12) * Eid
  E$tank$voc <- (calc$em.tank.voc / 12) * Eid
  E$tank$co <-  (calc$em.tank.co  / 12) * Eid
  
  
  # 6.0 Truck Loading -------------------------------------------------------
  
  # Truck loading has ongoing emissions that depend on oil production rates. 
  # Reference eci$truck CPT and calculate emissions using calc_E_truck function, 
  # which returns the calculated emissions factor in ton/bbl. Emissions are then 
  # (emissions factor) * (oil production), where production volumes are given by
  # osim.
  
  # Calculation of emission factor
  calc <- calc_E_truck(S     = eci$truck$s_factor[wsim$eqEF.truck],
                       P.vap = eci$truck$vapor_pressure[wsim$eqEF.truck],
                       MW    = eci$truck$molecular_weight[wsim$eqEF.truck],
                       Temp  = eci$truck$temp_r[wsim$eqEF.truck],
                       red   = eci$truck$control_percent[wsim$eqEF.truck])
  
  # Calculate and assign emissions
  E$truck$voc <- calc * osim
  
  
  # 7.0 Pneumatic Controllers -----------------------------------------------
  
  # Pneumatic controllers have ongoing emissions. Reference eci$pctrl CPT and 
  # calculate emissions using calc_E_pctrl function. Annual total emissions are 
  # converted to a monthly basis and applied to each month using Eid.
  
  # Calculation - some NA values in input columns (as of 2016-03-30 version of
  # OGEI database), overwrite NAs as 0
  calc <- calc_E_pctrl(op       = NA.overwrite(eci$pctrl$avg_hours[wsim$eqEF.pctrl]),
                       nHB      = NA.overwrite(eci$pctrl$high_bleed[wsim$eqEF.pctrl]),
                       nIB      = NA.overwrite(eci$pctrl$intermittent_bleed[wsim$eqEF.pctrl]),
                       nLB      = NA.overwrite(eci$pctrl$low_bleed[wsim$eqEF.pctrl]),
                       pctrl.ef = eopt$pctrl.EF,
                       wfrac    = NA.overwrite(eci$pctrl$wfrac[wsim$eqEF.pctrl]))
  
  # Assign emissions
  E$pctrl$voc <- (calc / 12) * Eid
  
  
  # 8.0 Pneumatic Pumps -----------------------------------------------------
  
  # Pneumatic pumps have ongoing emissions. Reference eci$ppump CPT and calculate
  # emissions using calc_E_ppump function. Annual total emissions are converted to
  # a monthly basis and applied to each month using Eid.
  
  # Calculation - some NA values in input columns (as of 2016-03-30 version of
  # OGEI database), overwrite NAs as 0
  calc <- calc_E_ppump(vent.rate = NA.overwrite(eci$ppump$vent_rate[wsim$eqEF.ppump]),
                       MW        = NA.overwrite(eci$ppump$gas_mw[wsim$eqEF.ppump]),
                       op        = NA.overwrite(eci$ppump$annual_operation[wsim$eqEF.ppump]),
                       m.voc     = eci$ppump$voc_wt[wsim$eqEF.ppump],
                       wfrac     = NA.overwrite(eci$ppump$wfrac[wsim$eqEF.ppump]))
  
  # Assign emissions
  E$ppump$voc <- (calc / 12) * Eid
  
  
  # 9.0 Fugitives -----------------------------------------------------------
  
  # Fugitives are ongoing emissions from piping equipment. Reference eci$fug CPT
  # and calculate emissions using calc_E_fug function. Annual total emissions are
  # converted to a monthly basis and applied to each month using Eid.
  
  # Calculation - some NA values in input columns (as of 2016-03-30 version of
  # OGEI database), overwrite NAs as 0. Have to calculate for each component.
  
  # Gas Component
  calc.gas <- calc_E_fug(valves     = NA.overwrite(eci$fug$gas$valves[wsim$eqEF.fug.gas]),
                         pumpseals  = NA.overwrite(eci$fug$gas$pumpseals[wsim$eqEF.fug.gas]),
                         others     = NA.overwrite(eci$fug$gas$other1[wsim$eqEF.fug.gas]),
                         connectors = NA.overwrite(eci$fug$gas$connectors[wsim$eqEF.fug.gas]),
                         flanges    = NA.overwrite(eci$fug$gas$flanges[wsim$eqEF.fug.gas]),
                         open.lines = NA.overwrite(eci$fug$gas$open_lines[wsim$eqEF.fug.gas]),
                         fug.EF     = eopt$fug.EF,
                         op         = eci$fug$gas$production_hours[wsim$eqEF.fug.gas],
                         wfrac      = NA.overwrite(eci$fug$gas$wfrac[wsim$eqEF.fug.gas]),
                         type       = "gas")
  
  # Heavy Oil Component
  calc.hoil <- calc_E_fug(valves     = NA.overwrite(eci$fug$hoil$valves[wsim$eqEF.fug.hoil]),
                          pumpseals  = NA.overwrite(eci$fug$hoil$pumpseals[wsim$eqEF.fug.hoil]),
                          others     = NA.overwrite(eci$fug$hoil$other1[wsim$eqEF.fug.hoil]),
                          connectors = NA.overwrite(eci$fug$hoil$connectors[wsim$eqEF.fug.hoil]),
                          flanges    = NA.overwrite(eci$fug$hoil$flanges[wsim$eqEF.fug.hoil]),
                          open.lines = NA.overwrite(eci$fug$hoil$open_lines[wsim$eqEF.fug.hoil]),
                          fug.EF     = eopt$fug.EF,
                          op         = eci$fug$hoil$production_hours[wsim$eqEF.fug.hoil],
                          wfrac      = NA.overwrite(eci$fug$hoil$wfrac[wsim$eqEF.fug.hoil]),
                          type       = "hoil")
  
  # Light Oil Component
  calc.loil <- calc_E_fug(valves     = NA.overwrite(eci$fug$loil$valves[wsim$eqEF.fug.loil]),
                          pumpseals  = NA.overwrite(eci$fug$loil$pumpseals[wsim$eqEF.fug.loil]),
                          others     = NA.overwrite(eci$fug$loil$other1[wsim$eqEF.fug.loil]),
                          connectors = NA.overwrite(eci$fug$loil$connectors[wsim$eqEF.fug.loil]),
                          flanges    = NA.overwrite(eci$fug$loil$flanges[wsim$eqEF.fug.loil]),
                          open.lines = NA.overwrite(eci$fug$loil$open_lines[wsim$eqEF.fug.loil]),
                          fug.EF     = eopt$fug.EF,
                          op         = eci$fug$loil$production_hours[wsim$eqEF.fug.loil],
                          wfrac      = NA.overwrite(eci$fug$loil$wfrac[wsim$eqEF.fug.loil]),
                          type       = "loil")
  
  # Water/Oil Component
  calc.woil <- calc_E_fug(valves     = NA.overwrite(eci$fug$woil$valves[wsim$eqEF.fug.woil]),
                          pumpseals  = NA.overwrite(eci$fug$woil$pumpseals[wsim$eqEF.fug.woil]),
                          others     = NA.overwrite(eci$fug$woil$other1[wsim$eqEF.fug.woil]),
                          connectors = NA.overwrite(eci$fug$woil$connectors[wsim$eqEF.fug.woil]),
                          flanges    = NA.overwrite(eci$fug$woil$flanges[wsim$eqEF.fug.woil]),
                          open.lines = NA.overwrite(eci$fug$woil$open_lines[wsim$eqEF.fug.woil]),
                          fug.EF     = eopt$fug.EF,
                          op         = eci$fug$woil$production_hours[wsim$eqEF.fug.woil],
                          wfrac      = NA.overwrite(eci$fug$woil$wfrac[wsim$eqEF.fug.woil]),
                          type       = "woil")
  
  # Assign emissions
  E$fug$voc <- (calc.gas + calc.hoil + calc.loil + calc.woil) / 12 * Eid
  
  
  # 10. Result --------------------------------------------------------------
  
  # Calculate total emissions by species
  r <- list(pm10 = colSums(E$wc$pm10   + E$rt$pm10   + E$sh$pm10),
            pm25 = colSums(E$wc$pm25   + E$rt$pm25   + E$sh$pm25),
            sox  = colSums(E$rt$sox    + E$sh$sox),
            nox  = colSums(E$wc$nox    + E$rt$nox    + E$sh$nox + E$dh$nox + E$tank$nox),
            voc  = colSums(E$wc$voc    + E$rt$voc    + E$sh$voc + E$dh$voc + E$tank$voc + E$truck$voc +
                           E$pctrl$voc + E$ppump$voc + E$fug$voc),
            co   = colSums(E$wc$co     + E$rt$co     + E$sh$co  + E$dh$co  + E$tank$co),
            ch2o = colSums(E$rt$ch2o))
  
  # Calculate how much each type of equipment contributed to each emissions total
  r$fpm10 <- list(wc = colSums(E$wc$pm10) / r$pm10,
                  rt = colSums(E$rt$pm10) / r$pm10,
                  sh = colSums(E$sh$pm10) / r$pm10)
  
  r$fpm25 <- list(wc = colSums(E$wc$pm25) / r$pm25,
                  rt = colSums(E$rt$pm25) / r$pm25,
                  sh = colSums(E$sh$pm25) / r$pm25)
  
  r$fsox <- list(rt = colSums(E$rt$sox) / r$sox,
                 sh = colSums(E$sh$sox) / r$sox)
  
  r$fnox <- list(wc   = colSums(E$wc$nox)   / r$nox,
                 rt   = colSums(E$rt$nox)   / r$nox,
                 sh   = colSums(E$sh$nox)   / r$nox,
                 dh   = colSums(E$dh$nox)   / r$nox,
                 tank = colSums(E$tank$nox) / r$nox)
  
  r$fvoc <- list(wc    = colSums(E$wc$voc)    / r$voc,
                 rt    = colSums(E$rt$voc)    / r$voc,
                 sh    = colSums(E$sh$voc)    / r$voc,
                 dh    = colSums(E$dh$voc)    / r$voc,
                 tank  = colSums(E$tank$voc)  / r$voc,
                 truck = colSums(E$truck$voc) / r$voc,
                 pctrl = colSums(E$pctrl$voc) / r$voc,
                 ppump = colSums(E$ppump$voc) / r$voc,
                 fug   = colSums(E$fug$voc)   / r$voc)
  
  r$fco <- list(wc   = colSums(E$wc$co)   / r$co,
                rt   = colSums(E$rt$co)   / r$co,
                sh   = colSums(E$sh$co)   / r$co,
                dh   = colSums(E$dh$co)   / r$co,
                tank = colSums(E$tank$co) / r$co)
  
  # CH2O emissions are entirely from RICE and Turbines
  
  # Return result r
  return(r)
}
