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
  
  # Create function for calculting reduced emissions based on selection criteria
  # coded into each piece of equipment.
  redfun <- function(base, ind, tstep, red) {
    
    # base  = base emissions matrix
    # ind   = row index of wells to which reductions will be applied
    # tstep = time step at which to begin applying reduction
    # red   = Reduction to apply (expressed as a fraction)
    
    # Calculate reduced emissions and replace specified elements in base
    base[ind, tstep:ncol(base)] <- base[ind, tstep:ncol(base)] * (1 - red)
    
    # Return result
    return(base)
  }
  
  
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
  
  # Find row indices of wells to which reduction will be applied. By default,
  # the only criteria applied is related to tDrill (i.e. drilling date)
  ind <- which(wsim$tDrill >= eopt$r$wc$tDrill)
  
  # Calculate reduced emissions and assign to list rE
  rE <- list(wc = list(pm10 = redfun(base = E$wc$pm10, ind = ind, tstep = eopt$r$wc$tstep, red = eopt$r$wc$red.pm10),
                       pm25 = redfun(base = E$wc$pm25, ind = ind, tstep = eopt$r$wc$tstep, red = eopt$r$wc$red.pm25),
                       nox  = redfun(base = E$wc$nox,  ind = ind, tstep = eopt$r$wc$tstep, red = eopt$r$wc$red.nox),
                       voc  = redfun(base = E$wc$voc,  ind = ind, tstep = eopt$r$wc$tstep, red = eopt$r$wc$red.voc),
                       co   = redfun(base = E$wc$co,   ind = ind, tstep = eopt$r$wc$tstep, red = eopt$r$wc$red.co)))
  
  
  # 2.0 RICE and Turbines ---------------------------------------------------
  
  # RICE and turbines have ongoing emissions. Reference eci$rt CPT and calculate 
  # emissions using calc_E_rt function. Annual total emissions are converted to a 
  # montly basis and applied to each month that each well is in operation using
  # the Eid matrix.
  
  # Cleanup NAs in eci$rt - only NAs are in horsepower, total_combusted, and wfrac
  rtclean <- data.frame(horespower      = NA.overwrite(eci$rt$horsepower),
                        annual_hours    = eci$rt$annual_hours,
                        total_combusted = NA.overwrite(eci$rt$total_combusted),
                        fuel_heating    = eci$rt$fuel_heating,
                        control_type    = eci$rt$control_type,
                        control_pm10    = eci$rt$control_pm10,
                        control_pm25    = eci$rt$control_pm25,
                        control_sox     = eci$rt$control_sox,
                        control_nox     = eci$rt$control_nox,
                        control_voc     = eci$rt$control_voc,
                        control_co      = eci$rt$control_co,
                        control_ch2o    = eci$rt$control_ch2o,
                        wfrac           = NA.overwrite(eci$rt$wfrac),
                        eci$rt[, which(names(eci$rt) == "fpm10"):which(names(eci$rt) == "cprob")])
  
  # Calculation
  calc <- calc_E_rt(rt = rtclean)
  
  # Assign emissions
  E$rt$pm10 <- (calc$em.rt.pm10 / 12) * Eid
  E$rt$pm25 <- (calc$em.rt.pm25 / 12) * Eid
  E$rt$sox  <- (calc$em.rt.sox  / 12) * Eid
  E$rt$nox  <- (calc$em.rt.nox  / 12) * Eid
  E$rt$voc  <- (calc$em.rt.voc  / 12) * Eid
  E$rt$co   <- (calc$em.rt.co   / 12) * Eid
  E$rt$ch2o <- (calc$em.rt.ch2o / 12) * Eid
  
  # Find row indices of wells to which reduction will be applied. By default,
  # the only criteria applied is related to tDrill (i.e. drilling date)
  ind <- which(wsim$tDrill >= eopt$r$rt$tDrill)
  
  # Calculate reduced emissions and assign to list rE
  rE$rt$pm10 <- redfun(base = E$rt$pm10, ind = ind, tstep = eopt$r$rt$tstep, red = eopt$r$rt$red.pm10)
  rE$rt$pm25 <- redfun(base = E$rt$pm25, ind = ind, tstep = eopt$r$rt$tstep, red = eopt$r$rt$red.pm25)
  rE$rt$sox  <- redfun(base = E$rt$sox,  ind = ind, tstep = eopt$r$rt$tstep, red = eopt$r$rt$red.sox)
  rE$rt$nox  <- redfun(base = E$rt$nox,  ind = ind, tstep = eopt$r$rt$tstep, red = eopt$r$rt$red.nox)
  rE$rt$voc  <- redfun(base = E$rt$voc,  ind = ind, tstep = eopt$r$rt$tstep, red = eopt$r$rt$red.voc)
  rE$rt$co   <- redfun(base = E$rt$co,   ind = ind, tstep = eopt$r$rt$tstep, red = eopt$r$rt$red.co)
  rE$rt$ch2o <- redfun(base = E$rt$ch2o, ind = ind, tstep = eopt$r$rt$tstep, red = eopt$r$rt$red.ch2o)
  
  
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
  
  # Find row indices of wells to which reduction will be applied. By default,
  # the only criteria applied is related to tDrill (i.e. drilling date)
  ind <- which(wsim$tDrill >= eopt$r$sh$tDrill)
  
  # Calculate reduced emissions and assign to list rE
  rE$sh$pm10 <- redfun(base = E$sh$pm10, ind = ind, tstep = eopt$r$sh$tstep, red = eopt$r$sh$red.pm10)
  rE$sh$pm25 <- redfun(base = E$sh$pm25, ind = ind, tstep = eopt$r$sh$tstep, red = eopt$r$sh$red.pm25)
  rE$sh$sox  <- redfun(base = E$sh$sox,  ind = ind, tstep = eopt$r$sh$tstep, red = eopt$r$sh$red.sox)
  rE$sh$nox  <- redfun(base = E$sh$nox,  ind = ind, tstep = eopt$r$sh$tstep, red = eopt$r$sh$red.nox)
  rE$sh$voc  <- redfun(base = E$sh$voc,  ind = ind, tstep = eopt$r$sh$tstep, red = eopt$r$sh$red.voc)
  rE$sh$co   <- redfun(base = E$sh$co,   ind = ind, tstep = eopt$r$sh$tstep, red = eopt$r$sh$red.co)
  
  
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
  
  # Find row indices of wells to which reduction will be applied. By default,
  # the only criteria applied is related to tDrill (i.e. drilling date)
  ind <- which(wsim$tDrill >= eopt$r$dh$tDrill)
  
  # Calculate reduced emissions and assign to list rE
  rE$dh$nox  <- redfun(base = E$dh$nox,  ind = ind, tstep = eopt$r$dh$tstep, red = eopt$r$dh$red.nox)
  rE$dh$voc  <- redfun(base = E$dh$voc,  ind = ind, tstep = eopt$r$dh$tstep, red = eopt$r$dh$red.voc)
  rE$dh$co   <- redfun(base = E$dh$co,   ind = ind, tstep = eopt$r$dh$tstep, red = eopt$r$dh$red.co)
  
  
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
  
  # Find row indices of wells to which reduction will be applied. Here the 
  # criteria applied is related to tDrill (i.e. drilling date) and annual
  # average VOC production (avoc).
  ind <- which(wsim$tDrill >= eopt$r$tank$tDrill &
               rowSums(E$tank$voc)/12 >= eopt$r$tank$avoc)
  
  # Calculate reduced emissions and assign to list rE
  rE$tank$nox  <- redfun(base = E$tank$nox,  ind = ind, tstep = eopt$r$tank$tstep, red = eopt$r$tank$red.nox)
  rE$tank$voc  <- redfun(base = E$tank$voc,  ind = ind, tstep = eopt$r$tank$tstep, red = eopt$r$tank$red.voc)
  rE$tank$co   <- redfun(base = E$tank$co,   ind = ind, tstep = eopt$r$tank$tstep, red = eopt$r$tank$red.co)
  
  
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
  
  # It's possible to have a pick of 0 for the temperature, given the database.
  # If that occurs, the result will come back as NaN (since calculation divides
  # by temperature). Find and replace any such NA values as 0.
  calc <- NA.overwrite(calc)
  
  # Calculate and assign emissions
  E$truck$voc <- calc * osim
  
  # Find row indices of wells to which reduction will be applied. By default,
  # the only criteria applied is related to tDrill (i.e. drilling date)
  ind <- which(wsim$tDrill >= eopt$r$truck$tDrill)
  
  # Calculate reduced emissions and assign to list rE
  rE$truck$voc  <- redfun(base = E$truck$voc,  ind = ind, tstep = eopt$r$truck$tstep, red = eopt$r$truck$red.voc)
  
  
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
  
  # Find row indices of wells to which reduction will be applied. By default,
  # the only criteria applied is related to tDrill (i.e. drilling date)
  ind <- which(wsim$tDrill >= eopt$r$pctrl$tDrill)
  
  # Calculate reduced emissions and assign to list rE
  rE$pctrl$voc  <- redfun(base = E$pctrl$voc,  ind = ind, tstep = eopt$r$pctrl$tstep, red = eopt$r$pctrl$red.voc)
  
  
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
  
  # Find row indices of wells to which reduction will be applied. By default,
  # the only criteria applied is related to tDrill (i.e. drilling date)
  ind <- which(wsim$tDrill >= eopt$r$ppump$tDrill)
  
  # Calculate reduced emissions and assign to list rE
  rE$ppump$voc  <- redfun(base = E$ppump$voc,  ind = ind, tstep = eopt$r$ppump$tstep, red = eopt$r$ppump$red.voc)
  
  
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
  
  # Find row indices of wells to which reduction will be applied. By default,
  # the only criteria applied is related to tDrill (i.e. drilling date)
  ind <- which(wsim$tDrill >= eopt$r$fug$tDrill)
  
  # Calculate reduced emissions and assign to list rE
  rE$fug$voc  <- redfun(base = E$fug$voc,  ind = ind, tstep = eopt$r$fug$tstep, red = eopt$r$fug$red.voc)
  
  
  # 10. Result --------------------------------------------------------------
  
  # Summation function
  etotal <- function(E) {
    
    # Create r list object
    r <- NULL
    
    # Calculate total emissions by species and equipment type
    r$fpm10 <- list(wc = colSums(E$wc$pm10),
                    rt = colSums(E$rt$pm10),
                    sh = colSums(E$sh$pm10))
    
    r$fpm25 <- list(wc = colSums(E$wc$pm25),
                    rt = colSums(E$rt$pm25),
                    sh = colSums(E$sh$pm25))
    
    r$fsox <- list(rt = colSums(E$rt$sox),
                   sh = colSums(E$sh$sox))
    
    r$fnox <- list(wc   = colSums(E$wc$nox),
                   rt   = colSums(E$rt$nox),
                   sh   = colSums(E$sh$nox),
                   dh   = colSums(E$dh$nox),
                   tank = colSums(E$tank$nox))
    
    r$fvoc <- list(wc    = colSums(E$wc$voc),
                   rt    = colSums(E$rt$voc),
                   sh    = colSums(E$sh$voc),
                   dh    = colSums(E$dh$voc),
                   tank  = colSums(E$tank$voc),
                   truck = colSums(E$truck$voc),
                   pctrl = colSums(E$pctrl$voc),
                   ppump = colSums(E$ppump$voc),
                   fug   = colSums(E$fug$voc))
    
    r$fco <- list(wc   = colSums(E$wc$co),
                  rt   = colSums(E$rt$co),
                  sh   = colSums(E$sh$co),
                  dh   = colSums(E$dh$co),
                  tank = colSums(E$tank$co))
    
    # CH2O emissions are entirely from RICE and Turbines
    
    # Calculate total emissions by species
    r$pm10 <- with(r$fpm10, wc + rt + sh)
    r$pm25 <- with(r$fpm25, wc + rt + sh)
    r$sox  <- with(r$fsox,  rt + sh)
    r$nox  <- with(r$fnox,  wc + rt + sh + dh + tank)
    r$voc  <- with(r$fvoc,  wc + rt + sh + dh + tank + truck + pctrl + ppump + fug)
    r$co   <- with(r$fco,   wc + rt + sh + dh + tank)
    r$ch2o <- colSums(E$rt$ch2o)
    
    # Return result
    return(r)
  }
  
  # Run summation function on base emissions list E
  result <- list(eb = etotal(E = E))
  
  # Run summation function on reduced emissions list rE
  result$re <- etotal(E = rE)
  
  # Return result
  return(result)
}
