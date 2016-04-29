# Function Info -----------------------------------------------------------
# Name:      calc_E_rt.R (RICE & Turbines emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# rt - object containing row(s) from eci$rt cumulative probability table for
# which emissions are to be calculated


# Outputs -----------------------------------------------------------------

# E.rt - Emissions for RICE and turbines (in tons/yr)


# Description -------------------------------------------------------------

# This function calculates the total emissions from reciprocating internal 
# combustion engines (RICE) and turbines. Emissions are calculated using either 
# AP-42 Chapter 3 or NSPS Subpart JJJJ based on the units specified in the CPT
# table.


# Function ----------------------------------------------------------------

calc_E_rt <- function(rt) {
  
  # Calc function
  calc <- function(units, fuel, HV, EF, red, hp, hr, wfrac) {
    
    result <- ifelse(test = units == "lb/mmbtu",
                     yes =  fuel * HV * EF * (1 - red) / 2000 * wfrac,
                     no =   (((hp * hr * EF) / 453.592) * (1 - red)) / 2000 * wfrac)
  }
  
  # Perform calculation for each pollutant
  E.rt <- data.frame(em.rt.pm10 = calc(units = rt$upm10,
                                       fuel =  rt$total_combusted,
                                       HV =    rt$fuel_heating,
                                       EF =    rt$fpm10,
                                       red =   rt$control_pm10,
                                       hp =    rt$horsepower,
                                       hr =    rt$annual_hours,
                                       wfrac = rt$wfrac),
                     
                     em.rt.pm25 = calc(units = rt$upm25,
                                       fuel =  rt$total_combusted,
                                       HV =    rt$fuel_heating,
                                       EF =    rt$fpm25,
                                       red =   rt$control_pm25,
                                       hp =    rt$horsepower,
                                       hr =    rt$annual_hours,
                                       wfrac = rt$wfrac),
                     
                     em.rt.sox  = calc(units = rt$usox,
                                       fuel =  rt$total_combusted,
                                       HV =    rt$fuel_heating,
                                       EF =    rt$fsox,
                                       red =   rt$control_sox,
                                       hp =    rt$horsepower,
                                       hr =    rt$annual_hours,
                                       wfrac = rt$wfrac),
                     
                     em.rt.nox  = calc(units = rt$unox,
                                       fuel =  rt$total_combusted,
                                       HV =    rt$fuel_heating,
                                       EF =    rt$fnox,
                                       red =   rt$control_nox,
                                       hp =    rt$horsepower,
                                       hr =    rt$annual_hours,
                                       wfrac = rt$wfrac),
                     
                     em.rt.voc  = calc(units = rt$uvoc,
                                       fuel =  rt$total_combusted,
                                       HV =    rt$fuel_heating,
                                       EF =    rt$fvoc,
                                       red =   rt$control_voc,
                                       hp =    rt$horsepower,
                                       hr =    rt$annual_hours,
                                       wfrac = rt$wfrac),
                     
                     em.rt.co   = calc(units = rt$uco,
                                       fuel =  rt$total_combusted,
                                       HV =    rt$fuel_heating,
                                       EF =    rt$fco,
                                       red =   rt$control_co,
                                       hp =    rt$horsepower,
                                       hr =    rt$annual_hours,
                                       wfrac = rt$wfrac),
                     
                     em.rt.ch2o = calc(units = rt$uch2o,
                                       fuel =  rt$total_combusted,
                                       HV =    rt$fuel_heating,
                                       EF =    rt$fch2o,
                                       red =   rt$control_ch2o,
                                       hp =    rt$horsepower,
                                       hr =    rt$annual_hours,
                                       wfrac = rt$wfrac))
  
  # Return emissions result
  return(E.rt)
}
