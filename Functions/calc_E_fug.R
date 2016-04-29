# Function Info -----------------------------------------------------------
# Name:      calc_E_fug.R (Fugitive emissions calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# eq - equipment counts dataframe/matrix with rows (mass fraction VOC, valves, 
# pumps, others, connectors, flanges, open-ended lines) and columns (gas, heavy
# oil , light oil, water/oil)

# fug.EF - fugitive VOC emissions factors data.frame with the same rows/columns
# as eq input (but no mass fraction VOC row)

# op - total operating hours

# wfrac - how much each individual well contributes to emissions at full site

# type - component type (gas, hoil, loil, or woil)


# Outputs -----------------------------------------------------------------

# VOC - VOC emissions total from fugitive sources for given component type
# (ton/yr)


# Description -------------------------------------------------------------

# This function calculates fugitive emissions for the specified component


# Function ----------------------------------------------------------------

calc_E_fug <- function(valves, pumpseals, others, connectors, flanges,
                       open.lines, fug.EF, op, wfrac, type) {
  
  # Determine total fugitive emissions factor
  ef <- valves     * fug.EF["Valves",           type] +
        pumpseals  * fug.EF["Pumps",            type] +
        others     * fug.EF["Others",           type] +
        connectors * fug.EF["Connectors",       type] +
        flanges    * fug.EF["Flanges",          type] +
        open.lines * fug.EF["Open-Ended Lines", type]
  
  # Calculate VOC emissions (ton/yr)
  VOC <- op * ef / 2000 * wfrac
  
  # Return emissions result
  return(VOC)
}
