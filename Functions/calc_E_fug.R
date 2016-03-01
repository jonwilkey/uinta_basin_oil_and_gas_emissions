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


# Outputs -----------------------------------------------------------------

# VOC - VOC emissions total from fugitive sources


# Description -------------------------------------------------------------

# This function calculates fugitive emissions


# Function ----------------------------------------------------------------

calc_E_fug <- function(eq, fug.EF, op) {
  
  # Determine total fugitive emissions factor
  ef <- eq[1,1]*sum(fug.EF[,1]*eq[2:nrow(eq),1]) +
        eq[1,2]*sum(fug.EF[,2]*eq[2:nrow(eq),2]) +
        eq[1,3]*sum(fug.EF[,3]*eq[2:nrow(eq),3]) +
        eq[1,4]*sum(fug.EF[,4]*eq[2:nrow(eq),4])
  
  # Calculate emissions
  VOC <- op * ef / 2000
  
  # Return emissions result
  return(ef)
}
