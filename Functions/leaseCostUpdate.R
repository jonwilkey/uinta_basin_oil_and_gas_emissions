# Function Info -----------------------------------------------------------
# Name:      leaseCostUpdate.R (Lease Capital and Operating Cost Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - list object containing directory paths for file I/O

# ver - version # for numbering *.rda output with unique filename

# tstart - start of modeling period

# tstop - end of modeling period

# "LOC oil and gas.csv" - implied input, formatted *.csv files from EIA 
# LOC report (see description for further info)

# basis - CPI index value to use to adjust prices to 'basis' USD

# LOCbasis - CPI index value at which LOC costs are adjusted to (last EIA report
# had them adjusted to 2009 values)

# eia.ep - EIA energy price history for Uinta Basin


# Outputs -----------------------------------------------------------------

# LOC.oil/gas.equip <- lm() fit object relating lease capital/operating costs to
# oil/gas price, well depth, and (for gas) gas production rate


# Description -------------------------------------------------------------

# This function fits a least squares model to the EIA Lease Operating Cost data 
# reported at: 
# http://www.eia.gov/pub/oil_gas/natural_gas/data_publications/cost_indices_equipment_production/current/coststudy.html

# The source data is contained in multiple tabs of an excel sheet, which must be
# reformatted into a csv matching the following format:
# https://docs.google.com/spreadsheets/d/1fhlVN5ERCzTKIdrTiQ4hjq29DK_isk3DpZKmdGrOQCU/edit?usp=sharing

# It is unlikely that this function will require updating as the lease operating
# cost data reporting has been discontinued by EIA.


# Function ----------------------------------------------------------------

leaseCostUpdate <- function(path, ver, tstart, tstop, basis, LOCbasis, eia.ep) {
  
  # Load EIA *.csv files ----------------------------------------------------
  LOC <- read.csv(file.path(path$raw, "LOC oil and gas.csv"))
  
  
  # Process raw data --------------------------------------------------------
  
  # Rename columns. Notes:
  # LOCtype: either "equip" for capital costs or "op" for monthly op expenses
  # cost:    in 2009 dollars per well
  # gasRate: in MCFD
  names(LOC) <- c("year", "LOCtype", "LOCindex", "cost", "depth", "wellType",
                  "gasRate")
  
  # Subset to specified time period
  LOC <- subset(LOC,
                subset = (year >= as.numeric(format(tstart, "%Y")) &
                            year <= as.numeric(format(tstop, "%Y"))))
  
  # Merge with price history ------------------------------------------------
  
  # Get subset of prices in same range as LOC
  eia.ep <- subset(eia.ep,
                   subset = (as.Date(month) >= tstart & as.Date(month) <= tstop))
  
  # Get zoo objects for both op and gp
  op.z <- zoo(eia.ep$OP, eia.ep$month)
  gp.z <- zoo(eia.ep$GP, eia.ep$month)
  
  # Define annual aggregating function
  as.year <- function(x) as.numeric(floor(as.yearmon(x)))
  
  # Get annual energy prices
  op.z <-aggregate(op.z, as.year, mean)
  gp.z <-aggregate(gp.z, as.year, mean)
  
  # Convert from zoo object to data.frame with integer year index
  op.z <- data.frame(year = as.integer(index(op.z)), op = coredata(op.z))
  gp.z <- data.frame(year = as.integer(index(gp.z)), op = coredata(gp.z))
  
  # Merge with LOC data
  LOC <- merge(x = LOC, y = op.z, by.x = "year", by.y = "year", all.x = T)
  LOC <- merge(x = LOC, y = gp.z, by.x = "year", by.y = "year", all.x = T)
  
  # Rename columns
  names(LOC) <- c("year", "LOCtype", "LOCindex", "cost", "depth", "wellType",
                  "gasRate", "op", "gp")
  
  
  # Inflation adjustment ----------------------------------------------------
  
  # Adjust lease costs to CPI basis dollars
  LOC$cost <- LOC$cost*(basis/LOCbasis)
  
  # Make copy for export
  LOC.data <- LOC
  
  
  # Run lm() ----------------------------------------------------------------
  
  LOC.oil.equip <- lm(formula = cost ~ op+depth-1,
                      data = LOC[which(LOC$wellType == "OW" & LOC$LOCtype == "equip"),])
  LOC.oil.op <-    lm(formula = cost ~ op+depth-1,
                      data = LOC[which(LOC$wellType == "OW" & LOC$LOCtype == "op"),])
  LOC.gas.equip <- lm(formula = cost ~ gp+depth+gasRate-1,
                      data = LOC[which(LOC$wellType == "GW" & LOC$LOCtype == "equip"),])
  LOC.gas.op <-    lm(formula = cost ~ gp+depth+gasRate-1,
                      data = LOC[which(LOC$wellType == "GW" & LOC$LOCtype == "op"),])
  
  
  # Save results ------------------------------------------------------------
  
  save(file=file.path(path$data, 
                      paste("leaseCost_", ver, ".rda", sep = "")),
                      list=c("LOC.oil.equip", "LOC.oil.op", "LOC.gas.equip",
                             "LOC.gas.op", "LOC.data"))
}