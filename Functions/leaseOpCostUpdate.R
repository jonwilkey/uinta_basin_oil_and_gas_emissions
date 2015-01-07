# Function Info -----------------------------------------------------------
# Name:      leaseOpCostUpdate.R (Lease Operating Cost Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - list object containing directory paths for file I/O

# ver - version # for numbering *.rda output with unique filename

# tstart - start of modeling period

# tstop - end of modeling period

# "LOC oil.csv" & "LOC gas.csv" - implied input, formatted *.csv files from EIA 
# LOC report (see description for further info)


# Outputs -----------------------------------------------------------------

# fit.LOC.oil <- lm() fit object relating lease operating costs to oil price
#                and well depth

# fit.LOC.gas <- lm() fit object relating lease operating costs to gas price,
#                well depth, and gas production rate


# Description -------------------------------------------------------------

# This function fits a least squares model to the EIA Lease Operating Cost data 
# reported at: 
# (http://www.eia.gov/pub/oil_gas/natural_gas/data_publications/cost_indices_equipment_production/current/coststudy.html).
# The source data is contained in multiple tabs of an excel sheet, which must be
# reformatted into a csv with the following columns: Year,  CPI	Price (nominal
# $/bbl),	Price (real 2009),	Depth (ft),	Cost (2009 USD)

# It is unlikely that this function will require updating as the lease operating
# cost data reporting has been discontinued by EIA. By default this function 
# uses all data in the *.csv files, but there is an option to use a subset of
# the dataset in between tstart and tstop if full = FALSE.


# Function ----------------------------------------------------------------

leaseOpCostUpdate <- function(path, ver, tstart, tstop, full) {
  
  # Load EIA *.csv files ----------------------------------------------------
  LOC.oil <- read.csv(file.path(path$raw, "LOC oil.csv"))
  LOC.gas <- read.csv(file.path(path$raw, "LOC gas.csv"))
  
  
  # Process raw data --------------------------------------------------------
  
  # All that we need to do is rename columns
  names(LOC.oil) <- c("year", "CPI", "nominal.price", "real.price", "depth",
                      "cost")
  names(LOC.gas) <- c("year", "CPI", "nominal.price", "real.price", "prodrate",
                      "depth", "cost")
  
  
  # Optional data subsetting ------------------------------------------------
  
  if(full == FALSE) {
    LOC.oil <- subset(LOC.oil,
                      subset = (year >= as.numeric(format(opt$tstart, "%Y")) &
                                year <= as.numeric(format(opt$tstop, "%Y"))))
  }
  
  
  # Run lm() ----------------------------------------------------------------
  
  fit.LOC.oil <- lm(formula = cost ~ real.price + depth,
                    data = LOC.oil)
  fit.LOC.gas <- lm(formula = cost ~ real.price + depth + prodrate,
                    data = LOC.gas)
  
  # Save results ------------------------------------------------------------
  
  save(file=file.path(path$data, 
                      paste("leaseOpCost_", ver, ".rda", sep = "")),
                      list=c("fit.LOC.oil", "fit.LOC.gas"))
}