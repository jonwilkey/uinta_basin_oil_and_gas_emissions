# Function Info -----------------------------------------------------------
# Name:      emissionUpdate.R (Emissions Updating Function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - filepaths for raw/prepared data

# ver - file version number

# wc.ctrl - Well completion flaring control types and reduction efficiencies
# data.frame


# Outputs -----------------------------------------------------------------

# wc.fuel.CDF - CDF for fuel usage from well completions

# wc.ctrl.prob - cumulative probability of various well completion control types


# Description -------------------------------------------------------------

# This function blah blah blah


# Function ----------------------------------------------------------------

emissionUpdate <- function(path, ver, wc.ctrl) {
  
  # 0.0 Load OGEI tables --------------------------------------------------
  
  # File path for OGEI *.csv tables
  pOGEI <- file.path(path$raw, "OGEI")
  
  # Read in each table
  apis <-        read.csv(file.path(pOGEI, "apis.csv"))                  # API #'s associated with each facility ID
  comp.count <-  read.csv(file.path(pOGEI, "component_count.csv"))       # Component counts
  dehydrators <- read.csv(file.path(pOGEI, "dehydrators.csv"))           # Dehydrators
  fac.list <-    read.csv(file.path(pOGEI, "facilities_list.csv"))       # Facilities list
  fugitives <-   read.csv(file.path(pOGEI, "fugitives.csv"))             # Fugitive emissions
  oper.info <-   read.csv(file.path(pOGEI, "operator_info.csv"))         # Operator information
  pneum.ctrl <-  read.csv(file.path(pOGEI, "pneumatic_controllers.csv")) # Pneumatic controllers
  pneum.pumps <- read.csv(file.path(pOGEI, "pneumatic_pumps.csv"))       # Pneumatic pumps
  prod.areas <-  read.csv(file.path(pOGEI, "production_areas.csv"))      # Production areas (keep?)
  rice.EF <-     read.csv(file.path(pOGEI, "rice_emission_factors.csv")) # RICE emission factors
  rice.turb <-   read.csv(file.path(pOGEI, "rice_turbines.csv"))         # RICE turbines
  sep.heat <-    read.csv(file.path(pOGEI, "separators_heaters.csv"))    # Separators and heaters
  tanks <-       read.csv(file.path(pOGEI, "tanks.csv"))                 # Tanks
  truck.load <-  read.csv(file.path(pOGEI, "truck_loading.csv"))         # Truck loading
  well.compl <-  read.csv(file.path(pOGEI, "well_completions.csv"))      # Well completions
  
  
  # 1.0 Well completion ---------------------------------------------------
  
  # 1.1 --- Fuel usage CDF ---
  
  # Determine CDF for diesel fuel usage (in gallons) for drilling each well
  
  # Calculate CDF
  wc.fuel.CDF <- CDFq(vector = well.compl$annual_diesel_usage, xq = opt$xq)
  
  
  # 1.2 --- Cumulative probability of control type ---
  
  # Determine probability of each type of control flare being used
  
  # Define temporary vector
  temp <- NULL
  
  # For each flare type (listed in wc.ctrl)
  for (i in wc.ctrl$type) {
    
    # Count fraction of entries in well.compl that have that control type
    temp <- c(temp, length(which(well.compl$flare_efficiency == i))/nrow(well.compl))
  }
  
  # Make cumulative probability data.frame
  wc.ctrl.prob <- data.frame(wc.ctrl,
                             cprob = cumsum(temp))
  
  # 2.0 RICE & Turbines -----------------------------------------------------
  
  
  # 3.0 Separators & Heaters ------------------------------------------------
  
  
  # 4.0 Dehydrators ---------------------------------------------------------
  
  
  # 5.0 Tanks ---------------------------------------------------------------
  
  
  # 6.0 Truck Loading -------------------------------------------------------
  
  
  # 7.0 Pneumatic Controllers -----------------------------------------------
  
  
  # 8.0 Pneumatic Pumps -----------------------------------------------------
  
  
  # 9.0 Fugitives -----------------------------------------------------------
  
  
  # Save results ------------------------------------------------------------
  
  save(file = file.path(path$data,
                        paste("emissionUpdate_", ver, ".rda", sep = "")),
       list = c("wc.fuel.CDF",
                "wc.ctrl.prob"))
}




