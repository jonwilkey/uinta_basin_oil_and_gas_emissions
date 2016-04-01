# Function Info -----------------------------------------------------------
# Name:      emissionUpdate.R (Emissions Updating Function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - filepaths for raw/prepared data

# ver - file version number


# Outputs -----------------------------------------------------------------

# eci - list of CDFs for all inputs required in emissions calculation functions,
# organized by calculation type (well completion, RICE & Turbines, etc.)


# Description -------------------------------------------------------------

# This function blah blah blah


# Function ----------------------------------------------------------------

emissionUpdate <- function(path, ver) {
  
  # 0.0 Load OGEI tables --------------------------------------------------
  
  # File path for OGEI *.csv tables
  pOGEI <- file.path(path$raw, "OGEI")
  
  # Read in each table
  apis <-        read.csv(file.path(pOGEI, "apis.csv"))                  # API #'s associated with each facility ID
  comp.count <-  read.csv(file.path(pOGEI, "component_count.csv"))       # Component counts
  dehy <-        read.csv(file.path(pOGEI, "dehydrators.csv"))           # Dehydrators
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
  truck <-       read.csv(file.path(pOGEI, "truck_loading.csv"))         # Truck loading
  well.compl <-  read.csv(file.path(pOGEI, "well_completions.csv"))      # Well completions
  
  # Predefine emissions calculation input CDF list
  eci <- NULL
  
  
  # 0.1 Internal Probability Table Function -------------------------------
  
  # The following function takes a table m, finds the unique rows in m across 
  # all columns, and then counts the number of repeats of each row. Note -
  # requires at least two columns to work.
  cptable <- function(m) {
    
    # Create character string for use in SQL query
    m.names <- capture.output(cat(names(m), sep = ", "))
    
    # Use SQL query to get count of unique rows in m
    munq <- sqldf(paste("select ", m.names, ", count(*) as count
                        from m
                        group by ", m.names,
                        " order by count desc", sep = ""))
    
    # Convert cprod count into cumulative probability
    munq$cprob <- cumsum(munq$count/nrow(m))
    
    # Return munq
    return(munq)
  }
  
  
  # 0.2 Facility ID Well Counts -------------------------------------------
  
  # Since everything else in the simulation is accounted for on a well-by-well 
  # basis, we need to know how many wells are associated with each unique 
  # facility ID. Stated differently, we need to know the 'per well' fraction for
  # each facility. That way, when each well draws from the following probability
  # tables, we'll also know how much of what type of emissions are attributable
  # to that individual well.
  
  # Get counts of wells associated with each key value
  apis <- cptable(apis[, c("operator_id", "facility_id")])
  
  # Make a single key combining the operator and facility ID columns and combine
  # with 1 / (well count) value
  apis <- data.frame(key =   paste0(apis$operator_id, "-", apis$facility_id),
                     wfrac = 1/apis$count)
  
  # Internal function for merging apis well count fraction with other data.frames
  apimerge <- function(m) {
    
    # Make single key
    m$key <- paste0(m$operator_id, "-", m$facility_id)
    
    # Merge with apis
    m <- merge(x = m, y = apis, by.x = "key", by.y = "key", all.x = TRUE)
    
    # Return result
    return(m)
  }
  
  
  # 0.3 Production area designation list ----------------------------------
  
  # For emissions steps that reference prod.areas table values (gas MW, VOC wt%,
  # etc.) we need a key connecting production area designations in fac.list with
  # the key in apis
  
  m <- apimerge(fac.list)
  
  # Subset to desired columns
  prodlist <- data.frame(key =      m$key,
                         operator = m$operator_id,
                         area =     m$production_area)
  
  # Replace "Default" values with 0
  prodlist$area[which(prodlist$area == "Default")] <- "0"
  
  # Change from character to integer value
  prodlist$area <- as.integer(prodlist$area)
  
  # Add in data about natural gas molecular weight and VOC wt%
  temp <- subset(prod.areas,
                 subset = (analysis_type == 0),
                 select = c("operator_id",
                            "production_designation",
                            "gas_mw",
                            "voc_wt"))
  names(temp) <- c("operator", "area", "gas_mw", "voc_wt")
  
  prodlist <- merge(x = prodlist, y = temp,
                by = c("operator", "area"),
                all.x = TRUE)
  
  
  # 1.0 Well completion ---------------------------------------------------
  
  # The inputs here are used in the "calc_E_wc.R" function
  
  # Note - assuming all records here are for an entire well (i.e. even if same
  # facility has multiple apis associated with it, each row in well.compl is for
  # a single well at that facility ID).
  
  # Select desired inputs
  m <- well.compl[, c("annual_diesel_usage",
                      "flare_efficiency",
                      "pct_control")]
  
  # Write table to eci list
  eci$wc <- cptable(m)
  
  # # Individual CDFs
  # 
  # # 1.1 --- Fuel Usage ---
  # 
  # # Determine CDF for diesel fuel usage (in gallons) for drilling each well
  # eci$wc$fuel <- CDFq(vector = well.compl$annual_diesel_usage, xq = xq)
  # 
  # 
  # # 1.2 --- Control ---
  # 
  # # Determine probability of each type of control flare being used
  # 
  # # Get list of all unique combinations of control flare efficiencies and their
  # # associated % control values
  # wc.ctrl <- data.frame(unique(well.compl[,c("flare_efficiency","pct_control")]))
  # 
  # # Change names
  # names(wc.ctrl) <- c("type", "red")
  # 
  # # Define temporary vector
  # temp <- NULL
  # 
  # # For each flare type (listed in wc.ctrl)
  # for (i in wc.ctrl$type) {
  #   
  #   # Count fraction of entries in well.compl that have that control type
  #   temp <- c(temp, length(which(well.compl$flare_efficiency == i))/nrow(well.compl))
  # }
  # 
  # # Make cumulative probability data.frame
  # eci$wc$ctrl <- data.frame(wc.ctrl,
  #                           cprob = cumsum(temp))
  
  
  # 2.0 RICE & Turbines ---------------------------------------------------
  
  # The inputs here are used in the "calc_E_rt.R" function
  
  # 2.1 --- Data Prep ---
  
  # Add in wfrac using key
  m <- apimerge(rice.turb)
  
  # Make a new merged flat table "m" based on rice.turb containing only the
  # columns of interest to the emissions calculation
  m <- m[,c("rice_id",
            "horsepower",
            "annual_hours",
            "total_combusted",
            "fuel_heating",
            "control_type",
            "control_pm10",
            "control_pm25",
            "control_sox",
            "control_nox",
            "control_voc",
            "control_co",
            "control_ch2o",
            "wfrac")]

  # Internal functions for performing merge on RICE tables
  rtmerge <- function(m, rice.EF, p, names) {

    # Make subset for given pollutant p
    temp <- subset(rice.EF,
                   subset = (pollutant == p),
                   select = c("rice_id", "factor", "units"))

    # Rename
    names(temp) <- c("rice_id", names)

    # Merge
    m <- merge(x = m,
               y = temp,
               by.x = "rice_id", by.y = "rice_id",
               all.x = TRUE)
  }

  # Run merge function on all pollutants
  m <- rtmerge(m, rice.EF, p = 0, names = c("fpm10", "upm10")) # PM10
  m <- rtmerge(m, rice.EF, p = 1, names = c("fpm25", "upm25")) # PM25
  m <- rtmerge(m, rice.EF, p = 2, names = c("fsox",  "usox" )) # SOX
  m <- rtmerge(m, rice.EF, p = 3, names = c("fnox",  "unox" )) # NOX
  m <- rtmerge(m, rice.EF, p = 4, names = c("fvoc",  "uvoc" )) # VOC
  m <- rtmerge(m, rice.EF, p = 5, names = c("fco",   "uco"  )) # CO
  m <- rtmerge(m, rice.EF, p = 6, names = c("fch2o", "uch2o")) # CH2O
  
  # Drop the rice_id column
  m <- m[, -which(names(m) == "rice_id")]
  
  # WARNING - 67 NA's in result - currently cleared by next step
  
  # Drop any NA values (must have values in all remaining columns in order to
  # perform calculation)
  m <- na.omit(m)
  
  # 2.2 --- Cumulative Probability Table ---
  
  # Calculate and write results table to eci list
  eci$rt <- cptable(m)
  
  
  # 3.0 Separators & Heaters ----------------------------------------------
  
  # The inputs here are used in the "calc_E_sh.R" function
  
  # Add in wfrac using key
  m <- apimerge(sep.heat)
  
  # Select desired input columns
  m <- m[, c("heat_duty",
             "hours_operation",
             "control_status",
             "percent_control",
             "wfrac")]
  
  # Calculate and write results to table to eci list
  eci$sh <- cptable(m)
  
  # WARNING - 17 NA's in result
  
  # # Individual CDF version
  # 
  # # 3.1 --- Heat Duty Rating (MMBtu/hr) ---
  # 
  # # Calculate CDF
  # eci$sh$heat <- CDFq(vector = sep.heat$heat_duty, xq = xq)
  # 
  # # 3.2 --- Hours of Operation (hr/yr)
  # 
  # # Calculate CDF
  # eci$sh$op <- CDFq(vector = sep.heat$hours_operation, xq = xq)
  # 
  # # 3.3 --- Low NOx Control ---
  # 
  # # Determine probability of Low NOx burner being used
  # 
  # # Get list of all unique combinations of controls
  # sh.ctrl <- data.frame(unique(sep.heat[,c("control_status","percent_control")]))
  # 
  # # Change names
  # names(sh.ctrl) <- c("type", "red")
  # 
  # # Define temporary vector
  # temp <- NULL
  # 
  # # For each control type
  # for (i in sh.ctrl$type) {
  #   
  #   # Count fraction of entries that have that control type
  #   temp <- c(temp, length(which(sep.heat$control_status == i))/nrow(sep.heat))
  # }
  # 
  # # Make cumulative probability data.frame
  # eci$sh$ctrl <- data.frame(sh.ctrl,
  #                           cprob = cumsum(temp))
  
  
  # 4.0 Dehydrators -------------------------------------------------------
  
  # The inputs here are used in the "calc_E_dh.R" function
  
  # Add in wfrac using key
  m <- apimerge(dehy)
  
  # Select desired input columns
  m <- m[, c("hours_operation",
             "control_type",
             "percent_control",
             "heat_input_rate",
             "pilot_volume",
             "factor_voc",
             "wfrac")]
  
  # Calculate and write results to table to eci list
  eci$dh <- cptable(m)
  
  
  # 5.0 Tanks -------------------------------------------------------------
  
  # The inputs here are used in the "calc_E_tank.R" function
  
  # Add in wfrac using key
  m <- apimerge(tanks)
  
  # Calculate combined oil and condensate volume if condensate volume is not NA
  m$oil <- ifelse(test = is.na(tanks$throughput_condensate),
                  yes =  tanks$throughput_oil,
                  no =   tanks$throughput_oil + tanks$throughput_condensate)
  
  # Calculate ratio of voc / oil
  m$ratio <- m$total_voc / m$oil
  
  # Exclude any rows from m for which oil == 0 or for which the ratio is NA
  m <- m[-which(m$oil == 0 | is.na(m$ratio)), ]
  
  # Select desired input columns
  m <- m[, c("control_type",
             "control_percent",
             "combustor_heat_input",
             "pilot_volume",
             "oil",
             "total_voc",
             "ratio",
             "wfrac")]
  
  # Calculate and write results to table to eci list
  eci$tank <- cptable(m)
  
  
  # 6.0 Truck Loading -----------------------------------------------------
  
  # The inputs here are used in the "calc_E_truck.R" function
  
  # Loading is accounted for based on production from each individual well, no
  # need to merge with wfrac vector in apis data.frame
  
  # Select desired input columns
  m <- truck[, c("s_factor",
                 "vapor_pressure",
                 "molecular_weight",
                 "temp_r",
                 "control_type",
                 "control_percent")]
  
  # Calculate and write results to table to eci list
  eci$truck <- cptable(m)
  
  
  # 7.0 Pneumatic Controllers ---------------------------------------------
  
  # The inputs here are used in the "calc_E_pctrl.R" function
  
  # Add in wfrac using key
  m <- apimerge(pneum.ctrl)
  
  # Select desired input columns
  m <- m[, c("high_bleed",
             "intermittent_bleed",
             "low_bleed",
             "avg_hours",
             "wfrac")]
  
  # Calculate and write results to table to eci list
  eci$pctrl <- cptable(m)
  
  
  # 8.0 Pneumatic Pumps ---------------------------------------------------
  
  # The inputs here are used in the "calc_E_ppump.R" function
  
  # Add in wfrac using key
  m <- apimerge(pneum.pumps)
  
  # Merge with prodlist to get gas MW and VOC wt% values
  m <- merge(x = m, y = prodlist, by.x = "key", by.y = "key")
  
  # Select desired input columns
  m <- m[, c("annual_operation",
             "vent_rate",
             "control_type",
             "control_percent",
             "gas_mw",
             "voc_wt",
             "wfrac")]
  
  # Calculate and write results to table to eci list
  eci$ppump <- cptable(m)
  
  
  # 9.0 Fugitives ---------------------------------------------------------
  
  # The inputs here are used in the "calc_E_fug.R" function
  
  # Add in wfrac using key
  m <- apimerge(fugitives)
  
  # Need equipment counts from the component_count table. Merge that table with
  # prodlist so that key is available to match with fugitives table.
  
  # Internal function for merging with prodlist and then fugitives
  fugmerge <- function(k) {
    
    # Get subset of component count table for just component type "k"
    a <- comp.count[which(comp.count$component_type == k),
                    -which(names(comp.count) == "total_emissions" |
                             names(comp.count) == "component_type")]
    
    names(a) <- c(names(a)[1:(which(names(a) == "open_lines"))], "operator", "area")
    
    # Merge with key from prodlist
    a <- merge(x = a, y = prodlist[, c("operator", "area", "key")],
               by = c("operator", "area"),
               all.x = TRUE)
    
    # Only the results that have a key value
    a <- a[which(!is.na(a$key)),]
    
    # Merge with fugitives
    a <- merge(x = m, y = a,
               by.x = "key", by.y = "key",
               all.x = TRUE)
    
    # Select desired input columns
    a <- a[, c("production_hours",
               "voc_wt",
               "valves",
               "pumpseals",
               "other1",
               "connectors",
               "flanges",
               "open_lines",
               "wfrac")]
    
    # Return a
    return(a)
  }
  
  # Run merge function on each component type
  gas <-  fugmerge(k = 0) # Gas component
  hoil <- fugmerge(k = 1) # Heavy oil component
  loil <- fugmerge(k = 2) # Light oil component
  woil <- fugmerge(k = 3) # Water / oil component
  
  # Calculate and write results to eci list
  eci$fug$gas <-  cptable(gas)
  eci$fug$hoil <- cptable(hoil)
  eci$fug$loil <- cptable(loil)
  eci$fug$woil <- cptable(woil)
  
  
  # Save results ----------------------------------------------------------
  
  save(file = file.path(path$data,
                        paste("emissionUpdate_", ver, ".rda", sep = "")),
       list = c("eci"))
}
