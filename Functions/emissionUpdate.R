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

# This function loads tables exported from the OGEI database, finds the unique
# combinations of inputs for each type of equipment, and creates a cumulative
# probability table (CPT) for each. The result is saved out as a list named
# "eci" containing the CPTs for each type of equipment, containing all of the
# inputs necessary for performing equipment-based emissions calculations.


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
  prod.areas <-  read.csv(file.path(pOGEI, "production_areas.csv"))      # Production areas
  rice.EF <-     read.csv(file.path(pOGEI, "rice_emission_factors.csv")) # RICE emission factors
  rice.turb <-   read.csv(file.path(pOGEI, "rice_turbines.csv"))         # RICE turbines
  sep.heat <-    read.csv(file.path(pOGEI, "separators_heaters.csv"))    # Separators and heaters
  tanks <-       read.csv(file.path(pOGEI, "tanks.csv"))                 # Tanks
  truck <-       read.csv(file.path(pOGEI, "truck_loading.csv"))         # Truck loading
  well.compl <-  read.csv(file.path(pOGEI, "well_completions.csv"))      # Well completions

  # Predefine emissions calculation input CDF list
  eci <- NULL


  # # This section and all subsequent acpt sections commented out due to issues
  # # with multiple CPT row matches for some APIs
  #
  # # 0.1 Create API-CPT cross-reference table ------------------------------
  #
  # # Make a copy of apis table for cross-referencing APIs and CPTs
  # acpt <- apis
  #
  # # Add operator-facility ID key
  # acpt$key <- paste0(acpt$operator_id, "-", acpt$facility_id)
  #
  # # Strip whitespace from API column
  # acpt$api <- gsub("[[:space:]]", "", acpt$api)
  #
  # # Select only the first 10 characters of the API string (to match the format
  # # used by DOGM for p$p_api)
  # acpt$api <- substr(x = acpt$api, start = 1, stop = 10)
  #
  # # Remove unknown API numbers
  # acpt <- acpt[-which(acpt$api == "Unknown"), ]
  #
  # # Remove op and facility columns
  # acpt <- acpt[, -c(2, 3)]
  #
  # # Remove duplicates
  # acpt <- acpt[-which(duplicated(acpt)),]


  # 0.2 Internal Probability Table Function -------------------------------

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


  # 0.3 Facility ID Well Counts -------------------------------------------

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


  # 0.4 Production area designation list ----------------------------------

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

  # Add in data about natural gas molecular weight and VOC wt%. Note: only
  # looking at produced natural gas analysis (analysis_type == 0), not flash or
  # SWB gas (only need gas MW and voc wt% for produced natural gas for ppump
  # calculation).
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

  # # 0.5 acpt-cpt match function -------------------------------------------
  #
  # # Need function that finds first matching row in CPT for each API in acpt
  # acpt.cpt <- function(btab, bcpt) {
  #
  #   # -- Inputs --
  #   # btab - base table from OGEI database
  #   # bcpt - CPT created from base table
  #
  #   # Select just the entries from each  type necessary to match rows
  #   x <- btab[, 2:ncol(btab)]     # Excludes key column
  #   y <- bcpt[, 1:(ncol(bcpt)-2)] # Excludes count and cprob columns
  #
  #   # Predefine results vector
  #   result <- rep(NA, nrow(x))
  #
  #   # For each row in base table x
  #   for (i in 1:nrow(x)) {
  #
  #     # Set row counter k
  #     k <- 1
  #
  #     # Set test condition to false
  #     test <- FALSE
  #
  #     # Extract test vector z from row i of base table x
  #     z <- c(x[i, ])
  #
  #     # While (a) no identical row has been found and (b) the row number is <=
  #     # total number of rows in the CPT y
  #     while(!test & k <= nrow(y)) {
  #
  #       # Test row k of CPT y to see if it's identical to z
  #       test <- identical(c(y[k, ]), z)
  #
  #       # Increment the row counter k
  #       k <- k + 1
  #     }
  #
  #     # When a match is found, the previous value of k is the matching row
  #     result[i] <- k - 1
  #   }
  #
  #   # Return results vector
  #   return(result)
  # }


  # 1.0 Well completion ---------------------------------------------------

  # The inputs here are used in the "calc_E_wc.R" function

  # Note - assuming all records here are for an entire well (i.e. even if same
  # facility has multiple apis associated with it, each row in well.compl is for
  # a single well at that facility ID).

  # Select desired inputs
  m <- well.compl[, c("annual_diesel_usage",
                      "flare_efficiency",
                      "pct_control")]

  # Exclude any records that have zero diesel usage (243 out of 1147 records)
  m <- m[which(m$annual_diesel_usage != 0), ]

  # Write table to eci list
  eci$wc <- cptable(m)

  # Don't need to merge well completions with acpt because existing wells
  # are completed prior to start of simulation


  # 2.0 RICE & Turbines ---------------------------------------------------

  # The inputs here are used in the "calc_E_rt.R" function

  # 2.1 --- Data Prep ---

  # Add in wfrac using key
  m <- apimerge(rice.turb)

  # Make a new merged flat table "m" based on rice.turb containing only the
  # columns of interest to the emissions calculation
  m <- m[,c("key",
            "rice_id",
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

  # # Make a copy of m for acpt
  # n <- m

  # Drop the key column
  m <- m[, -which(names(m) == "key")]

  # 2.2 --- Cumulative Probability Table ---

  # Calculate and write results table to eci list. Note: NAs occur in result,
  # primarily in wfrac (133 NA values)
  eci$rt <- cptable(m)

  # # 2.3 --- Match to existing wells
  #
  # # Run acpt.cpt function to find which rows in eci$rt match APIs of existing
  # # wells
  # n$rt <- acpt.cpt(btab = n, bcpt = eci$rt)
  #
  # # Select only key and rt columns from n
  # n <- n[,c("key", "rt")]
  #
  # # If there are duplicates in n
  # if(length(which(duplicated(n))) > 0) {
  #
  #   # Then remove them
  #   n <- n[-which(duplicated(n)),]
  # }
  #
  # # Merge with acpt
  # acpt <- merge(x = acpt, y = n, all.x = T)


  # 3.0 Separators & Heaters ----------------------------------------------

  # The inputs here are used in the "calc_E_sh.R" function

  # Add in wfrac using key
  m <- apimerge(sep.heat)

  # Calculate fuel heating value
  m$fuel_heat <- with(m, heat_duty * hours_operation / total_combusted)

  # Select desired input columns
  m <- m[, c("key",
             "heat_duty",
             "hours_operation",
             "fuel_heat",
             "control_status",
             "percent_control",
             "wfrac")]

  # # Make copy of m and drop key column from m
  # n <- m; m <- m[, -which(names(m) == "key")]

  # Calculate and write results to table to eci list (results contain NAs)
  eci$sh <- cptable(m)

  # # Run acpt.cpt function to find rows in eci$sh match APIs of existing wells
  # n$sh <- acpt.cpt(btab = n, bcpt = eci$sh)
  #
  # # Select only key and sh columns from n
  # n <- n[,c("key", "sh")]
  #
  # # If there are duplicates in n
  # if(length(which(duplicated(n))) > 0) {
  #
  #   # Then remove them
  #   n <- n[-which(duplicated(n)),]
  # }
  #
  # # Merge with acpt
  # acpt <- merge(x = acpt, y = n, all.x = T)


  # 4.0 Dehydrators -------------------------------------------------------

  # The inputs here are used in the "calc_E_dh.R" function

  # Add in wfrac using key
  m <- apimerge(dehy)

  # Select desired input columns
  m <- m[, c("key",
             "hours_operation",
             "control_type",
             "percent_control",
             "heat_input_rate",
             "pilot_volume",
             "factor_voc",
             "wfrac")]

  # # Make copy of m and drop key column from m
  # n <- m; m <- m[, -which(names(m) == "key")]

  # Calculate and write results to table to eci list (results contain NAs)
  eci$dh <- cptable(m)

  # # Run acpt.cpt function to find rows in eci$dh match APIs of existing wells
  # n$dh <- acpt.cpt(btab = n, bcpt = eci$dh)
  #
  # # Select only key and dh columns from n
  # n <- n[,c("key", "dh")]
  #
  # # If there are duplicates in n
  # if(length(which(duplicated(n))) > 0) {
  #
  #   # Then remove them
  #   n <- n[-which(duplicated(n)),]
  # }
  #
  # # Merge with acpt
  # acpt <- merge(x = acpt, y = n, all.x = T)


  # 5.0 Tanks -------------------------------------------------------------

  # The inputs here are used in the "calc_E_tank.R" function

  # Add in wfrac using key
  m <- apimerge(tanks)

  # Calculate combined oil and condensate volume, setting any NA values to 0
  m$oil <- (ifelse(test = is.na(m$throughput_condensate),
                   yes =  0,
                   no =   m$throughput_condensate) +
              ifelse(test = is.na(m$throughput_oil),
                     yes =  0,
                     no =   m$throughput_oil))

  # Calculate ratio of voc / oil
  m$ratio <- m$total_voc / m$oil

  # Rewrite any m$ratio values from m for which oil == 0 or NA as zero. This
  # applies to 856 rows (out of 7826), of which 737 have total_voc == 0.
  m$ratio[which(m$oil == 0 | is.na(m$ratio))] <- 0

  # Select desired input columns
  m <- m[, c("key",
             "control_type",
             "control_percent",
             "combustor_heat_input",
             "pilot_volume",
             "oil",
             "total_voc",
             "ratio",
             "wfrac")]

  # # Make copy of m and drop key column from m
  # n <- m; m <- m[, -which(names(m) == "key")]

  # Calculate and write results to table to eci list
  eci$tank <- cptable(m)

  # # Run acpt.cpt function to find rows in eci$tank match APIs of existing wells
  # n$tank <- acpt.cpt(btab = n, bcpt = eci$tank)
  #
  # # Select only key and tank columns from n
  # n <- n[,c("key", "tank")]
  #
  # # If there are duplicates in n
  # if(length(which(duplicated(n))) > 0) {
  #
  #   # Then remove them
  #   n <- n[-which(duplicated(n)),]
  # }
  #
  # # Merge with acpt
  # acpt <- merge(x = acpt, y = n, all.x = T)


  # 6.0 Truck Loading -----------------------------------------------------

  # The inputs here are used in the "calc_E_truck.R" function

  # Loading is accounted for based on production from each individual well, no
  # need to merge with wfrac vector in apis data.frame

  # However we do need the key column for CPT row matching, so do it anyway
  m <- apimerge(truck)

  # Select desired input columns
  m <- m[, c("key",
             "s_factor",
             "vapor_pressure",
             "molecular_weight",
             "temp_r",
             "control_type",
             "control_percent")]

  # # Make copy of m and drop key column from m
  # n <- m; m <- m[, -which(names(m) == "key")]

  # Calculate and write results to table to eci list
  eci$truck <- cptable(m)

  # # Run acpt.cpt function to find rows in eci$truck match APIs of existing wells
  # n$truck <- acpt.cpt(btab = n, bcpt = eci$truck)
  #
  # # Select only key and truck columns from n
  # n <- n[,c("key", "truck")]
  #
  # # If there are duplicates in n
  # if(length(which(duplicated(n))) > 0) {
  #
  #   # Then remove them
  #   n <- n[-which(duplicated(n)),]
  # }
  #
  # # Merge with acpt
  # acpt <- merge(x = acpt, y = n, all.x = T)


  # 7.0 Pneumatic Controllers ---------------------------------------------

  # The inputs here are used in the "calc_E_pctrl.R" function

  # Add in wfrac using key
  m <- apimerge(pneum.ctrl)

  # Select desired input columns
  m <- m[, c("key",
             "high_bleed",
             "intermittent_bleed",
             "low_bleed",
             "avg_hours",
             "wfrac")]

  # # Make copy of m and drop key column from m
  # n <- m; m <- m[, -which(names(m) == "key")]

  # Calculate and write results to table to eci list
  eci$pctrl <- cptable(m)

  # # Run acpt.cpt function to find rows in eci$pctrl match APIs of existing wells
  # n$pctrl <- acpt.cpt(btab = n, bcpt = eci$pctrl)
  #
  # # Select only key and pctrl columns from n
  # n <- n[,c("key", "pctrl")]
  #
  # # If there are duplicates in n
  # if(length(which(duplicated(n))) > 0) {
  #
  #   # Then remove them
  #   n <- n[-which(duplicated(n)),]
  # }
  #
  # # Merge with acpt
  # acpt <- merge(x = acpt, y = n, all.x = T)


  # 8.0 Pneumatic Pumps ---------------------------------------------------

  # The inputs here are used in the "calc_E_ppump.R" function

  # Add in wfrac using key
  m <- apimerge(pneum.pumps)

  # Merge with prodlist to get gas MW and VOC wt% values
  m <- merge(x = m, y = prodlist, by.x = "key", by.y = "key")

  # Select desired input columns
  m <- m[, c("key",
             "annual_operation",
             "vent_rate",
             "control_type",
             "control_percent",
             "gas_mw",
             "voc_wt",
             "wfrac")]

  # # Make copy of m and drop key column from m
  # n <- m; m <- m[, -which(names(m) == "key")]

  # Calculate and write results to table to eci list
  eci$ppump <- cptable(m)

  # # Run acpt.cpt function to find rows in eci$ppump match APIs of existing wells
  # n$ppump <- acpt.cpt(btab = n, bcpt = eci$ppump)
  #
  # # Select only key and ppump columns from n
  # n <- n[,c("key", "ppump")]
  #
  # # If there are duplicates in n
  # if(length(which(duplicated(n))) > 0) {
  #
  #   # Then remove them
  #   n <- n[-which(duplicated(n)),]
  # }
  #
  # # Merge with acpt
  # acpt <- merge(x = acpt, y = n, all.x = T)


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
    a <- a[, c("key",
               "production_hours",
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
  gas  <- fugmerge(k = 0) # Gas component
  hoil <- fugmerge(k = 1) # Heavy oil component
  loil <- fugmerge(k = 2) # Light oil component
  woil <- fugmerge(k = 3) # Water / oil component

  # # Make copies of each table and drop key column
  # ngas  <- gas;  gas  <- gas [, -which(names(gas)  == "key")]
  # nhoil <- hoil; hoil <- hoil[, -which(names(hoil) == "key")]
  # nloil <- loil; loil <- loil[, -which(names(loil) == "key")]
  # nwoil <- woil; woil <- woil[, -which(names(woil) == "key")]

  # Calculate and write results to eci list
  eci$fug$gas <-  cptable(gas)
  eci$fug$hoil <- cptable(hoil)
  eci$fug$loil <- cptable(loil)
  eci$fug$woil <- cptable(woil)

  # # Run acpt.cpt function to find rows in eci$fug matching existing wells
  # ngas$gas   <- acpt.cpt(btab = ngas,  bcpt = eci$fug$gas)
  # nhoil$hoil <- acpt.cpt(btab = nhoil, bcpt = eci$fug$hoil)
  # nloil$loil <- acpt.cpt(btab = nloil, bcpt = eci$fug$loil)
  # nwoil$woil <- acpt.cpt(btab = nwoil, bcpt = eci$fug$woil)
  #
  # # Select only key and ppump columns from n
  # ngas  <- ngas [,c("key", "gas")]
  # nhoil <- nhoil[,c("key", "hoil")]
  # nloil <- nloil[,c("key", "loil")]
  # nwoil <- nwoil[,c("key", "woil")]
  #
  # # If there are duplicates in n
  # if(length(which(duplicated(n))) > 0) {
  #
  #   # Then remove them
  #   n <- n[-which(duplicated(n)),]
  # }
  #
  # # Remove duplicates
  # if(length(which(duplicated(ngas))) > 0)  ngas  <- ngas [-which(duplicated(ngas)),]
  # if(length(which(duplicated(nhoil))) > 0) nhoil <- nhoil[-which(duplicated(nhoil)),]
  # if(length(which(duplicated(nloil))) > 0) nloil <- nloil[-which(duplicated(nloil)),]
  # if(length(which(duplicated(nwoil))) > 0) nwoil <- nwoil[-which(duplicated(nwoil)),]
  #
  # # Merge with acpt
  # acpt <- merge(x = acpt, y = ngas,  all.x = T)
  # acpt <- merge(x = acpt, y = nhoil, all.x = T)
  # acpt <- merge(x = acpt, y = nloil, all.x = T)
  # acpt <- merge(x = acpt, y = nwoil, all.x = T)


  # Save results ----------------------------------------------------------

  save(file = file.path(path$data,
                        paste("emissionUpdate_", ver, ".rda", sep = "")),
       list = "eci")
}
