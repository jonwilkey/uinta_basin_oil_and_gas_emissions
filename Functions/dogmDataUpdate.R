# Function Info -----------------------------------------------------------
# Name:      dogmDataUpdate.R (DOGM *.dbf database file processing/updating)
# Author(s): Michael Hogue, Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - list object containing directory paths for file I/O


# Outputs -----------------------------------------------------------------

# production.rda - Processed and merged copy of three DOGM *.dbf files
#                  (proddata, histdata, and welldata). Also saves individual
#                  copies of each database file in *.csv and *.rda formats.


# Description -------------------------------------------------------------

# This function is used for extracting the information contained in DOGM's *.dbf
# database files into a format usable by R (*.rda). In addition to extracting
# the database files, this function also merges the database files together into
# a single large dataframe and adds several columns for subsequent analysis.


# Function ----------------------------------------------------------------
dogmDataUpdate <- function(path) {
  
  # Options -----------------------------------------------------------------
  
  options(width=200)
  options(drop=FALSE)
  
  
  # Functions ---------------------------------------------------------------
  
  # List of functions to load
  flst <- file.path(path$fun, c("clean_names.R",
                                "diffMonPOSIX.R"))
  
  # Load all functions in file list
  for (f in flst) source(f)
  
  
  # Lookup Tables -----------------------------------------------------------
  
  names_pattern_replacement <- read.csv(file.path(path$look,
                                                  "names_pattern_replacement.csv"),
                                        header=TRUE,
                                        colClasses="character")
  
  names_pattern_replacement <- as.matrix(names_pattern_replacement)
  
  
  # Read-in DBF as dataframes and prepare -----------------------------------
  
  # Import the DBF files into R dataframes 
  welldata <- read.dbf(file.path(path$raw, "welldata.dbf"))
  proddata <- read.dbf(file.path(path$raw, "proddata.dbf"))
  histdata <- read.dbf(file.path(path$raw, "histdata.dbf"))
  
  # Drop welldata's comments and modify date columns
  welldata <- subset(welldata,select=-c(COMMENTS,MODIFYDATE))
  
  
  # Write dataframes to comma-delimited files -------------------------------
  
  write.table(welldata,file=file.path(path$raw, "welldata.txt"),
              sep=",",quote=TRUE,row.names=FALSE,na="NULL")
  write.table(proddata,file=file.path(path$raw, "proddata.txt"),
              sep=",",quote=TRUE,row.names=FALSE,na="NULL")
  write.table(histdata,file=file.path(path$raw, "histdata.txt"),
              sep=",",quote=TRUE,row.names=FALSE,na="NULL")
  
  
  # Create proddata ---------------------------------------------------------
  
  proddata_colclasses <- c("Date",      # rpt_period
                           "character", # acct_num
                           "character", # alt_addres
                           "character", # api
                           "character", # prod_zone
                           "character", # entity
                           "character", # wellstatus
                           "character", # well_type
                           "numeric",   # days_prod
                           "numeric",   # oil_prod
                           "numeric",   # gas_prod
                           "numeric",   # water_prod
                           "character", # date_recd
                           "character") # amend_flag
  
  proddata <- read.csv(file.path(path$raw,"proddata.txt"),
                       header=TRUE,
                       comment.char = "",
                       colClasses = proddata_colclasses)
  
  # Use clean_names function on proddata
  new_names <- clean_names(names(proddata), table=names_pattern_replacement)
  
  # Prepend 'p_' for 'proddata' to new names
  new_names <- gsub(pattern="^", replacement = "p_", x = new_names)
  
  # Assign these new names to proddata
  names(proddata) <- new_names
  
  # Remove unwanted fields p_alt_addres, p_date_recd, and p_amend_flag
  omits <- c("p_alt_addres", "p_date_recd", "p_amend_flag")
  if (all((omits %in% names(proddata)))) {
    proddata <- proddata[,setdiff(names(proddata), omits)]  
  } else stop("Check column names")
  
  # Order by api, rpt_period
  proddata <- proddata[order(proddata[,"p_api"], proddata[,"p_rpt_period"]),]
  
  
  # Combining production for wells producing in more than one zone ----------
  
  # Extract all and only records for wells with duplicate api-rpt_period
  # (these are wells with production in multiple zones).
  dd <- duplicated(proddata[,c("p_api","p_rpt_period")])
  aa <- unique(proddata[dd,"p_api"])
  
  # Multi zone.
  mz <- subset(proddata, subset = (p_api %in% aa))
  
  # Single zone.
  sz <- subset(proddata,
               subset = (p_api %in% setdiff(unique(proddata[,"p_api"]), aa)))
  
  # Combine production records from different production zones.
  # Get fields which will be summed (oil, gas, and water production) or the
  # maximum taken (days_prod). 
  mz1 <- mz[,c("p_rpt_period","p_api","p_oil_prod","p_gas_prod","p_water_prod",
               "p_days_prod")]
  
  # Do the combining. 
  mz1 <- ddply(mz1,
               c("p_api", "p_rpt_period"),
               summarize,
               p_oil_prod = sum(p_oil_prod),
               p_gas_prod = sum(p_gas_prod),
               p_water_prod = sum(p_water_prod),
               p_days_prod = max(p_days_prod))
  
  # Get fields which are being held constant for each distinct api-rpt_period
  # value, then remove duplicate rows.
  mz2 <- mz[,c("p_rpt_period","p_api","p_acct_num","p_prod_zone","p_entity",
               "p_wellstatus","p_well_type")]
  ww <- which(duplicated(mz2[,c("p_rpt_period","p_api")]))
  mz2 <- mz2[-ww,]
  
  # Now merge mz1 and mz2 as mz. 
  mz <- merge(mz1,
              mz2,
              by.x = c("p_api","p_rpt_period"),
              by.y = c("p_api","p_rpt_period"),
              sort = FALSE)
  
  # Now bind sz and mz as proddata.
  proddata <- rbind(sz,mz)
  
  # Reorder. 
  proddata <- proddata[order(proddata[,"p_api"], proddata[,"p_rpt_period"]),]
  
  
  # Create welldata ---------------------------------------------------------
  
  welldata_colclasses <- c("character", # api
                           "character", # well_name
                           "character", # acct_num
                           "character", # alt_addres
                           "numeric",   # field_num
                           "character", # elevation
                           "character", # locat_foot
                           "character", # utm_surf_n
                           "character", # utm_surf_e
                           "character", # utm_bhl_n
                           "character", # utm_bhl_e
                           "character", # qtr_qtr
                           "character", # section
                           "character", # township
                           "character", # range
                           "character", # meridian
                           "character", # county
                           "character", # dir_horiz
                           "character", # conf_flag
                           "character", # conf_date
                           "character", # lease_num
                           "character", # lease_type
                           "Date",      # abndondate
                           "character", # wellstatus
                           "character", # well_type
                           "numeric",   # totcum_oil
                           "numeric",   # totcum_gas                         
                           "numeric",   # totcum_wtr
                           "character", # ind_tribe
                           "character", # multi_lats
                           "character", # cbmethflag
                           "character", # surfowntyp
                           "character", # bond_num
                           "character", # bond_type
                           "character", # ca_number
                           "character", # field_type
                           "character", # unit_name
                           "character", # lat_surf
                           "character") # long_surf
  
  welldata <- read.csv(file.path(path$raw,"welldata.txt"),
                       header=TRUE,
                       comment.char = "",
                       colClasses = welldata_colclasses)
  
  # Clean names.
  new_names <- clean_names(names(welldata), table=names_pattern_replacement)
  
  # Prepend 'w_' for 'welldata' to cleaned names.
  new_names <- gsub(pattern="^", replacement = "w_", x = new_names)
  
  # Assign these new names to welldata.
  names(welldata) <- new_names
  
  # Remove unwanted fields.
  omits <- c("w_well_name", "w_alt_addres", "w_elevation", "w_locat_foot",
             "w_utm_bhl_n", "w_utm_bhl_e")
  if (all((omits %in% names(welldata)))) {
    welldata <- welldata[,setdiff(names(welldata), omits)]
  } else stop("Check column names")
  
  
  # Create histdata ---------------------------------------------------------
  
  histdata_colclasses <- c("character", # api 
                           "Date",      # apd_aprovd
                           "character", # work_type
                           "Date",      # spud_dry
                           "Date",      # spud_rotry                         
                           "character", # prod_zone
                           "Date",      # compl_date
                           "Date",      # intent_rec  
                           "Date",      # work_compl
                           "numeric",   # td_md
                           "numeric",   # td_tvd
                           "numeric",   # pbtd_md
                           "numeric",   # pbtd_tvd
                           "character", # wellstatus
                           "character", # well_type
                           "Date",      # first_prod
                           "character", # testmethod
                           "character", # choke
                           "character", # tubng_prs
                           "character", # casng_prs
                           "numeric",   # oil_24hr
                           "numeric",   # gas_24hr
                           "numeric",   # water_24hr
                           "character", # dir_survey
                           "character", # cored
                           "character", # dst
                           "character", # comp_type
                           "character", # directiona
                           "numeric",   # lat_count
                           "character", # rec_seq
                           "character") # conf_flag
  
  histdata <- read.csv(file.path(path$raw,"histdata.txt"),
                       header=TRUE,
                       comment.char = "",
                       colClasses = histdata_colclasses,
                       na.strings="NULL")
  
  # Clean names.
  new_names <- clean_names(names(histdata), table=names_pattern_replacement)
  
  # Prepend 'h_' for 'histdata' to cleaned names.
  new_names <- gsub(pattern="^", replacement = "h_", x = new_names)
  
  # Assign these new names to histdata.
  names(histdata) <- new_names
  omits <- c("h_td_tvd","h_pbtd_tvd","h_rec_seq")
  if (all((omits %in% names(histdata)))) {
    histdata <- histdata[,setdiff(names(histdata), omits)]
  } else stop("Check column names")
  
  
  # Save to *.rda files -----------------------------------------------------
  
  save(file=file.path(path$data, "proddata.rda"), list=c("proddata"))
  save(file=file.path(path$data, "welldata.rda"), list=c("welldata"))
  save(file=file.path(path$data, "histdata.rda"), list=c("histdata"))
  
  
  # Merge using data.table --------------------------------------------------
  
  # Create data.tables from data.frames of each DOGM database file
  p_dt <- data.table(proddata)
  w_dt <- data.table(welldata)
  h_dt <- data.table(histdata)
  
  # Assign keys
  setkey(p_dt, p_api, p_rpt_period)
  setkey(w_dt, w_api)
  setkey(h_dt, h_api)
  
  # Merge proddata with welldata
  pw_dt <- p_dt[w_dt, allow.cartesian = TRUE]
  
  # And now merge with histdata
  pwh_dt <- pw_dt[h_dt, allow.cartesian = TRUE]
  
  
  #-------------------------------------------------------------------------------
  # Add columns to to pwh_dt
  #-------------------------------------------------------------------------------
  
  # Add count of months since well first shows up in proddata (i.e. upon well
  # completion).
  pwh_dt[, time := 1 + diffMonPOSIX(first = min(p_rpt_period),
                                    last = p_rpt_period), by = "p_api"]
  
  # Add count of production records.
  pwh_dt[, nrec := length(unique(p_rpt_period)), by = "p_api"]
  
  # Max value of 'time'.
  pwh_dt[, maxtime := max(time), by = "p_api"]
  
  # So I can pick out rows. 
  pwh_dt[, lastrecord := (time == maxtime), by = "p_api"]
  
  # Add Boolean for whether records are complete.  Records are complete iff actual
  # number of records equals max time.
  pwh_dt[, complete := (nrec == maxtime), by = "p_api"]
  
  # Add Boolean for whether well ever produced; note this is not exactly right
  # when have wells drilled prior to 1984 since they may have produced before
  # coming into proddata.
  pwh_dt[, everproduce := any(p_wellstatus == "P"), by = "p_api"]
  
  # Add Boolean for whether well was orphaned
  pwh_dt[, orphan := any(p_acct_num == "N9999"), by = "p_api"]
  
  # Add Boolean for whether p_wellstatus has ever been PA
  pwh_dt[, everpa := any(p_wellstatus == "PA"), by = "p_api"]
  
  # Add number of months between w_abndondate and p_rpt_period
  pwh_dt[, padiff := diffMonPOSIX(first = p_rpt_period,
                                  last = w_abndondate)]
  
  # From data table to data frame
  production <- as.data.frame(pwh_dt)
  
  
  # Saved merged data.frame -------------------------------------------------
  
  save(file=file.path(path$data, "production.rda"), list=c("production"))
}