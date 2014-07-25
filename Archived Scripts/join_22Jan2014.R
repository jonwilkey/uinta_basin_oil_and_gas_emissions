#-------------------------------------------------------------------------------
# Options
#-------------------------------------------------------------------------------
options(stringsAsFactors=FALSE)
options(width=200)
options(drop=FALSE)

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------
library(data.table)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Raw data.
rdpath <- "/media/Data_Share/data/research/raw_data"
# Prepared data.
pdpath <- "/media/Data_Share/data/research/prepared_data"
# functions 
fin <- "/home/michael/r_code"
# lookup tables
lin <- "/home/michael/lutables"

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------
flst <- file.path(fin,c("clean_names.R",
                        "diffMonPOSIX.R"
                       )
                  )
for (f in flst) source(f)

#-------------------------------------------------------------------------------
# Lookup tables 
#-------------------------------------------------------------------------------
names_pattern_replacement <- read.csv(file.path(lin,"names_pattern_replacement.csv"),
                                      header=TRUE,
                                      colClasses="character"
                                      )
names_pattern_replacement <- as.matrix(names_pattern_replacement)

#-------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------
load(file.path(pdpath,"proddata.rda"))
load(file.path(pdpath,"welldata.rda"))
load(file.path(pdpath,"histdata.rda"))

# Jon - you may want to do some subsetting here

#-------------------------------------------------------------------------------
# To data.table()
#-------------------------------------------------------------------------------
# Create data table p_dt from proddata.
p_dt <- data.table(proddata)
# Assign key to p_dt.
setkey(p_dt, p_api, p_rpt_period)

w_dt <- data.table(welldata)
setkey(w_dt, w_api)

# Merge proddata with welldata 
pw_dt <- p_dt[w_dt]

#-------------------------------------------------------------------------------
# Add columns to to pw_dt
#-------------------------------------------------------------------------------
# Add count of months since well first shows up in proddata (i.e. upon well
# completion).
pw_dt[, time := 1 + diffMonPOSIX(first = min(p_rpt_period),
        last = p_rpt_period), by = "p_api"
      ]

# Add count of production records.
pw_dt[, nrec := length(unique(p_rpt_period)), by = "p_api"]

# Max value of 'time'.
pw_dt[, maxtime := max(time), by = "p_api"]

# So I can pick out rows. 
pw_dt[, lastrecord := (time == maxtime), by = "p_api"]

# Add Boolean for whether records are complete.  Records are complete iff actual
# number of records equals max time.
pw_dt[, complete := (nrec == maxtime), by = "p_api"]

# Add Boolean for whether well ever produced; note this is not exactly right
# when have wells drilled prior to 1984 since they may have produced before
# coming into proddata.
pw_dt[, everproduce := any(p_wellstatus == "P"), by = "p_api"]

# Add Boolean for whether well was orphaned.
pw_dt[, orphan := any(p_acct_num == "N9999"), by = "p_api"]

# Add Boolean for whether p_wellstatus has ever been PA.
pw_dt[, everpa := any(p_wellstatus == "PA"), by = "p_api"]

# Add number of months between w_abndondate and p_rpt_period.
pw_dt[, padiff := diffMonPOSIX(first = p_rpt_period,
                               last = w_abndondate
                               )

      ]

# From data table to data frame. 
pw_df <- as.data.frame(pw_dt)

