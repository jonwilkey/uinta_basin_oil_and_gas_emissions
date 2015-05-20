# Function Info -----------------------------------------------------------
# Name:      reworkUpdate.R (Well Rework Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# p - DOGM database data frame, used here to get list of wells located in Uintah
# or Duchesne counties

# tstart - start date of wells to include in analysis

# tstop - stop date of wells to include in analysis

# ver - Version number for file naming of exported data.frames

# wc.min - minimum number of wells in population required in order to include in
# CDF


# Outputs -----------------------------------------------------------------

# cdf.rework - data frame with CDF for probability that a well is reworked by
# the nth month of its production life


# Description -------------------------------------------------------------

# This function calculates the CDF that a well is reworked (reperforated or 
# recompleted) as a function of how far a well is into its productive life and 
# what type of well it is (oil or gas). The function begins by loading the 
# histdata data frame from disk. Next, the function selects out a subset of the 
# histdata on wells included in p (the subset of the DOGM database being 
# analyzed in the rest of the file - by default Uintah and Duchesne counties). 
# The histdata information is further subsetted into one data frame containing 
# the dates when a well was drilled ("drill") during the time frame of interest 
# between tstart and tstop, and a second data frame containing the dates when a 
# well was reworked. The number of wells that have existed for at least n months
# is calculated from 1 to duration of the oldest well in the dataset. Next, the 
# number of wells that were reworked n months into their productive life is 
# calculated. The rework vector is divided by the number of wells present to get
# the probability that a well will be reworked as a function of time, and that
# probability is summed together to the CDF for reworking.


# Function ----------------------------------------------------------------

reworkUpdate <- function(path, p, tstart, tstop, ver, wc.min) {
  
  # Load data ---------------------------------------------------------------
  
  # Load histdata
  load(file.path(path$data, "histdata.rda"))
  
  
  # Data selection ----------------------------------------------------------
  
  # Get list of unique api #s and what counties they are located in
  base <- sqldf("select distinct p_api, w_county, h_well_type from p")
  names(base) <- c("p_api", "w_county", "hwt")
  
  # Merge with histdata
  base <- merge(x = histdata, y = base, by.x = "h_api", by.y = "p_api", all.x = T)
  
  # Drop any wells not in Uintah or Duchesne county (i.e. w_county == NA) and
  # select only desired columns
  base <- subset(base,
                 subset = (!is.na(w_county) &
                             !is.na(h_wellstatus)),
                 select = c("h_api", "h_work_type", "h_compl_date", "h_work_compl",
                            "h_well_type", "h_first_prod", "h_wellstatus", "hwt"))
  
  # Round to floor all dates
  base$h_compl_date <- as.Date(as.yearmon(base$h_compl_date))
  base$h_work_compl <- as.Date(as.yearmon(base$h_work_compl))
  base$h_first_prod <- as.Date(as.yearmon(base$h_first_prod))
  
  # Get population of wells with (1) work type == DRILLING that (2) were
  # originally oil/gas wells and finally (3) were drilled within the specified
  # time period
  drill <- subset(base,
                  subset = (h_work_type == "DRILL" &
                              h_compl_date >= tstart &
                              h_compl_date <= tstop &
                              h_wellstatus == "P" &
                              (hwt == "OW" |
                                 hwt == "GW")),
                  select = c("h_api", "h_compl_date", "hwt"))
  
  # Get subset of oil wells which were reworked
  re <- subset(base,
               subset = ((h_work_type == "RECOMP" |
                            h_work_type == "REPERF") &
                           h_wellstatus == "P"),
               select = c("h_api", "h_work_compl", "hwt"))
  
  
  # Calculation -------------------------------------------------------------
  
  # Step 1: Get # of wells at least "n" timesteps old
  
  # Make sequence of dates from tstart to tstop
  tsteps <- seq(from = tstart, to = tstop, by = "months")
  
  # Predefine vectors for well counts
  wc.ow <- rep(0, times = length(tsteps))
  wc.gw <- wc.ow
  
  # Get # of wells drilled in each month
  for (i in 1:length(tsteps)) {
    wc.ow[i] <- length(which(drill$h_compl_date[drill$hwt == "OW"] == tsteps[i]))
    wc.gw[i] <- length(which(drill$h_compl_date[drill$hwt == "GW"] == tsteps[i]))
  }
  
  # Take reverse of cumulative sum of well count vectors
  wc.ow <- rev(cumsum(wc.ow))
  wc.gw <- rev(cumsum(wc.gw))
  
  # Step 2: Get # of months into well-life that rework occurred
  
  # Merge re with drill, drop NAs
  re <- na.omit(merge(x = re, y = drill[,c("h_api", "h_compl_date")], by.x = "h_api", by.y = "h_api", all.x = T))
  
  # Calculate time difference between drilling and rework
  re$dt <- with(re, round(as.numeric(difftime(h_work_compl, h_compl_date, units = "days"))*(12/365.25)))
  
  # Predefine vectors for well counts
  re.ow <- rep(0, times = length(tsteps))
  re.gw <- re.ow
  
  # Get # of wells reworked per time step since well is drilled
  for (i in 1:length(tsteps)) {
    re.ow[i] <- length(which(re$dt[re$hwt == "OW"] == i))
    re.gw[i] <- length(which(re$dt[re$hwt == "GW"] == i))
  }
  
  # Step 3: Divide # of reworks at time step n by number of wells at time step n,
  # then sum together to get CDF
  cdf.rework <- data.frame(month = 1:length(tsteps),
                           oil =   cumsum(re.ow/wc.ow),
                           gas =   cumsum(re.gw/wc.gw),
                           wc.ow,
                           wc.gw,
                           re.ow,
                           re.gw)
  
  # Step 4: It's possible to have a CDF > 1 if the population of wells at long 
  # times becomes small and on the same order of magnitude as the rework count. 
  # To prevent this, drop any data points beyond which the number of wells in 
  # the well count population is below a specified threshold. Find the last row
  # at which both the oil and gas well counts are above this threshold - that
  # will become the cutoff point
  cutoff <- length(which(cdf.rework$wc.ow > wc.min & cdf.rework$wc.gw > wc.min))
  
  # Drop any rows after cutoff
  cdf.rework <- cdf.rework[1:cutoff,]
  
  
  # Export results ----------------------------------------------------------
  
  save(file = file.path(path$data, paste("rework_", ver, ".rda", sep = "")),
       list = c("cdf.rework"))
}