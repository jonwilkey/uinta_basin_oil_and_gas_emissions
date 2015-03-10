# Function Info -----------------------------------------------------------
# Name:      waterUpdate.R (Water balance data analysis update function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - File directory

# tstart - Start date of analysis

# tstop <- Stop date of analysis

# p <- DOGM production database, subsetted to include only wells located in
# Uintah and Duchesne counties

# xq <- vector of quantiles at which to estimate the CDF

# f_mud <- Assumed volume ratio of (water used to mix with drilling mud) /
# (volume of borehole)

# f_cem <- Water to cement ratio in (gal/sack)

# rcut.pw.oil - Maximum ratio of produced water to oil production to be included
# in CDF for produced water from oil wells

# rcut.pw.gas - Maximum ratio of produced water to gas production to be included
# in CDF for produced water from gas wells

# rcut.disp - Maximum ratio of disposal water to produced water to be included
# in CDF for disposal water

# ver <- File version number


# Outputs -----------------------------------------------------------------

# cdf.water - CDF for produced water (by well type), water disposed of via
# injection wells, water injected for water flooding, water disposed of via
# evaporation ponds and fracking water usage (by well type)

# water.lm - linear regression model object for water usage in drilling (i.e.
# mud mixing and cementing)


# Description -------------------------------------------------------------

# This function loads *.csv files from a variety of sources on various 
# components of the water balance for conv. oil and gas production, analyzes the
# loaded data, and generates CDF and regression models for each term. The water
# balance equations used are:

# [1] (recycled) = (produced) - (disposal well + evaporated)
# [2] (water in) = (drilling + fracking + flooding) - (recycled)
# [3] (water intensity) = (water in) / (oil produced)

# The sources for information on each terms are:

# a) produced: from DOGM production database file

# b) disposal well: from DOGM online data search:
#    Well data: https://oilgas.ogm.utah.gov/Data_Center/LiveData_Search/inj_well_lookup.cfm
#    Disposal volumes: https://oilgas.ogm.utah.gov/Data_Center/LiveData_Search/inj_well_data_lookup.htm

# c) evaporated: from DOGM evaporation report data (received from Jean Sweet):
#    https://docs.google.com/spreadsheets/d/1B5q3zcjQEfWbCq7jsbFC6DLYWj7n26vafGoyI8zGq4k/edit?usp=sharing

# d) recycled: calculated via Eq.[1]

# e) drilling: calculated based on well report files analyzed here:
#    https://docs.google.com/spreadsheets/d/1hDPosxBltuMAM-H9R8PfU4g8hQmZJbKzCiGle9yh6eA/edit?usp=sharing

# f) fracking: from data collected on fracfocus.org:
#    https://docs.google.com/spreadsheets/d/1B5q3zcjQEfWbCq7jsbFC6DLYWj7n26vafGoyI8zGq4k/edit?usp=sharing

# g) flooding: from DOGM online data search:
#    Project numbers: https://oilgas.ogm.utah.gov/Data_Center/LiveData_Search/inj_project_lookup.cfm
#    Project volumes: https://oilgas.ogm.utah.gov/Data_Center/LiveData_Search/inj_proj_data.htm

# h) water in: calculated via Eq.[2]

# i) oil produced: from DOGM production database file

# j) water intensity: calculatd via Eq.[3]


# Function ----------------------------------------------------------------

waterUpdate <- function(path, p, tstart, tstop, xq, f_mud, f_cem, rcut.pw.oil,
                        rcut.pw.gas, rcut.disp, ver) {
  
  # Internal values - uncomment to debug ------------------------------------
  
#   tstart <- opt$tstart
#   tstop <-  opt$tstop
#   xq <-     opt$xq
#   f_mud <-  opt$f_mud
#   f_cem <-  opt$f_cem
  
  
  # Load required data files ------------------------------------------------
  
  # Read in *.csv files
  uic.projects <- read.csv(file.path(path$raw, "project.csv"), strip.white = TRUE)           # UIC Projects
  uic.wells <-    read.csv(file.path(path$raw, "inj_well.csv"), strip.white = TRUE)          # Injection Well info
  v <-            read.csv(file.path(path$raw, "inj_proj_all.csv"), strip.white = TRUE)      # Project Volumes
  d <-            read.csv(file.path(path$raw, "inj_well_data_all.csv"), strip.white = TRUE) # Disposal Well Volumes
  fw <-           read.csv(file.path(path$raw, "frackWater.csv"))                            # Fracking water consumption from fracfocus.org
  evap <-         read.csv(file.path(path$raw, "evapPond.csv"))                              # Evaporation pond quarterly data from DOGM
  dw <-           read.csv(file.path(path$raw, "drillWater.csv"))                            # Drilling water usage for mud and cement from DOGM wellfiles
  
  # Load required *.rda files
  load(file.path(path$data, "fieldnames.rda")) # Field name / number list
  
  
  # Preprocess input data files ---------------------------------------------
  
  # Covert columns in *.csv files
  uic.wells$Application.Date <- as.Date(uic.wells$Application.Date, "%m/%d/%Y")
  uic.wells$Next.MIT <-         as.Date(uic.wells$Next.MIT, "%m/%d/%Y")
  v$Report.Date <-              as.Date(as.yearmon(v$Report.Date, "%m/%d/%Y"))
  d$Report.Date <-              as.Date(as.yearmon(d$Report.Date, "%m/%d/%Y"))
  evap$date <-                  as.Date(evap$date)
  
  # Adjust fw
  names(fw) <- c("api", "field", "wellType", "depth", "duration", "water", "vdepth", "operator")
  
  
  # Process data - Water Injected as Time Series ----------------------------
  
  # Select distinct project numbers located in Uintah and Duchesne counties.
  project.num <- na.omit(unique(uic.wells$Project.Unit.Number[which(uic.wells$County == "UINTAH" | uic.wells$County == "DUCHESNE")]))
  
  # Get row indices of v which have the same project numbers as any of the unique
  # project numbers identified in project.num
  ind <- NULL
  for (i in 1:length(project.num)) {
    ind <- c(ind, which(v$Project.Number == project.num[i]))
  }
  
  # Finally, aggregate water injection volumes
  inj <- aggregate(Volume.Liquid ~ Report.Date, data = v[ind,], sum)
  
  
  # Process data - Water Disposal as Time Series ----------------------------
  
  # Disposal data is listed by API # in xx-yyy-zzzzz format where xx is state 
  # number, yyy is county number, and zzzzz is the individual well number. So use 
  # substr() function to find row indices where yyy == 013 (Duchesne county) or
  # yyy == 047 (Uintah county).
  
  ind <- which(substr(d$API.Well.Number, 4, 6) == "013" |
                 substr(d$API.Well.Number, 4, 6) == "047")
  
  # Get sum of disposal volumes from all wells in "ind" by date
  disp <- aggregate(Volume.Liquid ~ Report.Date, d[ind,], sum)
  
  
  # Process data - Water Produced as Time Series ----------------------------
  
  # Determine time series for water production from p based on well type
  prod.ow <- sqldf("select p_rpt_period, sum(p_water_prod)
                   from p
                   where h_well_type = 'OW'
                   group by p_rpt_period")
  
  prod.gw <- sqldf("select p_rpt_period, sum(p_water_prod)
                   from p
                   where h_well_type = 'GW'
                   group by p_rpt_period")
  
  
  # Process data - Average Well Age as Time Series --------------------------
  
  # Use SQL statement to determine average well age
  age <- sqldf("select p_rpt_period, avg(time)
               from p
               group by p_rpt_period")
  
  
  # Process data - Oil production as Time Series ----------------------------
  
  # Use SQL statement to determine total oil production as series of time
  oil <- sqldf("select p_rpt_period, sum(p_oil_prod)
               from p
               group by p_rpt_period")
  
  
  # Process data - Gas production as Time Series ----------------------------
  
  # Use SQL statement to determine total oil production as series of time
  gas <- sqldf("select p_rpt_period, sum(p_gas_prod)
               from p
               group by p_rpt_period")
  
  
  # Process Data - Drilling water -------------------------------------------
  
  # Values in dw data.frame for drilling usage water contain the following 
  # information:
  
  # 1) d - Well depth
  # 2) v - Borehole volume (in gal)
  # 3) n - # of sacks of cement used
  # 4) Well type
  
  # The total water usage for drilling is then:
  
  # DW = f_mud * v + f_cem * n
  
  # where DW is that total drilling water usage, f_mud represents the amount of 
  # water consumed in mixing drilling mud expressed as a fraction of the borehole 
  # volume, and f_cem is the ratio of gal of water used per sack of cement. Using
  # the input values for f_mud and f_cem, calculate total water usage and convert
  # to bbl.
  
  dw$water <- (f_mud*dw$borehole+f_cem*dw$cement)/42
  
  
  # Subset to specified date range ------------------------------------------
  
  # Make date sequence
  tsteps <- data.frame(month = seq(from = tstart, to = tstop, by = "months"))
  
  # Merge all observations together into one data.frame
  wd <- merge(x = tsteps, y = inj, by.x = "month",     by.y = "Report.Date", all.x = T)
  wd <- merge(x = wd,     y = disp, by.x = "month",    by.y = "Report.Date", all.x = T)
  wd <- merge(x = wd,     y = prod.ow, by.x = "month", by.y = "p_rpt_period", all.x = T)
  wd <- merge(x = wd,     y = prod.gw, by.x = "month", by.y = "p_rpt_period", all.x = T)
  wd <- merge(x = wd,     y = age, by.x = "month",     by.y = "p_rpt_period", all.x = T)
  wd <- merge(x = wd,     y = oil, by.x = "month",     by.y = "p_rpt_period", all.x = T)
  wd <- merge(x = wd,     y = gas, by.x = "month",     by.y = "p_rpt_period", all.x = T)
  
  # Rename columns
  names(wd) <- c("month", "inj", "disp", "pw.ow", "pw.gw", "age", "oil", "gas")
  
  # Add in total produced water column
  wd$pw.all <- wd$pw.ow+wd$pw.gw
  
  # To get quarterly values
  wdQ <- zoo(wd[,2:9], order.by = wd[,1])
  wdQ <- aggregate(wdQ, as.yearqtr, sum)
  #wdQ <- data.frame(qtr = as.Date(index(wdQ)), coredata(wdQ))
  
  # Merge in evaporation volumes
  evap <- zoo(evap[,2], order.by = as.yearqtr(evap[,1]))
  wdQ <- merge(wdQ, evap)
  
  
  # Fit all terms in water balance equation ---------------------------------
  
  # -- Produced water ---  
  # Get produced water to oil/gas ratio based on cumulative production reported 
  # in DOGM welldata database for wells (1) located within the Uinta Basin (i.e.
  # data frame p), (2) drilled within the time frame of interest, (3) that have
  # a nonzero cumulative oil/gas production, and (4) that have any value of
  # cumulative water production other than NA.
  prod.r <- sqldf("select distinct p_api, w_totcum_gas, w_totcum_oil, w_totcum_wtr, h_first_prod, h_well_type
                  from p")
  
  # Subset for oil wells
  prod.r.ow <- subset(prod.r, subset = (h_well_type == "OW" &
                                        w_totcum_oil > 0 &
                                        !is.na(w_totcum_wtr) &
                                        h_first_prod >= tstart &
                                        h_first_prod <= tstop))
  
  # Subset for gas wells
  prod.r.gw <- subset(prod.r, subset = (h_well_type == "GW" &
                                          w_totcum_gas > 0 &
                                          !is.na(w_totcum_wtr) &
                                          h_first_prod >= tstart &
                                          h_first_prod <= tstop))
  
  # Calculate ratio
  prod.r.ow$r <- prod.r.ow$w_totcum_wtr/prod.r.ow$w_totcum_oil
  prod.r.gw$r <- prod.r.gw$w_totcum_wtr/prod.r.gw$w_totcum_gas
  
  # Since there is a long tail to produced water for both oil and gas wells, cut
  # any value which has an r value > 100 for oil and r value > 10 for gas wells
  prod.r.ow <- prod.r.ow[which(prod.r.ow$r <= rcut.pw.oil),]
  prod.r.gw <- prod.r.gw[which(prod.r.gw$r <= rcut.pw.gas),]
  
  # Calculate CDF for produced water and add to data.frame cdf.water
  cdf.water <- data.frame(cdf = xq,
                          pw.oil = CDFq(prod.r.ow$r, xq)[,1],
                          pw.gas = CDFq(prod.r.gw$r, xq)[,1])
  
  
  # -- Disposal well injection water --  
  # Get ratio of water injectd into disposal wells vs. produced water from all
  # wells
  disp.r <- wd$disp/wd$pw.all
  
  # Reject single outlining data point (~0.93)
  disp.r <- disp.r[which(disp.r < rcut.disp)]
  
  # Calculate CDF and add to cdf.water data frame
  cdf.water$disp <- CDFq(disp.r, xq)[,1]
  
  
  # -- Evaporation pond water --  
  # Calculate as fraction of produced water
  evapf <- wdQ$evap/wdQ$pw.all
  
  # Assuming a normal distribution .
  cdf.water$evap <- qnorm(xq,
                          mean = mean(evapf, na.rm = T),
                          sd = sd(evapf, na.rm = T))
  
  # Zero out any values in evap that are < 0
  if(min(cdf.water$evap) < 0) {
    temp <- which(cdf.water$evap < 0)
    cdf.water$evap[temp] <- 0
  }
  
  # Fracking Water - CDF
  # Select only entries from fracfocus.org data.frame which are not NA
  fw <- fw[which(!is.na(fw$water)),]
  
  # Drop the single row which has 1 gal of frac water usage
  fw <- fw[-which(fw$water == 1),]
  
  # Convert from gal to bbl
  fw$water <- fw$water/42
  
  # Get subsets for oil wells and gas wells
  fw.ow <- fw[which(fw$wellType == "OW"),]
  fw.gw <- fw[which(fw$wellType == "GW"),]
  
  # Get CDFs
  cdf.water$fw.ow <- CDFq(fw.ow$water, xq)[,1]
  cdf.water$fw.gw <- CDFq(fw.gw$water, xq)[,1]
  
  
  # -- Flooding Water --
    # Calculate ratio of water injected for flooding vs. oil production from wd
  inj.r <- wd$inj/wd$oil
  
  # Calculate CDF and add to cdf.water
  cdf.water$inj <- CDFq(inj.r, xq)[,1]
  
  # -- Drilling Water --
  # Use linear regression to fit as function of well depth
  dw.lm <- lm(water~depth, dw)
  
  
  # Data export -------------------------------------------------------------
  
  # Make list out of all the different water regression models
  water.lm <- NULL
  water.lm$dw.lm <- dw.lm
  
  # Save to file
  save(file=file.path(path$data,
                      paste("water_models_", ver, ".rda", sep = "")),
       list=c("cdf.water",
              "water.lm"))
}