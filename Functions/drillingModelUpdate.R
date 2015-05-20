# Function Info -----------------------------------------------------------
# Name:      drillingModelUpdate.R (Drilling Schedule Model Fitting)
# Author(s): Jon Wilkey, Michael Hogue
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# p - DOGM databse data.frame

# min.depth - Minimum well depth, used as subsetting criteria

# tstart - Starting date for wells to be included in analysis

# tstop - Stopping date for wells to be included in analysis

# ver - Version number for file naming of exported data.frames

# eia.hp - EIA historical energy prices

# twindow - length of time window (i.e. number of months) for rolling drill
# model fit


# Outputs -----------------------------------------------------------------

# drillModel - lm() object containing fit of # of wells drilled (OW, GW, or D)
# in response to oil and gas prices.


# Description -------------------------------------------------------------

# This function fits the drilling history in the Basin within the specified time
# period to the EIA historical oil and gas prices according to the following
# function:

# W_n = a * OP_n + b * GP_n + c * W_n-1 + d

# where W is the wells drilled (oil, gas, or dry), n is the timestep (in 
# months), OP is first purchase price (FFP) of oil in Utah ($/bbl, inflation 
# adjusted to the date associated with "cpi" option input), GP is FFP of gas in
# Utah ($/MCF, also inflation adjusted), and all other terms are fitted 
# coefficients.


# Function ----------------------------------------------------------------

drillingModelUpdate <- function(path, p, min.depth, tstart, tstop, ver, eia.hp,
                                twindow) {
  
  # Determine number of wells drilled each month ------------------------------
  
  # Add column 'drill_month' as truncated to month version of h_first_prod
  p$drill_month <- as.yearmon(p[,"h_first_prod"])
  
  # Create dataframe containing dates of 1st production for each unique APD #
  # and the field it is located in
  well <- sqldf("select distinct(p_api), drill_month, h_well_type, h_td_md
                from p")
  
  # Drop NA observations
  well <- na.omit(well)
  
  # Only wells (1) inside price history timeframe (and prior month), (2) with
  # depths > 0, and (3) that were drilled with the intention of being producing
  # wells (i.e. oil wells, gas wells, or dry wells).
  well <- subset(well, subset = (drill_month >= (as.yearmon(tstart)-1/12) &
                                 drill_month <= as.yearmon(tstop) &
                                 h_td_md > min.depth &
                                 (h_well_type == "OW" |
                                  h_well_type == "GW" |
                                  h_well_type == "D")))
  
  # Determine total number of wells drilled each year
  drill <- sqldf("select drill_month, count(p_api)
                 from well
                 group by drill_month")
  
  m <- merge(x = eia.hp, y = drill,
             by.x = "month", by.y = "drill_month",
             all = TRUE)
  
  # Save two vectors for (1) wells drilled in prior year (prior) and (2) wells
  # drilled in current year (wells)
  prior <- m[(1:(nrow(m)-1)),4]
  wells <- m[(2:nrow(m)),4]
  
  
  # Fit drilling model ------------------------------------------------------
  
  # Create data.frame with all the data need to run lm()
  analysis <- data.frame(as.Date(eia.hp[,1]), wells, prior, eia.hp[,c(2,3)])
  names(analysis) <- c("month", "wells", "prior", "OP", "GP")
  
  # Drop NA values
  analysis <- na.omit(analysis)
  
  # Make copy for export
  drillModelData <- analysis
  
  # Run lm()
  drillModel <- lm(formula = (wells ~ OP + GP + prior),
                   data = analysis)
  
  # Fit rolling drilling model ----------------------------------------------
  
  # Make monthly time sequence tsteps
  tsteps <- seq(from = tstart, to = tstop, by = "months")
  
  # Calculate number of windows in time sequence
  nwin <- (length(tsteps)-twindow+1)
  
  # Make data.frame for storing coefficients
  dmw <- matrix(0, nrow = nwin, ncol = 5)
  
  # For each nwin period
  for (i in 1:nwin) {
    
    # Fit model to time subset
    temp <- lm(formula = (wells ~ OP + GP + prior),
               data = analysis[which(as.Date(analysis$month) >= tsteps[i] &
                                     as.Date(analysis$month) <= tsteps[i+twindow-1]),])
    
    # Extract coefficients and R^2 value
    dmw[i,] <- c(as.numeric(coef(temp)), summary(temp)$r.squared)
  }
  
  # Change type to data.frame and name columns
  drillModelWindow <- data.frame(a =  dmw[,2],
                                 b =  dmw[,3],
                                 c =  dmw[,4],
                                 d =  dmw[,1],
                                 R2 = dmw[,5])
  
  
  # Export fitted model -----------------------------------------------------
  
  save(file = file.path(path$data,
                        paste("drillModel_", ver, ".rda", sep = "")),
       list = c("drillModel",
                "drillModelWindow",
                "drillModelData"))
}