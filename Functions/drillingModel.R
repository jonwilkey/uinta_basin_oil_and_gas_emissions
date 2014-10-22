# Function Info -----------------------------------------------------------
# Name:      drillingModel.R (Drilling Schedule Model Fitting)
# Author(s): Jon Wilkey, Michael Hogue
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# p - DOGM databse data.frame

# EP.CPI.basis - EIA Historical Energy Prices CPI Basis (i.e. the CPI index
# value for the year to which all oil/gas prices in the EIA_HistPrice.csv file
# have been adjusted to).

# cpi - CPI basis for model

# min.depth - Minimum well depth, used as subsetting criteria

# version - Version number for file naming of exported data.frames


# Outputs -----------------------------------------------------------------

# drillModel - lm() object containing fit of # of wells drilled (OW, GW, or D)
# in response to oil and gas prices.


# Description -------------------------------------------------------------

# This function loads a *.csv file containing prior oil and gas prices and fits
# the drilling history in the Basin to those prices according to the following
# function:

# W_n = a * OP_n + b * GP_n + c * W_n-1 + d

# where W is the wells drilled (oil, gas, or dry), n is the timestep (in 
# months), OP is first purchase price (FFP) of oil in Utah ($/bbl, inflation 
# adjusted to the date associated with "cpi" input), GP is FFP of gas in Utah 
# ($/MMbtu, also inflation adjusted), and all other terms are fitted
# coefficients.


# Function ----------------------------------------------------------------

drillingModel <- function(path, p, EP.CPI.basis, cpi, min.depth, version) {
  # Load historical wellhead price data -------------------------------------
  
  # Make a *.csv file with the following format: [month, oilprice, gasprice].
  # The first column is the month associated with each row of the price data,
  # the second column is the oil price at the wellhead from the following source
  # [1] listed below in EP.CPI.basis real dollars per bbl, and the last column
  # is the natural gas wellhead price in the same real dollars per MMBtu from
  # source [2]. Save the file as "EIA_HistPrices.csv" and place it in your raw
  # data file path.
  
  # Sources:
  # [1] http://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=f004049__3&f=a
  # [2] http://www.eia.gov/dnav/ng/hist/na1140_sut_3a.htm
  
  # Load the EIA_HistPrices.csv file and rename columns
  eia.hp <- read.csv(file.path(path$raw, "EIA_HistPrices.csv"))
  names(eia.hp) <- c("month", "OP", "GP")
  
  # Drop years with incomplete price information (safe guard, source *.csv file
  # should be complete).
  eia.hp <- na.omit(eia.hp)
  
  # Adjust to models CPI-year real dollars
  eia.hp$OP <- inf_adj(price = eia.hp$OP, index = EP.CPI.basis, basis = cpi)
  eia.hp$GP <- inf_adj(price = eia.hp$GP, index = EP.CPI.basis, basis = cpi)
  
  
  # Determine number of wells drilled each month ------------------------------
  
  # Add column 'drill_month' as truncated to month version of h_first_prod.
  # Similarly, truncate dates in eia.hp to months.
  p$drill_month <- as.yearmon(p[,"h_first_prod"])
  eia.hp$month <- as.yearmon(eia.hp$month)
  
  # Create dataframe containing dates of 1st production for each unique APD #
  # and the field it is located in
  well <- sqldf("select distinct(p_api), drill_month, h_well_type, h_td_md
                from p")
  
  # Drop NA observations
  well <- na.omit(well)
  
  # Only wells (1) inside price history timeframe (and prior month), (2) with
  # depths > 0, and (3) that were drilled with the intention of being producing
  # wells (i.e. oil wells, gas wells, or dry wells).
  well <- subset(well, subset = (drill_month >= (min(eia.hp$month)-1/12) &
                                   drill_month <= max(eia.hp$month) &
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
  
  # Run lm()
  drillModel <- lm(formula = (wells ~ OP + GP + prior),
                   data = analysis)
  
  # # Plot - Uncomment if plot is desired
  # with(analysis,
  #      plot(month, wells,
  #           type = "l",
  #           xlab = "Year",
  #           ylab = "Total Wells Drilled (oil, gas, or dry)",
  #           main = "Drilling Schedule Model")
  # )
  # with(analysis,
  #      lines(month, fitted(drillModel),
  #            col = "red")
  # )
  # mtext(expression(W==a%.%OP+b%.%GP+c%.%W[n-1]+d))
  # legend("topleft",
  #        c("Actual", "Fit"),
  #        lty = c(1,1),
  #        col = c("black","red"))
  
  
  # Export fitted model -----------------------------------------------------
  
  save(file = file.path(path$data,
                        paste("drillModel_", version, ".rda", sep = "")),
       list = c("drillModel"))
}