# Function Info -----------------------------------------------------------
# Name:      drillingModel.R (Drilling Schedule Model Fitting)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# p - DOGM databse data.frame

# tsteps - Range of time steps in modeling period

# field - Vector of field numbers to be considered individually

# min.depth - Minimum well depth, used as subsetting criteria

# version - Version number for file naming of exported data.frames


# Outputs -----------------------------------------------------------------

# blah


# Description -------------------------------------------------------------

# blah


# Function ----------------------------------------------------------------
drillingModel <- function(path, p, EP.CPI.basis, cpi, min.depth, version) {}


# Load historical wellhead price data -------------------------------------

# Make a *.csv file with the following format: [year, oilprice, gasprice]. The 
# first column is the year associated with each row of the price data, the 
# second column is the oil price at the wellhead from the following source [1] 
# listed below in EP.CPI.basis real dollars per bbl, and the last column is the 
# natural gas wellhead price in the same real dollars per MMBtu from source [2].
# Save the file as "EIA_HistPrices.csv" and place it in your raw data file path.
# Sources:
# [1] http://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=pet&s=f004049__3&f=a
# [2] http://www.eia.gov/dnav/ng/hist/na1140_sut_3a.htm

# Load the EIA_HistPrices.csv file and rename columns
eia.hp <- read.csv(file.path(path$raw, "EIA_HistPricesMon.csv"))
names(eia.hp) <- c("year", "OP", "GP")

# Drop years with incomplete price information
eia.hp <- na.omit(eia.hp)

# Adjust to models CPI-year real dollars
eia.hp$OP <- inf_adj(price = eia.hp$OP, index = EP.CPI.basis, basis = cpi)
eia.hp$GP <- inf_adj(price = eia.hp$GP, index = EP.CPI.basis, basis = cpi)


# Determine number of wells drilled per year ------------------------------

# # Add column 'drill_yr' as truncated-to-year version of h_first_prod
# p$drill_yr <- as.numeric(floor(as.yearmon(p[,"h_first_prod"])))

# For monthly data
p$drill_yr <- as.yearmon(p[,"h_first_prod"])
eia.hp$year <- as.yearmon(eia.hp$year)

# Create dataframe containing dates of 1st production for each unique APD # and
# the field it is located in
well <- sqldf("select distinct(p_api), drill_yr, h_well_type, h_td_md
              from p")

# Drop NA observations
well <- na.omit(well)

# Only wells (1) inside price history timeframe (and prior year), (2) with
# depths > 0, and (3) that were drilled with the intention of being producing
# wells (i.e. oil wells, gas wells, or dry wells).
well <- subset(well, subset = (drill_yr >= (min(eia.hp$year)-1/12) &
                                 drill_yr <= max(eia.hp$year) &
                                 h_td_md > min.depth &
                                 (h_well_type == "OW" |
                                  h_well_type == "GW" |
                                  h_well_type == "D")))

# Determine total number of wells drilled each year
drill <- sqldf("select drill_yr, count(p_api) from well group by drill_yr")

m <- merge(x = eia.hp, y = drill, by.x = "year", by.y = "drill_yr", all = TRUE)

# Save two vectors for (1) wells drilled in prior year (prior) and (2) wells
# drilled in current year (wells)
prior <- m[(1:(nrow(m)-1)),4]
wells <- m[(2:nrow(m)),4]


# Fit drilling model ------------------------------------------------------

# Create data.frame with all the data need to run lm()
analysis <- data.frame(as.Date(eia.hp[,1]), wells, prior, eia.hp[,c(2,3)])
names(analysis) <- c("year", "wells", "prior", "OP", "GP")

# Drop NA values
analysis <- na.omit(analysis)


#cutoff <- as.Date("1977-07-01")
# Run lm()
drillModel <- lm(formula = (wells ~ OP + GP + prior),
                 data = analysis)#,
                 #subset = year >= cutoff)

# Plot - Uncomment if plot is desired
with(analysis,#[analysis$year >= cutoff,],
     plot(year, wells,
          type = "l",
          xlab = "Year",
          ylab = "Total Wells Drilled (oil, gas, or dry)",
          main = "Drilling Schedule Model")
)
with(analysis,#[analysis$year >= cutoff,],
     lines(year, fitted(drillModel),
           col = "red")
)
mtext(expression(W==a%.%OP+b%.%GP+c%.%W[n-1]+d))
legend("topleft",
       c("Actual", "Fit"),
       lty = c(1,1),
       #pch = c(1,NA),
       col = c("black","red"))


# Export fitted model -----------------------------------------------------

save(file.path(path$data,
               paste("drillModel.rda") ))
