# Function Info -----------------------------------------------------------
# Name:      EIApriceUpdate.R (EIA energy price history update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - list object containing directory paths for file I/O

# EP.CPI.basis - EIA Historical Energy Prices CPI Basis (i.e. the CPI index
# value for the year to which all oil/gas prices in the EIA_HistPrice.csv file
# have been adjusted to).

# cpi - CPI basis for model

# ver - Version number for file naming of exported data.frames

# cf.MCF.to.MMBtu - conversion factor for switching from MCF of gas to MMBtu of
# gas

# Outputs -----------------------------------------------------------------

# eia.hp - data.frame of EIA historical energy prices with observations "month"
# for time index, "OP" for first purchase price (FFP) of oil in Utah ($/bbl,
# inflation adjusted to the date associated with "cpi" option input), and "GP"
# for FFP of gas in Utah ($/MCF, also inflation adjusted)


# Description -------------------------------------------------------------

# This function loads a previously generated *.csv file with historical energy
# prices, formats them for use with other parts of the model, and saves them as
# a data.frame in an *.rda file.


# Function ---------------------------------------------------------------- 
EIApriceUpdate <- function(path, EP.CPI.basis, cpi, ver, cf.MCF.to.MMBtu) {
    
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
  
  # Example spreadsheet (see worksheet "CSV Export - Monthly):
  # https://docs.google.com/spreadsheets/d/1S1M6RD3QXHewViG-7stioRxxzDDZvSKBHUe4YEVH-CU/edit?usp=sharing
  
  # Load the EIA_HistPrices.csv file and rename columns
  eia.hp <- read.csv(file.path(path$raw, "EIA_HistPrices.csv"))
  names(eia.hp) <- c("month", "OP", "GP")
  
  # Drop years with incomplete price information (safe guard, source *.csv file
  # should be complete).
  eia.hp <- na.omit(eia.hp)
  
  # Adjust to models CPI-year real dollars
  eia.hp$OP <- inf_adj(price = eia.hp$OP, index = EP.CPI.basis, basis = cpi)
  eia.hp$GP <- inf_adj(price = eia.hp$GP, index = EP.CPI.basis, basis = cpi)
  
  # Adjust gas prices from $/MMBtu basis to $/MCF basis using conversion factor
  eia.hp$GP <- eia.hp$GP/cf.MCF.to.MMBtu
  
  # Truncate dates in eia.hp to months
  eia.hp$month <- as.yearmon(eia.hp$month)
  
  # Save result
  save(file = file.path(path$data, paste("EIAprices_", ver, ".rda", sep = "")),
       list = c("eia.hp"))
}