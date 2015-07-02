# EIA 2014 AEO forecast plotting

# Import data from *.csv files
oil <- read.csv("D:/Dropbox/CLEAR/EIA AEO Petroleum Data/EIA AEO 2014 Oil Data.csv")
gas <- read.csv("D:/Dropbox/CLEAR/EIA AEO Petroleum Data/EIA AEO 2014 Gas Data.csv")

# Plot prices
pdf(file.path(path$plot, "EIA 2014 AEO Rocky Mountain wellhead oil prices.pdf"))

plot(price~year, oil,
     type = "b",
     #lty = 2,
     xlab = "Year",
     ylab = "Wellhead Oil Price (2012 USD per bbl)",
     main = "EIA 2014 AEO Rocky Mountain Wellhead Oil Price Forecast")
grid(lty = 1)

dev.off()

pdf(file.path(path$plot, "EIA 2014 AEO Rocky Mountain wellhead gas prices.pdf"))

plot(price~year, gas,
     type = "b",
     #lty = 2,
     xlab = "Year",
     ylab = "Wellhead Gas Price (2012 USD per MCF)",
     main = "EIA 2014 AEO Rocky Mountain Wellhead Gas Price Forecast")
grid(lty = 1)

dev.off()