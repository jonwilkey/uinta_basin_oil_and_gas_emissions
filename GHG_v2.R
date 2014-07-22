# Script Info -------------------------------------------------------------
# GHG_v2.R (Greenhouse gas emissions)
# Version 2
# 05/13/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1. Loads *.rda files, determines PDF & CDF for each stage of oil and gas
#     production, applies CDF in MC simulation based on total oil/gas prodution
#     curves.
# v2. Added more emission factors for oil production from Canadian + California
#     study.


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(work_root)


# Functions ---------------------------------------------------------------


# Libraries ---------------------------------------------------------------
library(MASS)


# Load data files ---------------------------------------------------------
# Load *.rda files
load(file.path(data_root, "prod_gas_fit.rda"))
load(file.path(data_root, "prod_oil_fit.rda"))
load(file.path(data_root, "schedule_ow.rda"))
load(file.path(data_root, "schedule_gw.rda"))

# Determine wells drilled in each time step
schedule.gw <- rowSums(schedule.gw)
schedule.ow <- rowSums(schedule.ow)

# Rename for brevity
gas <- prod.gas.fit
oil <- prod.oil.fit
remove(prod.gas.fit, prod.oil.fit)

# Define time series
t <- seq(from = as.Date("1999-01-01"),
         to = as.Date("2013-12-01"),
         by = "months")


# Define emissions factors - gas ------------------------------------------
# Well drilling/completion/workover - Shale gas/oil wells (1e3 kg GHG / well)
ef1 <- 483                # O'Sullivan (2012)
ef2 <- 1294               # Santoro et al (2011)
ef3 <- (762+1624)/2       # Jiang et al (2011), mid point of range
ef4 <- 645                # Stephenson et al (2011)
ef5 <- 1841*(416236/1e6)  # Burnham (2011), conversion from per gas to per well
ef6 <- 2038*(416236/1e6)  # Jiang et al (2011), conversion from per gas to per well
ef7 <- 1072               # O'Sullivan (2012), for tight oil
ef8 <- 3717               # EPA revised emissions factors
ef9 <- 2058               # EPA (2013)
ef.dcw <- c(ef1, ef2, ef3, ef4, ef5, ef6, ef7, ef8, ef9)

# Production (1e3 kg / 1e6 MCF gas)
ef1 <- 7725               # Burnham (2011) - conventional (?)
ef2 <- 2922               # Burnham (2011) - unconventional
ef3 <- 7200               # Jiang et al (2011)
ef4 <- 1897               # Canadian study - Canadian data
ef5 <- 1481               # Canadian study - U.S. data
ef.prd <- c(ef1, ef2, ef3, ef4, ef5)/1e6

# Processing (1e3 kg GHG / 1e6 MCF gas)
ef1 <- 600                # Burnham (2011)
ef2 <- 1994               # Jiang et al (2011)
ef3 <- 533                # Canadian study - Canadian data (sweet)
ef4 <- 2267               # Canadian study - Canadian data (sour)
ef.prc <- c(ef1, ef2, ef3, ef4)/1e6

# Transmission and Distribution (1e3 kg GHG / 1e6 MCF)
ef1 <- 1773               # Jiang et al (2011)
ef2 <- 48                 # Emission Inventory Report
ef.trs <- c(ef1, ef2)/1e6

# CH4 Emissions from Gas Production (as fraction of total production)
ef1 <- 0.0142             # Kirchgessner et al (1997)
ef2 <- (0.036+0.079)/2    # Howarth et al (2011)
ef3 <- 0.022              # USEPA
ef4 <- (0.0168+0.077)/2   # Petron et al (2012)
ef5 <- (0.062+0.117)/2    # Karion et al (2013)
ef6 <- 0.17               # Peischl et al (2013)
ef7 <- 0.0275             # Burnham et al (2011) - conv.
ef8 <- 0.0201             # Burnham et al (2011) - unconv.
ef.tot <- c(ef1, ef2, ef3, ef4, ef5, ef6, ef7, ef8)

# CH4 Emissions from Trans/Stor/Distr - Conv (as fraction of total production)
ef1 <- 0.012             # Hayhoe et al (2002)
ef2 <- 0.014             # Howarth et al (2011)
ef3 <- 0.016             # EPA (2011)
ef4 <- 0.013             # Hultman et al (2011)
ef5 <- 0.018             # Ventatesh et al (2011)
ef6 <- 0.020             # Burnham et al (2011)
ef7 <- 0.004             # Stephenson et al (2011)
ef8 <- 0.009             # Cathles et al (2012)
ef.trs.conv <- c(ef1, ef2, ef3, ef4, ef5, ef6, ef7, ef8)

# CH4 Emissions from Trans/Stor/Distr - Unconv (as fraction of total production)
ef1 <- 0.033             # Howarth et al (2011)
ef2 <- 0.030             # EPA (2011)
ef3 <- 0.020             # Jiang et al (2011)
ef4 <- 0.028             # Hultman et al (2011)
ef5 <- 0.013             # Burnham et al (2011)
ef6 <- 0.006             # Stephenson et al (2011)
ef7 <- 0.009             # Cathles et al (2012)
ef8 <- 0.040             # Petron et al (2012)
ef.trs.unconv <- c(ef1, ef2, ef3, ef4, ef5, ef6, ef7, ef8)


# Define emissions factors - oil ------------------------------------------
# Well drilling/completion/workover
# Same range as given in gas well section

# Oil production (10^3 kg GHG / bbl oil )
ef1 <- 1.69e-5            # Canadian study - conventional
ef2 <- 8.13e-5            # Canadian study - heavy
ef3 <- 4.69e-2            # California study - total CO2e
ef.prd.oil <- c(ef1, ef2, ef3)

# Oil transport (1e3 kg GHG / tanker truck)
# Well drilling/completion/workover - Shale gas/oil wells (1e3 kg GHG / well)
ef1 <- 8.38e-8            # Canadian study
ef.trs.oil <- c(ef1)
truck.capacity <- 200     # bbl of oil per truck, from UDOT study

# Emissions from transport by tanker (truck or rail)
truckloads <- round(oil/truck.capacity, 0)


# PDF from density() ------------------------------------------------------
# Density criteria
# Number of points
n = 10^3
# Lower limit
f <- 0;

# Density fits - Gas
pdf.dcw        <- density(ef.dcw, from = f, n = n)
pdf.prd        <- density(ef.prd, from = f, n = n)
pdf.prc        <- density(ef.prc, from = f, n = n)
pdf.trs        <- density(ef.trs, from = f, n = n)
pdf.tot        <- density(ef.tot, from = f, n = n)
pdf.trs.conv   <- density(ef.trs.conv, from = f, n = n)
pdf.trs.unconv <- density(ef.trs.unconv, from = f, n = n)

# Density fits - Oil
pdf.prd.oil    <- density(ef.prd.oil, from = f, n = n)
# Can't fit density to ef.trs.oil because need at least 2 datapoints


# PDF from regression to known distribution using fitdistr() --------------
# Gas
fit.dcw        <- fitdistr(ef.dcw, "normal")
fit.prd        <- fitdistr(ef.prd, "normal")
fit.prc        <- fitdistr(ef.prc, "normal")
fit.trs        <- fitdistr(ef.trs, "normal")
fit.tot        <- fitdistr(ef.tot, "normal")
fit.trs.conv   <- fitdistr(ef.trs.conv, "normal")
fit.trs.unconv <- fitdistr(ef.trs.unconv, "normal")

# Oil
fit.prd.oil    <- fitdistr(ef.prd.oil, "normal")
fit.trs.oil    <- c(ef.trs.oil[1], ef.trs.oil[1]*0.2)


# CDF from fitdistr() PDF - gas -------------------------------------------
# Generate probability densities from fits
y.dcw <- dnorm(x = pdf.dcw$x, mean = fit.dcw$estimate[1], sd = fit.dcw$estimate[2])
y.prd <- dnorm(x = pdf.prd$x, mean = fit.prd$estimate[1], sd = fit.prd$estimate[2])
y.prc <- dnorm(x = pdf.prc$x, mean = fit.prc$estimate[1], sd = fit.prc$estimate[2])
y.trs <- dnorm(x = pdf.trs$x, mean = fit.trs$estimate[1], sd = fit.trs$estimate[2])
y.tot <- dnorm(x = pdf.tot$x, mean = fit.tot$estimate[1], sd = fit.tot$estimate[2])
y.trs.conv <- dnorm(x = pdf.trs.conv$x, mean = fit.trs.conv$estimate[1], sd = fit.trs.conv$estimate[2])
y.trs.unconv <- dnorm(x = pdf.trs.unconv$x, mean = fit.trs.unconv$estimate[1], sd = fit.trs.unconv$estimate[2])

# Sum to CDF
cdf.dcw <- cumsum(y.dcw*diff(pdf.dcw$x[1:2]))
cdf.prd <- cumsum(y.prd*diff(pdf.prd$x[1:2]))
cdf.prc <- cumsum(y.prc*diff(pdf.prc$x[1:2]))
cdf.trs <- cumsum(y.trs*diff(pdf.trs$x[1:2]))
cdf.tot <- cumsum(y.tot*diff(pdf.tot$x[1:2]))
cdf.trs.conv <- cumsum(y.trs.conv*diff(pdf.trs.conv$x[1:2]))
cdf.trs.unconv <- cumsum(y.trs.unconv*diff(pdf.trs.unconv$x[1:2]))

# Normalize
cdf.dcw <- cdf.dcw/max(cdf.dcw)
cdf.prd <- cdf.prd/max(cdf.prd)
cdf.prc <- cdf.prc/max(cdf.prc)
cdf.trs <- cdf.trs/max(cdf.trs)
cdf.tot <- cdf.tot/max(cdf.tot)
cdf.trs.conv <- cdf.trs.conv/max(cdf.trs.conv)
cdf.trs.unconv <- cdf.trs.unconv/max(cdf.trs.unconv)


# CDF from fitdistr() PDF - oil -------------------------------------------
# Generate probability densities from fits
y.prd.oil <- dnorm(x = pdf.prd.oil$x,
                   mean = fit.prd.oil$estimate[1],
                   sd = fit.prd.oil$estimate[2])
x.trs.oil <- seq(from = f, to = 2*fit.trs.oil[1], length.out = n)
y.trs.oil <- dnorm(x = x.trs.oil,
                   mean = fit.trs.oil[1],
                   sd = fit.trs.oil[2])

# Sum to CDF
cdf.prd.oil <- cumsum(y.prd.oil*diff(pdf.prd.oil$x[1:2]))
cdf.trs.oil <- cumsum(y.trs.oil*diff(x.trs.oil[1:2]))

# Normalize
cdf.prd.oil <- cdf.prd.oil/max(cdf.prd.oil)
cdf.trs.oil <- cdf.trs.oil/max(cdf.trs.oil)


# Monte Carlo Simulation - Gas --------------------------------------------
# Number of iterations
n <- 10^5
# Matrix for results
EF <- matrix(0, nrow = length(gas), ncol = n)
PF <- EF

for (i in 1:n) {
  # Pick random numbers
  rn <- runif(6, min = 0, max = 1)
  
  # Use findInterval to pick index and assign value to each variable
  a <- pdf.dcw$x[findInterval(rn[1], c(0, cdf.dcw))]
  b <- pdf.prd$x[findInterval(rn[2], c(0, cdf.prd))]
  c <- pdf.prc$x[findInterval(rn[3], c(0, cdf.prc))]
  d <- pdf.trs$x[findInterval(rn[4], c(0, cdf.trs))]
  e <- pdf.tot$x[findInterval(rn[5], c(0, cdf.tot))]
  f <- pdf.trs.unconv$x[findInterval(rn[6], c(0, cdf.trs.unconv))]
  
  # Estimate emissions
  EF[,i] <- a*schedule.gw+(b+c+d)*gas
  PF[,i] <- (e+f)*gas
}


# Extract quantiles from simulation runs
EFquant <- matrix(0, nrow = length(gas), ncol = 11)
PFquant <- EFquant
for (i in 1:length(EFquant[,1])) {
  temp  <- quantile(EF[i,], c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0))
  temp1 <- quantile(PF[i,], c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0))
  for (j in 1:ncol(EFquant)) {
    EFquant[i,j] <- temp[j]
  }
  for (k in 1:ncol(PFquant)) {
    PFquant[i,k] <- temp1[k]
  }
}


# Monte Carlo Simulation - Oil --------------------------------------------
# Number of iterations
n <- 10^5
# Matrix for results
EF.oil <- matrix(0, nrow = length(oil), ncol = n)

for (i in 1:n) {
  # Pick random numbers
  rn <- runif(3, min = 0, max = 1)
  
  # Use findInterval to pick index and assign value to each variable
  a <- pdf.dcw$x[    findInterval(rn[1], c(0, cdf.dcw))]
  b <- pdf.prd.oil$x[findInterval(rn[2], c(0, cdf.prd.oil))]
  c <- x.trs.oil[    findInterval(rn[3], c(0, cdf.trs.oil))]
    
  # Estimate emissions
  EF.oil[,i] <- a*schedule.ow + b*oil + c*truckloads
}


# Extract quantiles from simulation runs
EFquant.oil <- matrix(0, nrow = length(oil), ncol = 11)
for (i in 1:length(EFquant.oil[,1])) {
  temp  <- quantile(EF.oil[i,], c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0))
  for (j in 1:ncol(EFquant.oil)) {
    EFquant.oil[i,j] <- temp[j]
  }
}


# Plots - MC simulation results -------------------------------------------
# Save plots to PDF file
pdf(file.path(plot_root, "GHG_v2 Results.pdf"))

linecolor <- rainbow(11)
# GHG Emissions - Gas
plot(t, EFquant[,1],
     type = "l",
     col = linecolor[1],
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     main = "GHG MC Simulation Results - Gas")
for (i in 2:ncol(EFquant)) {
  lines(t,EFquant[,i], col = linecolor[i])
}
legend("topleft",
       c("100%", "90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%", "0%"),
       ncol = 2, col = linecolor, lty = 1)

# GHG Emissions - Oil
plot(t, EFquant.oil[,1],
     type = "l",
     col = linecolor[1],
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     main = "GHG MC Simulation Results - Oil")
for (i in 2:ncol(EFquant.oil)) {
  lines(t,EFquant.oil[,i], col = linecolor[i])
}
legend("topleft",
       c("100%", "90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%", "0%"),
       ncol = 2, col = linecolor, lty = 1)

# CH4 Emissions
plot(t, PFquant[,1],
     type = "l",
     col = linecolor[1],
     xlab = "Time",
     ylab = "CH4 Emissions (MCF)",
     main = "CH4 MC Simulation Results - Gas")
for (i in 2:ncol(PFquant)) {
  lines(t,PFquant[,i], col = linecolor[i])
}
legend("topleft",
       c("100%", "90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%", "0%"),
       ncol = 2, col = linecolor, lty = 1)


# Plots - emissions from each step ----------------------------------------
# Total GHG Emissions from Gas Drilling -> Trans. - Gas
plot(t, max(ef.dcw)*schedule.gw+(max(ef.prd)+max(ef.prc)+max(ef.trs))*gas,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     ylim = c(0, 5e5),
     main = "Total GHG Emissions from Gas Drilling through Transmission")
lines(t, mean(ef.dcw)*schedule.gw+(mean(ef.prd)+mean(ef.prc)+mean(ef.trs))*gas, col = "black")
lines(t, min(ef.dcw)*schedule.gw+(min(ef.prd)+min(ef.prc)+min(ef.trs))*gas, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Drilling / Completion / Workover - Gas
plot(t, max(ef.dcw)*schedule.gw,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     ylim = c(0, 5e5),
     main = "GHG Emissions from Gas Well Drilling / Completion / Workover")
lines(t, mean(ef.dcw)*schedule.gw, col = "black")
lines(t, min(ef.dcw)*schedule.gw, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Production - Gas
plot(t, max(ef.prd)*gas,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     ylim = c(0, 5e5),
     main = "GHG Emissions from Gas Production")
lines(t, mean(ef.prd)*gas, col = "black")
lines(t, min(ef.prd)*gas, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Processing - Gas
plot(t, max(ef.prc)*gas,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     ylim = c(0, 5e5),
     main = "GHG Emissions from Gas Processing")
lines(t, mean(ef.prc)*gas, col = "black")
lines(t, min(ef.prc)*gas, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Transmission - Gas
plot(t, max(ef.trs)*gas,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     ylim = c(0, 5e5),
     main = "GHG Emissions from Gas Transmission & Distribution")
lines(t, mean(ef.trs)*gas, col = "black")
lines(t, min(ef.trs)*gas, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Total GHG Emissions from Gas Drilling -> Trans. - Oil
plot(t, max(ef.dcw)*schedule.ow+max(ef.prd.oil)*oil+max(ef.trs.oil)*truckloads,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     ylim = c(0, 2.5e5),
     main = "Total GHG Emissions from Oil Drilling through Transmission")
lines(t, mean(ef.dcw)*schedule.ow+mean(ef.prd.oil)*oil+mean(ef.trs.oil)*truckloads, col = "black")
lines(t, min(ef.dcw)*schedule.ow+min(ef.prd.oil)*oil+min(ef.trs.oil)*truckloads, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Drilling / Completion / Workover - Oil
plot(t, max(ef.dcw)*schedule.ow,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     ylim = c(0, 2.5e5),
     main = "GHG Emissions from Oil Well Drilling / Completion / Workover")
lines(t, mean(ef.dcw)*schedule.ow, col = "black")
lines(t, min(ef.dcw)*schedule.ow, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Production - Oil
plot(t, max(ef.prd.oil)*oil,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     ylim = c(0, 2.5e5),
     main = "GHG Emissions from Oil Production")
lines(t, mean(ef.prd.oil)*oil, col = "black")
lines(t, min(ef.prd.oil)*oil, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Transmission - Oil
plot(t, max(ef.trs.oil)*truckloads,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "GHG Emissions (1e3 kg CO2e)",
     ylim = c(0, 2.5e5),
     main = "GHG Emissions from Oil Transmission & Distribution")
lines(t, mean(ef.trs.oil)*truckloads, col = "black")
lines(t, min(ef.trs.oil)*truckloads, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Total CH4 Emissions from gas
plot(t, (max(ef.tot)+max(ef.trs.unconv))*gas,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "CH4 Emissions (MCF)",
     ylim = c(0, 5e6),
     main = "Total Fugitive CH4 Emissions from Gas Prod. & Trans.")
lines(t, (mean(ef.tot)+mean(ef.trs.unconv))*gas, col = "black")
lines(t, (min(ef.tot)+min(ef.trs.unconv))*gas, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# Total Fugitive CH4 Emissions
plot(t, max(ef.tot)*gas,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "CH4 Emissions (MCF)",
     ylim = c(0, 5e6),
     main = "Fugitive CH4 Emissions from Gas Production")
lines(t, mean(ef.tot)*gas, col = "black")
lines(t, min(ef.tot)*gas, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# CH4 Emissions from Gas Transmission & Distribution - Conventional
plot(t, max(ef.trs.conv)*gas,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "CH4 Emissions (MCF)",
     ylim = c(0, 5e6),
     main = "Fugitive CH4 Emissions from Conv. Gas Trans. & Distr.")
lines(t, mean(ef.trs.conv)*gas, col = "black")
lines(t, min(ef.trs.conv)*gas, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))

# CH4 Emissions from Gas Transmission & Distribution - Unconventional
plot(t, max(ef.trs.unconv)*gas,
     type = "l",
     col = "blue",
     xlab = "Time",
     ylab = "CH4 Emissions (MCF)",
     ylim = c(0, 5e6),
     main = "Fugitive CH4 Emissions from Unconv. Gas Trans. & Distr.")
lines(t, mean(ef.trs.unconv)*gas, col = "black")
lines(t, min(ef.trs.unconv)*gas, col = "red")
legend("topleft", c("Max", "Mean", "Min"), lty = 1, col = c("blue", "black", "red"))


# Plots - PDF from density() vs. fitdistr() -------------------------------
# Drilling / Completion / Workover
plot(pdf.dcw,
     xlab = "GHG Emissions (1e3 kg / well)",
     main = "GHG Emissions from Well Drilling / Completion / Workover")
rug(jitter(ef.dcw))
lines(pdf.dcw$x,
      dnorm(x = pdf.dcw$x, mean = fit.dcw$estimate[1], sd = fit.dcw$estimate[2]),
      col = "blue")
legend("topright",
       c("Density", "Normal"),
       lty = c(1,1),
       col = c("black", "blue"))

# Production - Gas
plot(pdf.prd,
     xlab = "GHG Emissions (1e3 kg / MCF Gas)",
     ylim = c(0, 150),
     main = "GHG Emissions from Gas Production")
rug(jitter(ef.prd))
lines(pdf.prd$x,
      dnorm(x = pdf.prd$x, mean = fit.prd$estimate[1], sd = fit.prd$estimate[2]),
      col = "blue")
legend("topright",
       c("Density", "Normal"),
       lty = c(1,1),
       col = c("black", "blue"))

# Production - Oil
plot(pdf.prd.oil,
     xlab = "GHG Emissions (1e3 kg / bbl Oil)",
     ylim = c(0, 25),
     main = "GHG Emissions from Oil Production")
rug(jitter(ef.prd.oil))
lines(pdf.prd.oil$x,
      dnorm(x = pdf.prd.oil$x, mean = fit.prd.oil$estimate[1], sd = fit.prd.oil$estimate[2]),
      col = "blue")
legend("topright",
       c("Density", "Normal"),
       lty = c(1,1),
       col = c("black", "blue"))

# Processing
plot(pdf.prc,
     xlab = "GHG Emissions (1e3 kg / MCF Gas)",
     ylim = c(0, 500),
     main = "GHG Emissions from Gas Processing")
rug(jitter(ef.prc))
lines(pdf.prc$x,
      dnorm(x = pdf.prc$x, mean = fit.prc$estimate[1], sd = fit.prc$estimate[2]),
      col = "blue")
legend("topright",
       c("Density", "Normal"),
       lty = c(1,1),
       col = c("black", "blue"))

# Transmission - Gas
plot(pdf.trs,
     xlab = "GHG Emissions (1e3 kg / MCF Gas)",
     ylim = c(0, 500),
     main = "GHG Emissions from Gas Transmission & Distribution")
rug(jitter(ef.trs))
lines(pdf.trs$x,
      dnorm(x = pdf.trs$x, mean = fit.trs$estimate[1], sd = fit.trs$estimate[2]),
      col = "blue")
legend("topright",
       c("Density", "Normal"),
       lty = c(1,1),
       col = c("black", "blue"))

# Transmission - Oil
plot(x.trs.oil,
     dnorm(x = x.trs.oil, mean = fit.trs.oil[1], sd = fit.trs.oil[2]),
     type = "l",
     col = "blue",
     xlab = "GHG Emissions (1e3 kg / bbl Oil)",
     ylab = "Density",
     main = "GHG Emissions from Oil Transmission & Distribution")
rug(jitter(ef.trs.oil))
legend("topright",
       c("Normal"),
       lty = 1,
       col = c("blue"))

# Total Fugitive CH4 Emissions
plot(pdf.tot,
     xlab = "CH4 Emissions (fraction of Total Gas)",
     main = "Total Fugitive CH4 Emissions")
rug(jitter(ef.tot))
lines(pdf.tot$x,
      dnorm(x = pdf.tot$x, mean = fit.tot$estimate[1], sd = fit.tot$estimate[2]),
      col = "blue")
legend("topright",
       c("Density", "Normal"),
       lty = c(1,1),
       col = c("black", "blue"))

# CH4 Emissions from Gas Transmission & Distribution - Conventional
plot(pdf.trs.conv,
     xlab = "CH4 Emissions (fraction of Total Gas)",
     ylim = c(0, 90),
     main = "CH4 Emissions from Conv. Gas Transmission and Distribution")
rug(jitter(ef.trs.conv))
lines(pdf.trs.conv$x,
      dnorm(x = pdf.trs.conv$x, mean = fit.trs.conv$estimate[1], sd = fit.trs.conv$estimate[2]),
      col = "blue")
legend("topright",
       c("Density", "Normal"),
       lty = c(1,1),
       col = c("black", "blue"))

# CH4 Emissions from Gas Transmission & Distribution - Conventional
plot(pdf.trs.unconv,
     xlab = "CH4 Emissions (fraction of Total Gas)",
     ylim = c(0, 40),
     main = "CH4 Emissions from Unconv. Gas Transmission and Distribution")
rug(jitter(ef.trs.unconv))
lines(pdf.trs.unconv$x,
      dnorm(x = pdf.trs.unconv$x, mean = fit.trs.unconv$estimate[1], sd = fit.trs.unconv$estimate[2]),
      col = "blue")
legend("topright",
       c("Density", "Normal"),
       lty = c(1,1),
       col = c("black", "blue"))

# End PDF file
dev.off()


# Export PDF and CDF data frames ------------------------------------------

# Reformat as data.frame. First column is value of term (x-axis on CDF plot),
# second column is cumulative probability density of that value occuring (y-axis
# on CDF plot).
cdf.dcw        <- data.frame(pdf.dcw$x, cdf.dcw);               names(cdf.dcw) <- c("x", "y")
cdf.prd        <- data.frame(pdf.prd$x, cdf.prd);               names(cdf.prd) <- c("x", "y")
cdf.prd.oil    <- data.frame(pdf.prd.oil$x, cdf.prd.oil);       names(cdf.prd.oil) <- c("x", "y")
cdf.tot        <- data.frame(pdf.tot$x, cdf.tot);               names(cdf.tot) <- c("x", "y")
cdf.trs        <- data.frame(pdf.trs$x, cdf.trs);               names(cdf.trs) <- c("x", "y")
cdf.trs.conv   <- data.frame(pdf.trs.conv$x, cdf.trs.conv);     names(cdf.trs.conv) <- c("x", "y")
cdf.trs.oil    <- data.frame(pdf.trs.oil$x, cdf.trs.oil);       names(cdf.trs.oil) <- c("x", "y")
cdf.trs.unconv <- data.frame(pdf.trs.unconv$x, cdf.trs.unconv); names(cdf.trs.unconv) <- c("x", "y")

# Make CDF data.frame
cdf.ghg <- data.frame(pdf.dcw$x,        cdf.dcw,
                      pdf.prd$x,        cdf.prd,
                      pdf.prc$x,        cdf.prc,
                      pdf.trs$x,        cdf.trs,
                      pdf.tot$x,        cdf.tot,
                      pdf.trs.unconv$x, cdf.trs.unconv,
                      pdf.prd.oil$x,    cdf.prd.oil,
                      x.trs.oil,        cdf.trs.oil)

# Rename columns
names(cdf.ghg) <- data.frame ("dcw.x",        "dcw.y",
                              "prd.x",        "prd.y",
                              "prc.x",        "prc.y",
                              "trs.x",        "trs.y",
                              "tot.x",        "tot.y",
                              "trs.unconv.x", "trs.unconv.y",
                              "prd.oil.x",    "prd.oil.y",
                              "trs.oil.x",    "trs.oil.y")

# Save to file
save(file=file.path(data_root, "GHG_v2.rda"), list=c("cdf.ghg"))