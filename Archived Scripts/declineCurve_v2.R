#-------------------------------------------------------------------------------
# declineCurve_v2.R (DOGM Decline Curve Fitting)
# Version 2
# 01/07/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Change this to the folder where you keep your *.rda files
# For Mac:
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data"
# For Windows:
data_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(data_root)

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------

# Copy/Paste function
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------

# 'minpack.lm' for nonlinear least-squares regression using Levenberg-Marquardt
# algorithm
library(minpack.lm)
# 'sqldf' for SQL queries
library(sqldf)

#-------------------------------------------------------------------------------
# Load DOGM data tables
#-------------------------------------------------------------------------------

# Load the dataframe named 'production'
p <- load(file.path(data_root, "production.rda"))
# Rename dataframe from brevity
p <- production
# Drop first month of production since it can be partial month
p <- subset(p, subset = (ptime != 0))

#-------------------------------------------------------------------------------
# Regression to All Wells in Basin
#-------------------------------------------------------------------------------

# Select subsets of production data for oil wells 'po' and gas wells 'pg' that
# have ever produced
po <- subset(p, subset = (histdata_well_type == "OW" &
                            everproduce == "TRUE"))
pg <- subset(p, subset = (histdata_well_type == "GW" &
                            everproduce == "TRUE"))

# Fit the data for each fluid for each type of well
# Oil from oil wells
oo.exp <- nlsLM(proddata_oil_prod ~ alpha * exp(-ptime / theta),
                 data = po,
                 start = list(alpha = 1053, theta = 32),
                 control = list(maxiter=1000))
oo.hyp <- nlsLM(proddata_oil_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                 data = po,
                 start = list(alpha = 1496, theta = 1.5, delta = 0.2),
                 control = list(maxiter=1000))
oo.har <- nlsLM(proddata_oil_prod ~ alpha / (1 + theta * ptime),
                 data = po,
                 start = list(alpha = 1402, theta = 0.09),
                 control = list(maxiter=1000))

# Gas from oil wells
go.exp <- nlsLM(proddata_gas_prod ~ alpha * exp(-ptime / theta),
                data = po,
                start = list(alpha = 2165, theta = 70),
                control = list(maxiter=1000))
go.hyp <- nlsLM(proddata_gas_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                data = po,
                start = list(alpha = 2461, theta = 1.0, delta = 0.03),
                control = list(maxiter=1000))
go.har <- nlsLM(proddata_gas_prod ~ alpha / (1 + theta * ptime),
                data = po,
                start = list(alpha = 2459, theta = 0.03),
                control = list(maxiter=1000))

# Oil from gas wells
og.exp <- nlsLM(proddata_oil_prod ~ alpha * exp(-ptime / theta),
                data = pg,
                start = list(alpha = 170, theta = 23),
                control = list(maxiter=1000))
og.hyp <- nlsLM(proddata_oil_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                data = pg,
                start = list(alpha = 2121, theta = 0.8, delta = 0.1),
                control = list(maxiter=1000))
og.har <- nlsLM(proddata_oil_prod ~ alpha / (1 + theta * ptime),
                data = pg,
                start = list(alpha = 218, theta = 0.1),
                control = list(maxiter=1000))

# Gas from gas wells
gg.exp <- nlsLM(proddata_gas_prod ~ alpha * exp(-ptime / theta),
                data = pg,
                start = list(alpha = 16390, theta = 31),
                control = list(maxiter=1000))
gg.hyp <- nlsLM(proddata_gas_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                data = pg,
                start = list(alpha = 20820, theta = 1.0, delta = 0.09),
                control = list(maxiter=1000))
gg.har <- nlsLM(proddata_gas_prod ~ alpha / (1 + theta * ptime),
                data = pg,
                start = list(alpha = 20890, theta = 0.09),
                control = list(maxiter=1000))

# Determine median and mean oil and gas production from oil wells
po.median.oil <- NULL
po.median.gas <- NULL
po.mean.oil <- NULL
po.mean.gas <- NULL
for (j in 1:max(po$ptime)){
  po.ptime <- subset(po, subset = (ptime == j))
  po.median.oil <- rbind(po.median.oil, median(po.ptime$proddata_oil_prod))
  po.median.gas <- rbind(po.median.gas, median(po.ptime$proddata_gas_prod))
  po.mean.oil <- rbind(po.mean.oil, mean(po.ptime$proddata_oil_prod))
  po.mean.gas <- rbind(po.mean.gas, mean(po.ptime$proddata_gas_prod))
}
# Determine median and mean oil and gas production from gas wells
pg.median.oil <- NULL
pg.median.gas <- NULL
pg.mean.oil <- NULL
pg.mean.gas <- NULL
for (j in 1:max(pg$ptime)){
  pg.ptime <- subset(pg, subset = (ptime == j))
  pg.median.oil <- rbind(pg.median.oil, median(pg.ptime$proddata_oil_prod))
  pg.median.gas <- rbind(pg.median.gas, median(pg.ptime$proddata_gas_prod))
  pg.mean.oil <- rbind(pg.mean.oil, mean(pg.ptime$proddata_oil_prod))
  pg.mean.gas <- rbind(pg.mean.gas, mean(pg.ptime$proddata_gas_prod))
}

# Print results to PDF
pdf(file = "Fit Results from Basin.pdf")
# Oil from oil wells plot
x <- seq(from = 1, to = max(po$ptime))
plot(x, po.median.oil,
     type="l",
     ylab="Oil Production (bbl/month)",
     xlab="Time Since First Production (months)")
title("Oil Production from all Oil Wells in the Basin")
lines(x, po.mean.oil, col="black", lty=2)
y.exp <- coef(oo.exp)[1] * exp(-x / coef(oo.exp)[2])
y.hyp <- coef(oo.hyp)[1] * (1 + coef(oo.hyp)[3] * coef(oo.hyp)[2] * x)^(-1/coef(oo.hyp)[2])
y.har <- coef(oo.har)[1] / (1 + coef(oo.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
# Gas from oil wells plot
plot(x, po.median.gas,
     type="l",
     ylab="Gas Production (MCF/month)",
     xlab="Time Since First Production (months)")
title("Gas Production from all Oil Wells in the Basin")
lines(x, po.mean.gas, col="black", lty=2)
y.exp <- coef(go.exp)[1] * exp(-x / coef(go.exp)[2])
y.hyp <- coef(go.hyp)[1] * (1 + coef(go.hyp)[3] * coef(go.hyp)[2] * x)^(-1/coef(go.hyp)[2])
y.har <- coef(go.har)[1] / (1 + coef(go.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
# Oil from gas wells plot
x <- seq(from = 1, to = max(pg$ptime))
plot(x, pg.median.oil,
     type="l",
     ylab="Oil Production (bbl/month)",
     xlab="Time Since First Production (months)")
title("Oil Production from all Gas Wells in the Basin")
lines(x, pg.mean.oil, col="black", lty=2)
y.exp <- coef(og.exp)[1] * exp(-x / coef(og.exp)[2])
y.hyp <- coef(og.hyp)[1] * (1 + coef(og.hyp)[3] * coef(og.hyp)[2] * x)^(-1/coef(og.hyp)[2])
y.har <- coef(og.har)[1] / (1 + coef(og.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
# Gas from gas wells plot
plot(x, pg.median.gas,
     type="l",
     ylab="Gas Production (MCF/month)",
     xlab="Time Since First Production (months)")
title("Gas Production from all Gas Wells in the Basin")
lines(x, pg.mean.gas, col="black", lty=2)
y.exp <- coef(gg.exp)[1] * exp(-x / coef(gg.exp)[2])
y.hyp <- coef(gg.hyp)[1] * (1 + coef(gg.hyp)[3] * coef(gg.hyp)[2] * x)^(-1/coef(gg.hyp)[2])
y.har <- coef(gg.har)[1] / (1 + coef(gg.har)[2] * x)
lines(x, y.exp, col="red")
lines(x, y.hyp, col="blue")
lines(x, y.har, col="green")
legend("topright",
       c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
       lty = c(1,2,1,1,1),
       col = c("black", "black", "red", "blue", "green"))
dev.off()

#-------------------------------------------------------------------------------
# Regression to Individual Oil Fields
#-------------------------------------------------------------------------------

# Create list field.ow of unique field #'s in p that are oil wells
field.ow <- sqldf("select distinct(welldata_field_num)
                  from p
                  where welldata_well_type = 'OW'")

# Define count of unique api numbers for use in for loop
field.ow.api.count <- NULL
for (j in 1:length(field.ow[,1])){
  ps <- subset(p, subset = (welldata_field_num == field.ow[j,]))
  # Count number of unique APIs in field ps
  count <- sqldf("select count(distinct(welldata_api)) from ps where welldata_well_type = 'OW'")
  # Add to list field.ow.api.count
  field.ow.api.count <- rbind(field.ow.api.count, count)
}
# Merge list of field numbers and counts of unique API numbers in each field
field.ow <- cbind(field.ow, field.ow.api.count)
# Drop fields that fail to reach convergence
field.ow <- subset(field.ow, subset = (field.ow[,1] != 600))

# Create dataframes for storing model coefficients
coef.exp.oow <- NULL
coef.hyp.oow <- NULL
coef.har.oow <- NULL
coef.exp.gow <- NULL
coef.hyp.gow <- NULL
coef.har.gow <- NULL

# Save output to pdf
pdf(file = paste("Fit Results Summary from Oil Fields.pdf"))

for (j in 1:length(field.ow[,1])){
  # Create subset ps of all wells in field j of field.ow that have ever produced
  ps <- subset(p, subset = (welldata_field_num == field.ow[j,1] &
                              everproduce == "TRUE"))
  
  # Fit each fluid from each well type by each decline curve
  # Oil from field wells
  of.exp <- nlsLM(proddata_oil_prod ~ alpha * exp(-ptime / theta),
                  data = ps,
                  start = list(alpha = coef(oo.exp)[1],
                               theta = coef(oo.exp)[2]),
                  control = list(maxiter=1000))
  of.hyp <- nlsLM(proddata_oil_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                  data = ps,
                  start = list(alpha = coef(oo.hyp)[1],
                               theta = coef(oo.hyp)[2],
                               delta = coef(oo.hyp)[3]),
                  control = list(maxiter=1000))
  of.har <- nlsLM(proddata_oil_prod ~ alpha / (1 + theta * ptime),
                  data = ps,
                  start = list(alpha = coef(oo.har)[1],
                               theta = coef(oo.har)[2]),
                  control = list(maxiter=1000))
  # Extract coefficients
  coef.exp.oow <- rbind(coef.exp.oow, coef(of.exp))
  coef.hyp.oow <- rbind(coef.hyp.oow, coef(of.hyp))
  coef.har.oow <- rbind(coef.har.oow, coef(of.har))
  
  # Gas from field wells
  gf.exp <- nlsLM(proddata_gas_prod ~ alpha * exp(-ptime / theta),
                  data = ps,
                  start = list(alpha = coef(go.exp)[1],
                               theta = coef(go.exp)[2]),
                  control = list(maxiter=1000))
  gf.hyp <- nlsLM(proddata_gas_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                  data = ps,
                  start = list(alpha = coef(go.hyp)[1],
                               theta = coef(go.hyp)[2],
                               delta = coef(go.hyp)[3]),
                  control = list(maxiter=1000))
  gf.har <- nlsLM(proddata_gas_prod ~ alpha / (1 + theta * ptime),
                  data = ps,
                  start = list(alpha = coef(go.har)[1],
                               theta = coef(go.har)[2]),
                  control = list(maxiter=1000))
  # Extract coefficients
  coef.exp.gow <- rbind(coef.exp.gow, coef(gf.exp))
  coef.hyp.gow <- rbind(coef.hyp.gow, coef(gf.hyp))
  coef.har.gow <- rbind(coef.har.gow, coef(gf.har))
  
  # Determine median and mean oil and gas production from field wells
  ps.median.oil <- NULL
  ps.median.gas <- NULL
  ps.mean.oil <- NULL
  ps.mean.gas <- NULL
  for (k in 1:max(ps$ptime)){
    ps.ptime <- subset(ps, subset = (ptime == k))
    ps.median.oil <- rbind(ps.median.oil, median(ps.ptime$proddata_oil_prod))
    ps.median.gas <- rbind(ps.median.gas, median(ps.ptime$proddata_gas_prod))
    ps.mean.oil <- rbind(ps.mean.oil, mean(ps.ptime$proddata_oil_prod))
    ps.mean.gas <- rbind(ps.mean.gas, mean(ps.ptime$proddata_gas_prod))
  }
  # Determine median and mean oil and gas production from field wells
  ps.median.oil <- NULL
  ps.median.gas <- NULL
  ps.mean.oil <- NULL
  ps.mean.gas <- NULL
  for (l in 1:max(ps$ptime)){
    ps.ptime <- subset(ps, subset = (ptime == l))
    ps.median.oil <- rbind(ps.median.oil, median(ps.ptime$proddata_oil_prod))
    ps.median.gas <- rbind(ps.median.gas, median(ps.ptime$proddata_gas_prod))
    ps.mean.oil <- rbind(ps.mean.oil, mean(ps.ptime$proddata_oil_prod))
    ps.mean.gas <- rbind(ps.mean.gas, mean(ps.ptime$proddata_gas_prod))
  }
  
  # Oil from field wells plot
  x <- seq(from = 1, to = max(ps$ptime))
  plot(x, ps.median.oil,
       type="l",
       ylab="Oil Production (bbl/month)",
       xlab="Time Since First Production (months)")
  title(paste("Oil Production from all Wells in Field # ", field.ow[j,1], sep = ""))
  lines(x, ps.mean.oil, col="black", lty=2)
  y.exp <- coef(of.exp)[1] * exp(-x / coef(of.exp)[2])
  y.hyp <- coef(of.hyp)[1] * (1 + coef(of.hyp)[3] * coef(of.hyp)[2] * x)^(-1/coef(of.hyp)[2])
  y.har <- coef(of.har)[1] / (1 + coef(of.har)[2] * x)
  lines(x, y.exp, col="red")
  lines(x, y.hyp, col="blue")
  lines(x, y.har, col="green")
  legend("topright",
         c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
         lty = c(1,2,1,1,1),
         col = c("black", "black", "red", "blue", "green"))
  # Gas from field wells plot
  plot(x, ps.median.gas,
       type="l",
       ylab="Gas Production (MCF/month)",
       xlab="Time Since First Production (months)")
  title(paste("Gas Production from all Wells in Field # ", field.ow[j,1], sep = ""))
  lines(x, ps.mean.gas, col="black", lty=2)
  y.exp <- coef(gf.exp)[1] * exp(-x / coef(gf.exp)[2])
  y.hyp <- coef(gf.hyp)[1] * (1 + coef(gf.hyp)[3] * coef(gf.hyp)[2] * x)^(-1/coef(gf.hyp)[2])
  y.har <- coef(gf.har)[1] / (1 + coef(gf.har)[2] * x)
  lines(x, y.exp, col="red")
  lines(x, y.hyp, col="blue")
  lines(x, y.har, col="green")
  legend("topright",
         c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
         lty = c(1,2,1,1,1),
         col = c("black", "black", "red", "blue", "green"))
}
dev.off()

#-------------------------------------------------------------------------------
# Regression to Individual Gas Fields
#-------------------------------------------------------------------------------

# Create list field.ow of unique field #'s in p that are oil wells
field.gw <- sqldf("select distinct(welldata_field_num)
                  from p
                  where welldata_well_type = 'GW'")

# Define count of unique api numbers for use in for loop
field.gw.api.count <- NULL
for (j in 1:length(field.gw[,1])){
  ps <- subset(p, subset = (welldata_field_num == field.gw[j,]))
  # Count number of unique APIs in field ps
  count <- sqldf("select count(distinct(welldata_api)) from ps where welldata_well_type = 'GW'")
  # Add to list field.ow.api.count
  field.gw.api.count <- rbind(field.gw.api.count, count)
}
# Merge list of field numbers and counts of unique API numbers in each field
field.gw <- cbind(field.gw, field.gw.api.count)
# Only keep fields with > set number of unique APIs
field.gw <- subset(field.gw, subset = (field.gw[,1] != 80 &
                                       field.gw[,1] != 625 &
                                       field.gw[,1] != 799))

# Create dataframes for storing model coefficients
coef.exp.ogw <- NULL
coef.hyp.ogw <- NULL
coef.har.ogw <- NULL
coef.exp.ggw <- NULL
coef.hyp.ggw <- NULL
coef.har.ggw <- NULL

# Save output to txt and pdf
pdf(file = paste("Fit Results Summary from Gas Fields.pdf"))

for (j in 1:length(field.gw[,1])){
  # Create subset ps of all wells in field j of field.ow that have ever produced
  ps <- subset(p, subset = (welldata_field_num == field.gw[j,1] &
                              everproduce == "TRUE"))
  
  # Fit each fluid from each well type by each decline curve
  # Oil from field wells
  of.exp <- nlsLM(proddata_oil_prod ~ alpha * exp(-ptime / theta),
                  data = ps,
                  start = list(alpha = coef(og.exp)[1],
                               theta = coef(og.exp)[2]),
                  control = list(maxiter=1000))
  of.hyp <- nlsLM(proddata_oil_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                  data = ps,
                  start = list(alpha = coef(og.hyp)[1],
                               theta = coef(og.hyp)[2],
                               delta = coef(og.hyp)[3]),
                  control = list(maxiter=1000))
  of.har <- nlsLM(proddata_oil_prod ~ alpha / (1 + theta * ptime),
                  data = ps,
                  start = list(alpha = coef(og.har)[1],
                               theta = coef(og.har)[2]),
                  control = list(maxiter=1000))
  # Extract coefficients
  coef.exp.ogw <- rbind(coef.exp.ogw, coef(of.exp))
  coef.hyp.ogw <- rbind(coef.hyp.ogw, coef(of.hyp))
  coef.har.ogw <- rbind(coef.har.ogw, coef(of.har))
  
  # Gas from field wells
  gf.exp <- nlsLM(proddata_gas_prod ~ alpha * exp(-ptime / theta),
                  data = ps,
                  start = list(alpha = coef(gg.exp)[1],
                               theta = coef(gg.exp)[2]),
                  control = list(maxiter=1000))
  gf.hyp <- nlsLM(proddata_gas_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                  data = ps,
                  start = list(alpha = coef(gg.hyp)[1],
                               theta = coef(gg.hyp)[2],
                               delta = coef(gg.hyp)[3]),
                  control = list(maxiter=1000))
  gf.har <- nlsLM(proddata_gas_prod ~ alpha / (1 + theta * ptime),
                  data = ps,
                  start = list(alpha = coef(gg.har)[1],
                               theta = coef(gg.har)[2]),
                  control = list(maxiter=1000))
  # Extract coefficients
  coef.exp.ggw <- rbind(coef.exp.ggw, coef(gf.exp))
  coef.hyp.ggw <- rbind(coef.hyp.ggw, coef(gf.hyp))
  coef.har.ggw <- rbind(coef.har.ggw, coef(gf.har))
  
  # Determine median and mean oil and gas production from field wells
  ps.median.oil <- NULL
  ps.median.gas <- NULL
  ps.mean.oil <- NULL
  ps.mean.gas <- NULL
  for (k in 1:max(ps$ptime)){
    ps.ptime <- subset(ps, subset = (ptime == k))
    ps.median.oil <- rbind(ps.median.oil, median(ps.ptime$proddata_oil_prod))
    ps.median.gas <- rbind(ps.median.gas, median(ps.ptime$proddata_gas_prod))
    ps.mean.oil <- rbind(ps.mean.oil, mean(ps.ptime$proddata_oil_prod))
    ps.mean.gas <- rbind(ps.mean.gas, mean(ps.ptime$proddata_gas_prod))
  }
  # Determine median and mean oil and gas production from field wells
  ps.median.oil <- NULL
  ps.median.gas <- NULL
  ps.mean.oil <- NULL
  ps.mean.gas <- NULL
  for (l in 1:max(ps$ptime)){
    ps.ptime <- subset(ps, subset = (ptime == l))
    ps.median.oil <- rbind(ps.median.oil, median(ps.ptime$proddata_oil_prod))
    ps.median.gas <- rbind(ps.median.gas, median(ps.ptime$proddata_gas_prod))
    ps.mean.oil <- rbind(ps.mean.oil, mean(ps.ptime$proddata_oil_prod))
    ps.mean.gas <- rbind(ps.mean.gas, mean(ps.ptime$proddata_gas_prod))
  }
  
  # Oil from field wells plot
  x <- seq(from = 1, to = max(ps$ptime))
  plot(x, ps.median.oil,
       type="l",
       ylab="Oil Production (bbl/month)",
       xlab="Time Since First Production (months)")
  title(paste("Oil Production from all Wells in Field # ", field.gw[j,1], sep = ""))
  lines(x, ps.mean.oil, col="black", lty=2)
  y.exp <- coef(of.exp)[1] * exp(-x / coef(of.exp)[2])
  y.hyp <- coef(of.hyp)[1] * (1 + coef(of.hyp)[3] * coef(of.hyp)[2] * x)^(-1/coef(of.hyp)[2])
  y.har <- coef(of.har)[1] / (1 + coef(of.har)[2] * x)
  lines(x, y.exp, col="red")
  lines(x, y.hyp, col="blue")
  lines(x, y.har, col="green")
  legend("topright",
         c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
         lty = c(1,2,1,1,1),
         col = c("black", "black", "red", "blue", "green"))
  # Gas from field wells plot
  plot(x, ps.median.gas,
       type="l",
       ylab="Gas Production (MCF/month)",
       xlab="Time Since First Production (months)")
  title(paste("Gas Production from all Wells in Field # ", field.gw[j,1], sep = ""))
  lines(x, ps.mean.gas, col="black", lty=2)
  y.exp <- coef(gf.exp)[1] * exp(-x / coef(gf.exp)[2])
  y.hyp <- coef(gf.hyp)[1] * (1 + coef(gf.hyp)[3] * coef(gf.hyp)[2] * x)^(-1/coef(gf.hyp)[2])
  y.har <- coef(gf.har)[1] / (1 + coef(gf.har)[2] * x)
  lines(x, y.exp, col="red")
  lines(x, y.hyp, col="blue")
  lines(x, y.har, col="green")
  legend("topright",
         c("Median", "Mean", "Exponential", "Hyperbolic", "Harmonic"),
         lty = c(1,2,1,1,1),
         col = c("black", "black", "red", "blue", "green"))
}
dev.off()

# #-------------------------------------------------------------------------------
# # Pasting to Clipboard to Copy to Excel Table
# #-------------------------------------------------------------------------------
# # Uncomment and run one line at a time and to copy & paste into Excel
# 
# # Basin Coefficients
# write.excel(coef(oo.exp), col.names = FALSE)
# write.excel(coef(oo.hyp), col.names = FALSE)
# write.excel(coef(oo.har), col.names = FALSE)
# write.excel(coef(go.exp), col.names = FALSE)
# write.excel(coef(go.hyp), col.names = FALSE)
# write.excel(coef(go.har), col.names = FALSE)
# write.excel(coef(og.exp), col.names = FALSE)
# write.excel(coef(og.hyp), col.names = FALSE)
# write.excel(coef(og.har), col.names = FALSE)
# write.excel(coef(gg.exp), col.names = FALSE)
# write.excel(coef(gg.hyp), col.names = FALSE)
# write.excel(coef(gg.har), col.names = FALSE)
# # Oil Field Coefficients
# write.excel(field.ow[,1], col.names = FALSE)
# write.excel(field.ow[,2]/sum(field.ow.api.count[,1]))
# write.excel(coef.exp.oow, col.names = FALSE)
# write.excel(coef.hyp.oow, col.names = FALSE)
# write.excel(coef.har.oow, col.names = FALSE)
# write.excel(coef.exp.gow, col.names = FALSE)
# write.excel(coef.hyp.gow, col.names = FALSE)
# write.excel(coef.har.gow, col.names = FALSE)
# # Gas Field Coefficients
# write.excel(field.gw[,1], col.names = FALSE)
# write.excel(field.gw[,2]/sum(field.gw.api.count[,1]))
# write.excel(coef.exp.ogw, col.names = FALSE)
# write.excel(coef.hyp.ogw, col.names = FALSE)
# write.excel(coef.har.ogw, col.names = FALSE)
# write.excel(coef.exp.ggw, col.names = FALSE)
# write.excel(coef.hyp.ggw, col.names = FALSE)
# write.excel(coef.har.ggw, col.names = FALSE)