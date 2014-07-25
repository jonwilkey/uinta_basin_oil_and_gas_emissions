#-------------------------------------------------------------------------------
# declineCurve.R (DOGM Decline Curve Fitting)
# Version 1
# 12/25/13
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
# have ever produced and that have at least 4 years of production
po <- subset(p, subset = (histdata_well_type == "OW" &
                            everproduce == "TRUE" &
                            maxtime >= 48))
pg <- subset(p, subset = (histdata_well_type == "GW" &
                            everproduce == "TRUE" &
                            maxtime >= 48))

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

# Save fit summaries to text file
sink(file = "Fit Results Summary from Basin.pdf")
print("-----------------------------------------------------------------------")
print("-                           Oil from Oil Wells                        -")
print("-----------------------------------------------------------------------")
print("                        ***   Exponential Fit ***                      ")
summary(oo.exp)
print("                        ***   Hyperbolic Fit ***                       ")
summary(oo.hyp)
print("                         ***   Harmonic Fit ***                        ")
summary(oo.har)
print("-----------------------------------------------------------------------")
print("-                           Gas from Oil Wells                        -")
print("-----------------------------------------------------------------------")
print("                        ***   Exponential Fit ***                      ")
summary(go.exp)
print("                        ***   Hyperbolic Fit ***                       ")
summary(go.hyp)
print("                         ***   Harmonic Fit ***                        ")
summary(go.har)
print("-----------------------------------------------------------------------")
print("-                           Oil from Gas Wells                        -")
print("-----------------------------------------------------------------------")
print("                        ***   Exponential Fit ***                      ")
summary(og.exp)
print("                        ***   Hyperbolic Fit ***                       ")
summary(og.hyp)
print("                         ***   Harmonic Fit ***                        ")
summary(og.har)
print("-----------------------------------------------------------------------")
print("-                           Gas from Gas Wells                        -")
print("-----------------------------------------------------------------------")
print("                        ***   Exponential Fit ***                      ")
summary(gg.exp)
print("                        ***   Hyperbolic Fit ***                       ")
summary(gg.hyp)
print("                         ***   Harmonic Fit ***                        ")
summary(gg.har)
sink()

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
# Regression to Individual Fields
#-------------------------------------------------------------------------------
# Note - run this section for:
# (1) Oil wells - Fields: 105, 72, 590, 55, 718, 117, 1, 60
# (2) Gas wells - Fields: 630, 48, 710, 35, 18, 132, 40, 665

# Subset to select well type 'ps' from field 'f'
ps <- subset(p, subset = (histdata_well_type == "OW" &
                            everproduce == "TRUE" &
                            maxtime >= 48))
field <- 105
f <- subset(ps, subset = (welldata_field_num == "105"))

# Fit each fluid from each well type by each decline curve
# Oil from field wells
of.exp <- nlsLM(proddata_oil_prod ~ alpha * exp(-ptime / theta),
                data = f,
                start = list(alpha = 1053, theta = 32),
                control = list(maxiter=1000))
of.hyp <- nlsLM(proddata_oil_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                data = f,
                start = list(alpha = 1496, theta = 1.5, delta = 0.2),
                control = list(maxiter=1000))
of.har <- nlsLM(proddata_oil_prod ~ alpha / (1 + theta * ptime),
                data = f,
                start = list(alpha = 1402, theta = 0.09),
                control = list(maxiter=1000))
# Gas from field wells
gf.exp <- nlsLM(proddata_gas_prod ~ alpha * exp(-ptime / theta),
                data = f,
                start = list(alpha = 2165, theta = 70),
                control = list(maxiter=1000))
gf.hyp <- nlsLM(proddata_gas_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                data = f,
                start = list(alpha = 2461, theta = 1.0, delta = 0.03),
                control = list(maxiter=1000))
gf.har <- nlsLM(proddata_gas_prod ~ alpha / (1 + theta * ptime),
                data = f,
                start = list(alpha = 2459, theta = 0.03),
                control = list(maxiter=1000))
# Determine median and mean oil and gas production from field wells
f.median.oil <- NULL
f.median.gas <- NULL
f.mean.oil <- NULL
f.mean.gas <- NULL
for (j in 1:max(f$ptime)){
  f.ptime <- subset(f, subset = (ptime == j))
  f.median.oil <- rbind(f.median.oil, median(f.ptime$proddata_oil_prod))
  f.median.gas <- rbind(f.median.gas, median(f.ptime$proddata_gas_prod))
  f.mean.oil <- rbind(f.mean.oil, mean(f.ptime$proddata_oil_prod))
  f.mean.gas <- rbind(f.mean.gas, mean(f.ptime$proddata_gas_prod))
}
# Determine median and mean oil and gas production from field wells
f.median.oil <- NULL
f.median.gas <- NULL
f.mean.oil <- NULL
f.mean.gas <- NULL
for (j in 1:max(f$ptime)){
  f.ptime <- subset(f, subset = (ptime == j))
  f.median.oil <- rbind(f.median.oil, median(f.ptime$proddata_oil_prod))
  f.median.gas <- rbind(f.median.gas, median(f.ptime$proddata_gas_prod))
  f.mean.oil <- rbind(f.mean.oil, mean(f.ptime$proddata_oil_prod))
  f.mean.gas <- rbind(f.mean.gas, mean(f.ptime$proddata_gas_prod))
}
# Save fit summaries to text file
sink(file = paste("Fit Results Summary from Field ", field, sep = "", ".txt"))
print("-----------------------------------------------------------------------")
print("-                          Oil from Field Wells                       -")
print("-----------------------------------------------------------------------")
print("                        ***   Exponential Fit ***                      ")
summary(of.exp)
print("                        ***   Hyperbolic Fit ***                       ")
summary(of.hyp)
print("                         ***   Harmonic Fit ***                        ")
summary(of.har)
print("-----------------------------------------------------------------------")
print("-                          Gas from Field Wells                       -")
print("-----------------------------------------------------------------------")
print("                        ***   Exponential Fit ***                      ")
summary(gf.exp)
print("                        ***   Hyperbolic Fit ***                       ")
summary(gf.hyp)
print("                         ***   Harmonic Fit ***                        ")
summary(gf.har)
sink()
# Print results to PDF
pdf(file = paste("Fit Results from Field ", field, sep = "", ".pdf"))
# Oil from field wells plot
x <- seq(from = 1, to = max(f$ptime))
plot(x, f.median.oil,
     type="l",
     ylab="Oil Production (bbl/month)",
     xlab="Time Since First Production (months)")
title(paste("Oil Production from all Wells in Field # ", field, sep = ""))
lines(x, f.mean.oil, col="black", lty=2)
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
plot(x, f.median.gas,
     type="l",
     ylab="Gas Production (MCF/month)",
     xlab="Time Since First Production (months)")
title(paste("Gas Production from all Wells in Field # ", field, sep = ""))
lines(x, f.mean.gas, col="black", lty=2)
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
dev.off()

#-------------------------------------------------------------------------------
# Regression to Individual Wells for Oil Fields
#-------------------------------------------------------------------------------

coef.exp.o <- NULL
coef.hyp.o <- NULL
coef.har.o <- NULL
pdf(file = paste("Oil Wells from Field ", field, sep = "", ".pdf"))
for (a in unique(f[,"proddata_api",drop=TRUE])) {
  # Creates a subset 'w' from dataframe 'f' whose API# is = to the API# in step
  # 'a' of the for-loop
  w <- subset(f,subset = (proddata_api == a))
  # Perform regression for oil from well
  fit.exp.o <- nlsLM(proddata_oil_prod ~ alpha * exp(-ptime / theta),
                   data = w,
                   start = list(alpha = coef(of.exp)[1], theta = coef(of.exp)[2]),
                   control = list(maxiter=1000))
  fit.hyp.o <- nlsLM(proddata_oil_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                   data = w,
                   start = list(alpha = coef(of.hyp)[1], theta = coef(of.hyp)[2], delta = coef(of.hyp)[3]),
                   control = list(maxiter=1000))
  fit.har.o <- nlsLM(proddata_oil_prod ~ alpha / (1 + theta * ptime),
                   data = w,
                   start = list(alpha = coef(of.hyp)[1], theta = coef(of.hyp)[2]),
                   control = list(maxiter=1000))  
  coef.exp.o <- rbind(coef.exp.o, coef(fit.exp.o))
  coef.hyp.o <- rbind(coef.hyp.o, coef(fit.hyp.o))
  coef.har.o <- rbind(coef.har.o, coef(fit.har.o))  
  plot(proddata_oil_prod ~ ptime,
       data=w,
       type="b",
       pch = 20,
       ylab="Oil Production (bbl/month)",
       xlab="Time Since First Production (months)")
  title(paste("Oil from API# ", a, sep = ""))
  lines(w$ptime, y=predict(fit.exp.o), col="red")
  lines(w$ptime, y=predict(fit.hyp.o), col="blue")
  lines(w$ptime, y=predict(fit.har.o), col="green")
  legend("topright",
         c("Actual", "Exponential", "Hyperbolic", "Harmonic"),
         lty = c(1,1,1,1),
         col = c("black", "red", "blue", "green"))  
}
dev.off()

sink(file = paste("Oil Wells Summary from Field ", field, sep = "", ".txt"))
print("-----------------------------------------------------------------------")
print("-                        Oil from Field Wells by Well                 -")
print("-----------------------------------------------------------------------")
print("                        ***   Exponential Fit ***                      ")
summary(coef.exp.o)
print("                        ***   Hyperbolic Fit ***                       ")
summary(coef.hyp.o)
print("                         ***   Harmonic Fit ***                        ")
summary(coef.har.o)
sink()
# Drop observations that are > 10^3 median observation for density plots
d.coef.exp.o <- subset(coef.exp.o, subset = (coef.exp.o[,1] < 1000*median(coef.exp.o[,1]) & coef.exp.o[,2] < 1000*median(coef.exp.o[,2])))
d.coef.hyp.o <- subset(coef.hyp.o, subset = (coef.hyp.o[,1] < 1000*median(coef.hyp.o[,1]) & coef.hyp.o[,2] < 1000*median(coef.hyp.o[,2])))
d.coef.har.o <- subset(coef.har.o, subset = (coef.har.o[,1] < 1000*median(coef.har.o[,1]) & coef.har.o[,2] < 1000*median(coef.har.o[,2])))
# Print density plots of fit coefficients
pdf(file = paste("Oil Wells Fit Coefficients from Field ", field, sep = "", ".pdf"))
plot(density(d.coef.exp.o[,1]), xlab = "Value", main = "Alpha - Exponential Fit for Oil")
plot(density(d.coef.exp.o[,2]), xlab = "Value", main = "Theta - Exponential Fit for Oil")
plot(density(d.coef.hyp.o[,1]), xlab = "Value", main = "Alpha - Hyperbolic Fit for Oil")
plot(density(d.coef.hyp.o[,3]), xlab = "Value", main = "Theta - Hyperbolic Fit for Oil")
plot(density(d.coef.hyp.o[,2]), xlab = "Value", main = "Delta - Hyperbolic Fit for Oil")
plot(density(d.coef.har.o[,1]), xlab = "Value", main = "Alpha - Harmonic Fit for Oil")
plot(density(d.coef.har.o[,2]), xlab = "Value", main = "Theta - Harmonic Fit for Oil")
dev.off()

#-------------------------------------------------------------------------------
# Regression to Individual Wells for Gas Fields
#-------------------------------------------------------------------------------

coef.exp.g <- NULL
coef.hyp.g <- NULL
coef.har.g <- NULL
pdf(file = paste("Gas Wells from Field ", field, sep = "", ".pdf"))
for (a in unique(f[,"proddata_api",drop=TRUE])) {
  # Creates a subset 'w' from dataframe 'f' whose API# is = to the API# in step
  # 'a' of the for-loop
  w <- subset(f,subset = (proddata_api == a))
  # Perform regression for oil from well
  fit.exp.g <- nlsLM(proddata_gas_prod ~ alpha * exp(-ptime / theta),
                     data = w,
                     start = list(alpha = coef(gf.exp)[1], theta = coef(gf.exp)[2]),
                     control = list(maxiter=1000))
  fit.hyp.g <- nlsLM(proddata_gas_prod ~ alpha*(1+theta*delta*ptime)^(-1/theta),
                     data = w,
                     start = list(alpha = coef(gf.hyp)[1], theta = coef(gf.hyp)[2], delta = coef(gf.hyp)[3]),
                     control = list(maxiter=1000))
  fit.har.g <- nlsLM(proddata_gas_prod ~ alpha / (1 + theta * ptime),
                     data = w,
                     start = list(alpha = coef(gf.hyp)[1], theta = coef(gf.hyp)[2]),
                     control = list(maxiter=1000))  
  coef.exp.g <- rbind(coef.exp.g, coef(fit.exp.g))
  coef.hyp.g <- rbind(coef.hyp.g, coef(fit.hyp.g))
  coef.har.g <- rbind(coef.har.g, coef(fit.har.g))  
  plot(proddata_gas_prod ~ ptime,
       data=w,
       type="b",
       pch = 20,
       ylab="Gas Production (MCF/month)",
       xlab="Time Since First Production (months)")
  title(paste("Gas from API# ", a, sep = ""))
  lines(w$ptime, y=predict(fit.exp.g), col="red")
  lines(w$ptime, y=predict(fit.hyp.g), col="blue")
  lines(w$ptime, y=predict(fit.har.g), col="green")
  legend("topright",
         c("Actual", "Exponential", "Hyperbolic", "Harmonic"),
         lty = c(1,1,1,1),
         col = c("black", "red", "blue", "green"))  
}
dev.off()

sink(file = paste("Gas Wells Summary from Field ", field, sep = "", ".txt"))
print("-----------------------------------------------------------------------")
print("-                        Gas from Field Wells by Well                 -")
print("-----------------------------------------------------------------------")
print("                        ***   Exponential Fit ***                      ")
summary(coef.exp.g)
print("                        ***   Hyperbolic Fit ***                       ")
summary(coef.hyp.g)
print("                         ***   Harmonic Fit ***                        ")
summary(coef.har.g)
sink()
# Drop observations that are > 10^3 median observation for density plots
d.coef.exp.g <- subset(coef.exp.g, subset = (coef.exp.g[,1] < 1000*median(coef.exp.g[,1]) & coef.exp.g[,2] < 1000*median(coef.exp.g[,2])))
d.coef.hyp.g <- subset(coef.hyp.g, subset = (coef.hyp.g[,1] < 1000*median(coef.hyp.g[,1]) & coef.hyp.g[,2] < 1000*median(coef.hyp.g[,2])))
d.coef.har.g <- subset(coef.har.g, subset = (coef.har.g[,1] < 1000*median(coef.har.g[,1]) & coef.har.g[,2] < 1000*median(coef.har.g[,2])))
# Print density plots of fit coefficients
pdf(file = paste("Gas Wells Fit Coefficients from Field ", field, sep = "", ".pdf"))
plot(density(d.coef.exp.g[,1]), xlab = "Value", main = "Alpha - Exponential Fit for Gas")
plot(density(d.coef.exp.g[,2]), xlab = "Value", main = "Theta - Exponential Fit for Gas")
plot(density(d.coef.hyp.g[,1]), xlab = "Value", main = "Alpha - Hyperbolic Fit for Gas")
plot(density(d.coef.hyp.g[,3]), xlab = "Value", main = "Theta - Hyperbolic Fit for Gas")
plot(density(d.coef.hyp.g[,2]), xlab = "Value", main = "Delta - Hyperbolic Fit for Gas")
plot(density(d.coef.har.g[,1]), xlab = "Value", main = "Alpha - Harmonic Fit for Gas")
plot(density(d.coef.har.g[,2]), xlab = "Value", main = "Theta - Harmonic Fit for Gas")
dev.off()

#-------------------------------------------------------------------------------
# Pasting to Clipboard to Copy to Excel Table
#-------------------------------------------------------------------------------
# Copy Basin Coefficients (run one line at a time and paste into Excel)
write.excel(coef(oo.exp), col.names = FALSE)
write.excel(coef(oo.hyp), col.names = FALSE)
write.excel(coef(oo.har), col.names = FALSE)
write.excel(coef(go.exp), col.names = FALSE)
write.excel(coef(go.hyp), col.names = FALSE)
write.excel(coef(go.har), col.names = FALSE)
write.excel(coef(og.exp), col.names = FALSE)
write.excel(coef(og.hyp), col.names = FALSE)
write.excel(coef(og.har), col.names = FALSE)
write.excel(coef(gg.exp), col.names = FALSE)
write.excel(coef(gg.hyp), col.names = FALSE)
write.excel(coef(gg.har), col.names = FALSE)
# Copy Field Coefficients (run one line at a time and paste into Excel)
write.excel(coef(of.exp), col.names = FALSE)
write.excel(coef(of.hyp), col.names = FALSE)
write.excel(coef(of.har), col.names = FALSE)
write.excel(coef(gf.exp), col.names = FALSE)
write.excel(coef(gf.hyp), col.names = FALSE)
write.excel(coef(gf.har), col.names = FALSE)
# Copy Field Coefficients (run one line at a time and paste into Excel)
write.excel(c(median(coef.exp.o[,1]), median(coef.exp.o[,2])), col.names = FALSE)
write.excel(c(median(coef.hyp.o[,1]), median(coef.hyp.o[,2]), median(coef.hyp.o[,3])), col.names = FALSE)
write.excel(c(median(coef.har.o[,1]), median(coef.har.o[,2])), col.names = FALSE)
write.excel(c(median(coef.exp.g[,1]), median(coef.exp.g[,2])), col.names = FALSE)
write.excel(c(median(coef.hyp.g[,1]), median(coef.hyp.g[,2]), median(coef.hyp.o[,3])), col.names = FALSE)
write.excel(c(median(coef.har.g[,1]), median(coef.har.g[,2])), col.names = FALSE)

# To get API totals (# of unique APIs in each field, and # of APIs that survive
# filter), run the following SQL queries after changing field number in both
# lines
sqldf("select count(distinct proddata_api) from p where (welldata_field_num = 665)")
sqldf("select count(distinct proddata_api) from p where (welldata_field_num = 665 and maxtime >= 48)")