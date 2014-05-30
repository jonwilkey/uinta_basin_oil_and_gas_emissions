#-------------------------------------------------------------------------------
# Decline Curves
#-------------------------------------------------------------------------------

# Because wells can start production at any day during the first month of production I drop the initial month of
# production (t = 0) for the purpose of estimating parameters. Once the model parameters are estimated use the model to
# predict the t = 0 production. Alternatively, scale first month production based on how many days during the
# month the well produced.
p <- subset(p, subset = (ptime != 0))

# The hyperbolic decline curve is fit to data by nonlinear least-squares. The R function nls() finds the minimum of the
# sum of squares function S(x, parameters) using Gauss-Newton. derive() returns the evaluations of S(x,parameters) and
# its gradient across parameters, for use in the Gauss-Newton routine.

# Hyperbolic function
hyperbol <- deriv(~ alpha * (1 + theta * delta * x)^(-1/theta),
                  namevec = c("alpha", "theta", "delta"),                  
                  function(x, alpha, theta, delta){}
)

# Create a dataframe containing only data for all fields
pk <- p
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/allfields.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 3.278750e+04
test_delta <- 1.321544e+00
test_theta <- 2.430975e-01
decline_coef_hgg <- c(test_alpha, test_delta, test_theta)
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 358.8669581
test_delta <- 1.1508262
test_theta <- 0.2811788
decline_coef_hog <- c(test_alpha, test_delta, test_theta)
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 2.982489e+03
test_delta <- 1.449227e+00
test_theta <- 5.596764e-02
decline_coef_hgo <- c(test_alpha, test_delta, test_theta)
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 3704.193836
test_delta <- 1.780258
test_theta <- 1.780258
decline_coef_hoo <- c(test_alpha, test_delta, test_theta)
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pko)
dev.off()

# Create a dataframe containing only data for field 105
pk <- subset(p, subset = (welldata_field_num == 105))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field105.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 32022.402085
test_delta <- 3.629990
test_theta <- 6.801084
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 32787.5
test_delta <- 1.321544
test_theta <- 0.2430975
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 1.747882e+03
test_delta <- 8.552538e-01
test_theta <- 4.066022e-02
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 4428.621721
test_delta <- 1.913315
test_theta <- 2.571770
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pko)
dev.off()

# Create a dataframe containing only data for Field 72
pk <- subset(p, subset = (welldata_field_num == 72))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field72.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 7178.6706538
test_delta <- 3.3239009
test_theta <- 0.5754223
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 1641.171183
test_delta <- 1.110734
test_theta <- 0.346322
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 4.852456e+03
test_delta <- 2.184503e+00
test_theta <- 7.135896e-02
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 2370.725505
test_delta <- 1.321261
test_theta <- 0.386710
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pko)
dev.off()

# Create a dataframe containing only data for Field 590
pk <- subset(p, subset = (welldata_field_num == 590))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field590.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 7178.6706538
test_delta <- 3.3239009
test_theta <- 0.5754223
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 227.2630444
test_delta <- 2.6207542
test_theta <- 0.4429381
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 4.852456e+03
test_delta <- 2.184503e+00
test_theta <- 7.135896e-02
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 2100.4479578
test_delta <- 1.6341804
test_theta <- 0.6916781
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pko)
dev.off()

# Create a dataframe containing only data for Field 55
pk <- subset(p, subset = (welldata_field_num == 55))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field55.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 7178.6706538
test_delta <- 3.3239009
test_theta <- 0.5754223
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 15082.703377
test_delta <- 2.721793
test_theta <- 2.956527
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 1.161969e+04
test_delta <- 7.599038e-01
test_theta <- 6.101237e-02
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 2100.4479578
test_delta <- 1.6341804
test_theta <- 0.6916781
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
detach(pko)
dev.off()

# Create a dataframe containing only data for Field 718
pk <- subset(p, subset = (welldata_field_num == 718))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field718.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- -85625.881
test_delta <- -3410.258
test_theta <- -28930.286
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "yellow", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 3229.1709124
test_delta <- 1.1537583
test_theta <- 0.1444363
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 1.161969e+04
test_delta <- 7.599038e-01
test_theta <- 6.101237e-02
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 2354.7193245
test_delta <- 1.3942956
test_theta <- 0.3409627
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pko)
dev.off()

# Create a dataframe containing only data for Field 117
pk <- subset(p, subset = (welldata_field_num == 117))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field117.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,1))
# 
# Since there are no gas wells in field 117, skip gas well section
#
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 4.756769e+03
test_delta <- 9.738780e+00
test_theta <- 4.285265e-02
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 4370.7540520
test_delta <- 0.9802673
test_theta <- 1.0630020
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pko)
dev.off()

# Create a dataframe containing only data for Field 1
pk <- subset(p, subset = (welldata_field_num == 1))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field1.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- -85625.881
test_delta <- -3410.258
test_theta <- -28930.286
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 3229.1709124
test_delta <- 1.1537583
test_theta <- 0.1444363
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 1624.778878
  test_delta <- 3.786327
  test_theta <- 2.353937
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "yellow", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 4370.7540520
  test_delta <- 0.9802673
  test_theta <- 1.0630020
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pko)
dev.off()

# Create a dataframe containing only data for Field 60
pk <- subset(p, subset = (welldata_field_num == 60))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field60.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 7620.898314
test_delta <- 3.814989
test_theta <- 4.755576
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "yellow", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 3229.1709124
test_delta <- 1.1537583
test_theta <- 0.1444363
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 1624.778878
test_delta <- 3.786327
test_theta <- 2.353937
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 4425.825988
test_delta <- 2.116982
test_theta <- 5.110954
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pko)
dev.off()

# Create a dataframe containing only data for Field 630
pk <- subset(p, subset = (welldata_field_num == 630))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field630.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 3.537132e+04
  test_delta <- 1.343883e+00
  test_theta <- 2.776321e-01
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 367.215639
  test_delta <- 1.107465
  test_theta <- 0.284849
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 1.624955e+04
  test_delta <- 9.301824e-01
  test_theta <- 2.140955e-01
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 4425.825988
  test_delta <- 2.116982
  test_theta <- 5.110954
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
detach(pko)
dev.off()

# # Create a dataframe containing only data for Field 48
# pk <- subset(p, subset = (welldata_field_num == 48))
# # Split into gas and oil wells
# pko <- subset(pk, subset = (histdata_well_type == "OW"))
# pkg <- subset(pk, subset = (histdata_well_type == "GW"))
# 
# png(filename="C:/Users/Jon/Pictures/field48.png", width = 7.5, height = 7.5, res = 100, units = "in")
# par(mfrow = c(2,2))
# attach(pkg)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pkg)
# attach(pko)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pko)
# dev.off()

# Create a dataframe containing only data for Field 710
pk <- subset(p, subset = (welldata_field_num == 710))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field710.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 3.335540e+04
  test_delta <- 1.170973e+00
  test_theta <- 3.516852e-01
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "blue", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 3805.203513
  test_delta <- 1.856597
  test_theta <- 355.270193
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "yellow", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 1306.385403
  test_delta <- 5.740213
  test_theta <- 2.519364
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "yellow", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 11128.277727
  test_delta <- 2.574266
  test_theta <- 25.964119
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "yellow", lwd = 2)
detach(pko)
dev.off()

# # Create a dataframe containing only data for Field 35
# pk <- subset(p, subset = (welldata_field_num == 35))
# # Split into gas and oil wells
# pko <- subset(pk, subset = (histdata_well_type == "OW"))
# pkg <- subset(pk, subset = (histdata_well_type == "GW"))
# 
# png(filename="C:/Users/Jon/Pictures/field35.png", width = 7.5, height = 7.5, res = 100, units = "in")
# par(mfrow = c(2,2))
# attach(pkg)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pkg)
# attach(pko)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pko)
# dev.off()

# # Create a dataframe containing only data for Field 18
# pk <- subset(p, subset = (welldata_field_num == 18))
# # Split into gas and oil wells
# pko <- subset(pk, subset = (histdata_well_type == "OW"))
# pkg <- subset(pk, subset = (histdata_well_type == "GW"))
# 
# png(filename="C:/Users/Jon/Pictures/field18.png", width = 7.5, height = 7.5, res = 100, units = "in")
# par(mfrow = c(2,2))
# attach(pkg)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pkg)
# attach(pko)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pko)
# dev.off()

# # Create a dataframe containing only data for Field 132
# pk <- subset(p, subset = (welldata_field_num == 132))
# # Split into gas and oil wells
# pko <- subset(pk, subset = (histdata_well_type == "OW"))
# pkg <- subset(pk, subset = (histdata_well_type == "GW"))
# 
# png(filename="C:/Users/Jon/Pictures/field132.png", width = 7.5, height = 7.5, res = 100, units = "in")
# par(mfrow = c(2,2))
# attach(pkg)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pkg)
# attach(pko)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pko)
# dev.off()

# # Create a dataframe containing only data for Field 40
# pk <- subset(p, subset = (welldata_field_num == 40))
# # Split into gas and oil wells
# pko <- subset(pk, subset = (histdata_well_type == "OW"))
# pkg <- subset(pk, subset = (histdata_well_type == "GW"))
# 
# png(filename="C:/Users/Jon/Pictures/field40.png", width = 7.5, height = 7.5, res = 100, units = "in")
# par(mfrow = c(2,2))
# attach(pkg)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pkg)
# attach(pko)
# plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
# test_x <- seq(from = 0, to = max(ptime))
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
# test_alpha <- 
#   test_delta <- 
#   test_theta <- 
# decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
#   test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
# lines(test_x, test_y, col = "blue", lwd = 2)
# detach(pko)
# dev.off()

# Create a dataframe containing only data for Field 665
pk <- subset(p, subset = (welldata_field_num == 665))
# Split into gas and oil wells
pko <- subset(pk, subset = (histdata_well_type == "OW"))
pkg <- subset(pk, subset = (histdata_well_type == "GW"))

png(filename="C:/Users/Jon/Pictures/field665.png", width = 7.5, height = 7.5, res = 100, units = "in")
par(mfrow = c(2,2))
attach(pkg)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Gas Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 32022.402085
  test_delta <- 3.629990
  test_theta <- 6.801084
decline_coef_hgg <- rbind(decline_coef_hgg, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "yellow", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Gas Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 3069.366085
  test_delta <- 2.445221
  test_theta <- 11.845062
decline_coef_hog <- rbind(decline_coef_hog, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "yellow", lwd = 2)
detach(pkg)
attach(pko)
plot(ptime, proddata_gas_prod, type = "p", pch = 1, main = "Gas from Oil Wells", xlab = "Time (months)", ylab = "Gas Production (MCF)")
test_x <- seq(from = 0, to = max(ptime))
test_alpha <- 1306.385403
  test_delta <- 5.740213
  test_theta <- 2.519364
decline_coef_hgo <- rbind(decline_coef_hgo, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "red", lwd = 2)
plot(ptime, proddata_oil_prod, type = "p", pch = 1, main = "Oil from Oil Wells", xlab = "Time (months)", ylab = "Oil Production (bbl)")
test_alpha <- 10464.805692
  test_delta <- 2.731841
  test_theta <- 34.921622
decline_coef_hoo <- rbind(decline_coef_hoo, c(test_alpha, test_delta, test_theta))
  test_y <- test_alpha * (1 + test_theta * test_delta * test_x)^(-1/test_theta)
lines(test_x, test_y, col = "yellow", lwd = 2)
detach(pko)
dev.off()