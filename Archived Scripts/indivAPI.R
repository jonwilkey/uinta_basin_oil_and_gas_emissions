#-------------------------------------------------------------------------------
# Decline Curve Functions
#-------------------------------------------------------------------------------

# Exponential Function
exponential <- deriv(~ alpha * exp(-x / theta),
                  namevec = c("alpha", "theta"),                  
                  function(x, alpha, theta){}
)

# Hyperbolic Function
hyperbol <- deriv(~ alpha * (1 + theta * delta * x)^(-1/theta),
                  namevec = c("alpha", "theta", "delta"),                  
                  function(x, alpha, theta, delta){}
)

# Harmonic function
harmonic <- deriv(~ alpha / (1 + theta * x),
                  namevec = c("alpha", "theta"),                  
                  function(x, alpha, theta){}
)

# Histograms of production rates at each time step
png("C:/Users/Jon/Pictures/Movie/gasallfields%02d.png",
    width = 5, height = 5, res = 200, units = "in")

for (j in 1:max(pko$ptime)) {
  hist.oil.prod <- subset(pko, subset = (ptime == j))
  hist(hist.oil.prod$proddata_gas_prod,
       freq = TRUE,       
       breaks = 100,
       xlab = "Gas Production Rate (MCF per month)",
       main = "Gas Production - Oil Wells - All Fields")
}

dev.off()

# Plots of all wells
api.list <- sqldf("select distinct proddata_api from p where welldata_field_num = 55")

png("C:/Users/Jon/Pictures/Movie/indivoil%02d.png",
    width = 5, height = 5, res = 200, units = "in")

for (j in 1:length(api.list[,1])) {  
  api.data <- subset(p,
                     proddata_api == api.list[j,1],
                     select = c(ptime,
                                proddata_oil_prod)
                     )
  
  # For Oil
  plot(api.data$ptime, api.data$proddata_oil_prod,
       type = "b",
       pch = 20,
       main = paste("Oil from API# ", api.list[j,1], sep = ""),
       xlab = "Time (months)",
       ylab = "Oil Production (bbl)")
}
dev.off()


# Fitting individual wells
api.list <- sqldf("select distinct proddata_api from p where welldata_field_num = 72")

# Altamont (Field 55) results
# works <- c(5, 10, 22, 25, 48, 56, 70, 73, 80, 82, 83, 86, 91)
# step <- c(8, 12, 14, 19, 20, 27, 30, 32, 33, 36, 37, 50, 53, 55, 59, 62, 65, 68, 72, 78, 81, 84, 85, 92, 93, 94, 95, 96, 97, 98, 101, 106, 109)

# Field 72 Brundage Canyon Works
# works <- c(6, 14, 15, 21, 33, 49, 52, 55, 57, 58, 60, 73, 74, 75, 76, 78, 80, 81, 96)
# step <- c(3, 12, 27, 28, 29, 31, 48, 62, 65, 67, 77)
fail <- c(1, 2, 4, 5, 6, 7, 8, 9,
          10, 11, 13, 16, 17, 18, 19,
          20, 22, 23, 24, 25, 
          30, 32, 34, 35, 36, 37, 38, 39,
          40, 41, 42, 43, 44, 45, 46, 47,
          50, 53, 54, 56, 59,
          61, 63, 64, 66, 68, 69,
          70, 71, 72, 79,
          82, 83, 84, 85, 86, 87, 88, 89,
          90, 91, 92, 93, 94, 95, 97, 98, 99)

png("C:/Users/Jon/Pictures/Movie/BrundageCanyon%02d.png",
    width = 5, height = 5, res = 200, units = "in")

api.coef.exp <- NULL
api.coef.hyp <- NULL
api.coef.har <- NULL

for (j in fail) {
  
  api.data <- subset(p, proddata_api == api.list[j,1], select = c(ptime, proddata_oil_prod))
  api.data$proddata_oil_prod <- api.data$proddata_oil_prod / api.data$proddata_oil_prod[1]
  hoo.exp <- nls(proddata_oil_prod ~ exponential(ptime, alpha, theta),
             start = c(alpha = 1, theta = 1),
             trace = FALSE,
             data = api.data,
             control = list(maxiter = 500, tol = 1e-06, minFactor = 1/(1024),
                            printEval = FALSE, warnOnly = TRUE))
#   hoo.hyp <- nls(proddata_oil_prod ~ hyperbol(ptime, alpha, delta, theta),
#                  start = c(alpha = 2, delta = 1, theta = 0.8),
#                  trace = FALSE,
#                  data = api.data,
#                  control = list(maxiter = 500, tol = 1e-06, minFactor = 1/(1024),
#                                 printEval = FALSE, warnOnly = TRUE))
  
  hoo.har <- nls(proddata_oil_prod ~ harmonic(ptime, alpha, theta),
                 start = c(alpha = 1, theta = 1),
                 trace = FALSE,
                 data = api.data,
                 control = list(maxiter = 500, tol = 1e-06, minFactor = 1/(1024),
                                printEval = FALSE, warnOnly = TRUE))
  
  api.coef.exp <- rbind(api.coef.exp, coef(hoo.exp))
#   api.coef.hyp <- rbind(api.coef.hyp, coef(hoo.hyp))
  api.coef.har <- rbind(api.coef.har, coef(hoo.har))
  
  plot(api.data$ptime, api.data$proddata_oil_prod,
       type = "b",
       pch = 20,
       main = paste("Oil from API# ", api.list[j,1], sep = ""),
       xlab = "Time (months)",
       ylab = "Normalized Oil Production")
  test_x <- seq(from = 0, to = max(api.data$ptime))
  y.exp <- coef(hoo.exp)[1] * exp(-test_x / coef(hoo.exp)[2])
#   y.hyp <- coef(hoo.hyp)[1] * (1 + coef(hoo.hyp)[3] * coef(hoo.hyp)[2] * test_x)^(-1/coef(hoo.hyp)[2])
  y.har <- coef(hoo.har)[1] / (1 + coef(hoo.har)[2] * test_x)
  lines(test_x, y.exp, col = "red", lwd = 2)
#   lines(test_x, y.hyp, col = "blue", lwd = 2)
  lines(test_x, y.har, col = "green", lwd = 2)
  legend("topright", c("Actual", "Exp", "Har"), lty = c(1,1,1), col = c("black", "red", "green"))
#   legend("topright", c("Actual", "Exp", "Hyp", "Har"), lty = c(1,1,1), col = c("black", "red", "blue", "green"))
#   text(max(api.data$ptime),0.9, paste("alpha = ", round(coef(hoo)[1],3), sep = ""), adj = c(1,0))
#   text(max(api.data$ptime),0.8, paste("delta = ", round(coef(hoo)[2],3), sep = ""), adj = c(1,0))
#   text(max(api.data$ptime),0.7, paste("theta = ", round(coef(hoo)[3],3), sep = ""), adj = c(1,0))
  
}

plot(density(api.coef.exp[,1]),
     xlab = "Value",
     main = "Alpha - Exponential Fit")
plot(density(api.coef.exp[,2]),
     xlab = "Value",
     main = "Theta - Exponential Fit")

# plot(density(api.coef.hyp[,1]),
#      xlab = "Value",
#      main = "Alpha - Hyperbolic Fit")
# plot(density(api.coef.hyp[,2]),
#      xlab = "Value",
#      main = "Delta - Hyperbolic Fit")
# plot(density(api.coef.hyp[,3]),
#      xlab = "Value",
#      main = "Theta - Hyperbolic Fit")

plot(density(api.coef.har[,1]),
     xlab = "Value",
     main = "Alpha - Harmonic Fit")
plot(density(api.coef.har[,2]),
     xlab = "Value",
     main = "Theta - Harmonic Fit")

dev.off()