min.RSS <- function(d, init, par) {
  
  # Initial wells drilled
  w <- round(par[1]*d$OP[1]+par[2]*d$GP[1]+par[3]*init+par[4])
  #w <- round(par[1]*d$OP[1]+par[2]*d$GP[1]+par[3])
  w <- ifelse(w < 0, 0, w)
  
  # Initial RSS
  RSS <- (w-d$wells[1])^2
  
  for(i in 2:nrow(d)) {
    
    # Wells drilled
    w <- round(par[1]*d$OP[i]+par[2]*d$GP[i]+par[3]*w+par[4])
    #w <- round(par[1]*d$OP[i]+par[2]*d$GP[i]+par[3])
    w <- ifelse(w < 0, 0, w)
    
    RSS <- (w-d$wells[i])^2+RSS
  }
  
  return(RSS)
}

EDM <- function(OP, GP, par, init) {
  
  # Initial wells drilled
  w <- round(par[1]*OP[1]+par[2]*GP[1]+par[3]*init+par[4])
  #w <- round(par[1]*OP[1]+par[2]*GP[1]+par[3])
  w <- ifelse(w < 0, 0, w)
  
  for(i in 2:length(OP)) {
    
    # Wells drilled
    w <- c(w, round(par[1]*OP[i]+par[2]*GP[i]+par[3]*w[length(w)]+par[4]))
    #w <- c(w, round(par[1]*OP[i]+par[2]*GP[i]+par[3]))
    w <- ifelse(w < 0, 0, w)
  }
  
  return(w)
}

rsquared <- function(obs, pred) {
  rsq <- 1-sum((obs-pred)^2)/sum((obs-mean(obs))^2)
  return(rsq)
}

RSS <- function(obs, pred) {
  RSS <- sum((obs-pred)^2)
  return(RSS)
}

err <- function(obs, pred) {
  err <- 1-pred/obs
  return(err)
}

as.year <- function(x) as.numeric(floor(as.yearmon(x)))

d <- drillModelData
# w.z <- zoo(d[,2], as.Date(d[,1])); w.z <- aggregate(w.z, as.year, sum)
# p.z <- zoo(d[,3], as.Date(d[,1])); p.z <- aggregate(p.z, as.year, sum)
# op.z <- zoo(d[,4], as.Date(d[,1])); op.z <- aggregate(op.z, as.year, mean)
# gp.z <- zoo(d[,5], as.Date(d[,1])); gp.z <- aggregate(gp.z, as.year, mean)
# d <- data.frame(month = as.year(index(w.z)),
#                 wells = coredata(w.z),
#                 prior = c(0,coredata(w.z)[1:(length(w.z)-1)]),
#                 OP =    coredata(op.z),
#                 GP =    coredata(gp.z))

# Get LHS sample points
spLHS <- optimumLHS(n = 128, k = 4)

parLHS <- matrix(c(qunif(spLHS[,1],-1,1),
                   qunif(spLHS[,2],-5,5),
                   qunif(spLHS[,3],-1,2),
                   qunif(spLHS[,4],-25,25)),
                 nrow = nrow(spLHS), ncol = ncol(spLHS))

# Start/stop data.frame
tss <- data.frame(start = seq.Date(from = as.Date("1978-01-01"), to = as.Date("2000-01-01"), by = "year"),
                  tstop = seq.Date(from = as.Date("1987-12-01"), to = as.Date("2009-12-01"), by = "year"))

# Define results data.frame
rm1 <- matrix(0, nrow = nrow(tss), ncol = 60)
rm2 <- rm1
rm3 <- rm1
rcoef <- matrix(0, nrow = nrow(tss), ncol = 9)

# Start PDF plot
pdf(file.path(path$plot, "Drilling model fit tests.pdf -10yr train -all data.pdf"), width = 14, height = 7)
par(mfcol = c(1, 2))

# For loop
for (i in 1:nrow(tss)) {
  
  # Get index of dates
  dr <- which(d$month == tss$start[i]):which(d$month == tss$tstop[i])
  
  # Get initial well drilling value
  init <- d$prior[dr[1]]
  
  # Run standard model fit
  parR <- matrix(0, nrow = nrow(parLHS), ncol = ncol(parLHS))
  RSSr <- rep(0,nrow(parR))
  for (j in 1:nrow(spLHS)) {
    temp <-     optim(par = parLHS[j,], fn = min.RSS, d = d[dr,], init = init)
    parR[j,] <- temp$par
    RSSr[j] <-  temp$value
  }
  parR <- parR[which.min(RSSr),]
  
  # Calculate standard model result
  test <- EDM(OP = d$OP[dr], GP = d$GP[dr], par = parR, init = init)
  
  # Fit other price models
  opm <- lm(d$wells[dr]~d$OP[dr-1])
  gpm <- lm(d$wells[dr]~d$OP[dr-1]+d$GP[dr-1])
  
  # Main plot with Training drilling schedule
  plot(d$month[dr], d$wells[dr],
       ylim = c(0,110),
       type = "l",
       xlab = "Date (monthly time steps)",
       #xlab = "Year",
       ylab = "Total Wells Drilled (oil, gas, or dry)")
  title(main = "Training Fit", line = 3)
  
  # Add line for model fit results
  lines(d$month[dr], test, col = "red", lty = 2)
  lines(d$month[dr], fitted(opm), col = "blue", lty = 2)
  lines(d$month[dr], fitted(gpm), col = "green", lty = 2)
  
  # Add text line with equation
  mtext(paste("W = (",
              round(parR[1],3), ")*OP + (",
              round(parR[2],3), ")*GP + (",
              round(parR[3],3), ")*PriorW + (",
              round(parR[3],3), ")",
              "  R^2 = ", round(rsquared(obs = d$wells[dr], pred = test),3),
              "  RSS = ", round(RSS(obs = d$wells[dr], pred = test)),
              sep = ""),
        line = 2)
  
  mtext(paste("W(t) = (",
              round(coefficients(opm)[2],3), ")*OP(t-1) + (",
              round(coefficients(opm)[1],3), ")",
              "   R^2 = ", round(summary(opm)$r.squared, 3),
              "   RSS = ", round(RSS(obs = d$wells[dr], pred = fitted(opm))),
              sep = ""),
        line = 0)
  
  mtext(paste("W(t) = (",
              round(coefficients(gpm)[2],3), ")*OP(t-1) + (",
              round(coefficients(gpm)[3],3), ")*GP(t-1) + (",
              round(coefficients(gpm)[1],3), ")",
              "   R^2 = ", round(summary(gpm)$r.squared, 3),
              "   RSS = ", round(RSS(obs = d$wells[dr], pred = fitted(gpm))),
              sep = ""),
        line = 1)
  
  # Legend
  legend("topleft", c("Actual", "Prior", "EP", "OP Only"), lty = c(1,2,2,2), col = c("black","red","green","blue"))
  
  # Main plot with Test drilling schedule
  tdr <- max(dr)+1:60
  test <- EDM(OP = d$OP[tdr], GP = d$GP[tdr], par = parR, init = d$wells[max(dr)])
  test1 <- coefficients(opm)[2]*d$OP[tdr-1]+coefficients(opm)[1]
  test2 <- coefficients(gpm)[2]*d$OP[tdr-1]+coefficients(gpm)[3]*d$GP[tdr-1]+coefficients(gpm)[1]
  plot(d$month[tdr], d$wells[tdr],
       ylim = c(0,110),
       type = "l",
       xlab = "Date (monthly time steps)",
       #xlab = "Year",
       ylab = "Total Wells Drilled (oil, gas, or dry)")
  title(main = "Test Validation", line = 3)
  
  # Add line for model fit results
  lines(d$month[tdr], test, col = "red", lty = 2)
  lines(d$month[tdr], test1, col = "blue", lty = 2)
  lines(d$month[tdr], test2, col = "green", lty = 2)
  
  # Add text line with equation
  mtext(paste("W = (",
              round(parR[1],3), ")*OP + (",
              round(parR[2],3), ")*GP + (",
              round(parR[3],3), ")*PriorW + (",
              round(parR[3],3), ")",
              "  rE = ", round(mean(err(obs = d$wells[tdr], pred = test)),3),
              "  RSS = ", round(RSS(obs = d$wells[tdr], pred = test)),
              sep = ""),
        line = 2)
  
  mtext(paste("W(t) = (",
              round(coefficients(opm)[2],3), ")*OP(t-1) + (",
              round(coefficients(opm)[1],3), ")",
              "   rE = ", round(mean(err(obs = d$wells[tdr], pred = test1)),3),
              "   RSS = ", round(RSS(obs = d$wells[tdr], pred = test1)),
              sep = ""),
        line = 0)
  
  mtext(paste("W(t) = (",
              round(coefficients(gpm)[2],3), ")*OP(t-1) + (",
              round(coefficients(gpm)[3],3), ")*GP(t-1) + (",
              round(coefficients(gpm)[1],3), ")",
              "   rE = ", round(mean(err(obs = d$wells[tdr], pred = test2)),3),
              "   RSS = ", round(RSS(obs = d$wells[tdr], pred = test2)),
              sep = ""),
        line = 1)
  
  # Legend
  legend("topleft", c("Actual", "Prior", "EP", "OP Only"), lty = c(1,2,2,2), col = c("black","red","green","blue"))
  
  # Extract results
  rm1[i,] <- err(obs = d$wells[tdr], pred = test)
  rm2[i,] <- err(obs = d$wells[tdr], pred = test1)
  rm3[i,] <- err(obs = d$wells[tdr], pred = test2)
  rcoef[i,] <- c(parR, coefficients(opm), coefficients(gpm))
}

# Close PDF
dev.off()

# Plot rE trends
pdf(file.path(path$plot, "Drilling model fit tests -RE trends -10yr train -all data.pdf"))

plot(1:60, apply(rm1, 2, mean)*100,
     type = "l",
     col = "red",
     xlab = "Prediction Time Step (months into future)",
     ylab = "Relative Error (%)",
     main = "Mean of Relative Error of Predictions for 10yr Training Period")
lines(1:60, apply(rm2, 2, mean)*100, col = "blue")
lines(1:60, apply(rm3, 2, mean)*100, col = "green")
legend("topleft", c("Prior", "EP", "OP Only"), lty = 1, col = c("red","green","blue"))

plot(1:60, apply(rm1, 2, sd)*100,
     type = "l",
     col = "red",
     xlab = "Prediction Time Step (months into future)",
     ylab = "Relative Error (%)",
     main = "SD of Relative Error of Predictions for 10yr Training Period")
lines(1:60, apply(rm2, 2, sd)*100, col = "blue")
lines(1:60, apply(rm3, 2, sd)*100, col = "green")
legend("topleft", c("Prior", "EP", "OP Only"), lty = 1, col = c("red","green","blue"))

dev.off()


# Just prior OP work ------------------------------------------------------

test <- lm(d$wells[151:390]~d$OP[150:389]+d$GP[150:389])

subd <- d[150:390,]

plot(wells~month, subd[2:nrow(subd),],
     type = "l",
     xlab = "Date (monthly time steps)",
     ylab = "Total wells Drilled (oil, gas or dry)",
     main = "Drilling Schedule Model")

lines(subd$month[2:nrow(subd)], fitted(test), col = "red")

mtext(paste("W(t) = (",
            round(coefficients(test)[2],3), ")*OP(t-1) + (",
            round(coefficients(test)[3],3), ")*GP(t-1) + (",
            round(coefficients(test)[1],3), "),  ",
            "  R^2 = ", round(summary(test)$r.squared, 3),
            sep = ""))

# Legend
legend("topleft", c("Actual", "Fit"), lty = c(1,1), col = c("black","red"))

# Test Period
w <- rep(0, times = length(391:nrow(d)))
for (i in 391:(nrow(d))) {
  w[i-390] <- coefficients(test)[2]*d$OP[i-1]+coefficients(test)[3]*d$GP[i-1]+coefficients(test)[1]
}

rsquared <- function(obs, pred) {
  rsq <- 1-sum((obs-pred)^2)/sum((obs-mean(obs))^2)
  return(rsq)
}

RSS <- function(obs, pred) {
  RSS <- sum((obs-pred)^2)
  return(RSS)
}

plot(wells~month, d[391:nrow(d),],
     type = "l",
     xlab = "Date (monthly time steps)",
     ylab = "Total wells Drilled (oil, gas or dry)",
     main = "Drilling Schedule Model")

lines(d$month[391:nrow(d)], w, col = "red")

mtext(paste("W(t) = (",
            round(coefficients(test)[2],3), ")*OP(t-1) + (",
            round(coefficients(test)[3],3), ")*GP(t-1) + (",
            round(coefficients(test)[1],3), "),  ",
            "  RSS = ", round(RSS(obs = d$wells[391:nrow(d)], pred = w)),
            sep = ""))

# Legend
legend("topleft", c("Actual", "Fit"), lty = c(1,1), col = c("black","red"))
