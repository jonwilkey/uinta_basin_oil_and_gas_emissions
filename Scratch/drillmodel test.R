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
dr <- which(d$month == as.Date("1990-01-01")):which(d$month == as.Date("2009-12-01"))
#dr <- which(d$month == 2010):which(d$month == 2014)
init <- d$prior[dr[1]]

# Get LHS sample points
spLHS <- optimumLHS(n = 4*(2^4), k = 4, maxSweeps = 5, eps = 0.05)

parLHS <- matrix(c(qunif(spLHS[,1],-1,1),
                   qunif(spLHS[,2],-5,5),
                   qunif(spLHS[,3],-1,2),
                   qunif(spLHS[,4],-25,25)),
                 nrow = nrow(spLHS), ncol = ncol(spLHS))

parR <- matrix(0, nrow = nrow(parLHS), ncol = ncol(parLHS))
RSSr <- rep(0,nrow(parR))
for (i in 1:nrow(spLHS)) {
  temp <-     optim(par = parLHS[i,], fn = min.RSS, d = d[dr,], init = init)
  parR[i,] <- temp$par
  RSSr[i] <-  temp$value
}

parR <- parR[which.min(RSSr),]
test <- EDM(OP = d$OP[dr], GP = d$GP[dr], par = parR, init = init)

# Main plot with actual drilling schedule
plot(d$month[dr], d$wells[dr],
     #ylim = c(0,1e3),
     type = "l",
     xlab = "Date (monthly time steps)",
     #xlab = "Year",
     ylab = "Total Wells Drilled (oil, gas, or dry)",
     main = "Drilling Schedule Model")

# Add line for model fit results
lines(d$month[dr], test, col = "red")

# Add text line with equation
mtext(paste("W = (",
            round(parR[1],3), ")*OP + (",
            round(parR[2],3), ")*GP + (",
            #round(parR[3],3), ")*PriorW + (",
            round(parR[3],3), ")",
            "  R^2 = ", round(rsquared(obs = d$wells[dr], pred = test),3),
            sep = ""))

# Legend
legend("topleft", c("Actual", "Fit"), lty = c(1,1), col = c("black","red"))