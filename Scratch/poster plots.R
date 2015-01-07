pdf(file.path("C:/Users/Jon/Pictures/", "DCA ideal vs typical.pdf"))
# Ideal
temp <- subset(p, subset = (p_api == 4301332916), select = c("time", "p_oil_prod"))
names(temp) <- c("time", "prod")
hyp <- nlsLM(formula = prod ~ qo*(1+b*Di*time)^(-1/b),
             data =    temp,
             start =   list(qo = 7e3, b = opt$b.start.oil, Di = opt$Di.start.oil),
             lower =   c(0,0,0),
             upper =   c(Inf,10,Inf),
             control = list(maxiter=1000))
plot(temp,
     type = "p",
     col = "grey",
     xlab = "Time Since First Production (months)",
     ylab = "Oil Production (bbl)",
     main = "Ideal Decline Curve")
mtext("Oil Production from Well API# 43-013-32916")
lines(fitted(hyp), col = "blue")
legend("topright",
       c("Data", "Fit"),
       pch = c(1, NA),
       lty = c(NA, 1),
       col = c("grey", "blue"))

# Typical
temp <- subset(p, subset = (p_api == 4301330801), select = c("time", "p_oil_prod"))
names(temp) <- c("time", "prod")
hyp <- nlsLM(formula = prod ~ qo*(1+b*Di*time)^(-1/b),
             data =    temp,
             start =   list(qo = 7e3, b = opt$b.start.oil, Di = opt$Di.start.oil),
             lower =   c(0,0,0),
             upper =   c(Inf,10,Inf),
             control = list(maxiter=1000))
plot(temp,
     type = "p",
     col = "grey",
     xlab = "Time Since First Production (months)",
     ylab = "Oil Production (bbl)",
     main = "Typical Decline Curve")
mtext("Oil Production from Well API# 43-013-30801")
lines(fitted(hyp), col = "blue")
legend("topright",
       c("Data", "Fit"),
       pch = c(1, NA),
       lty = c(NA, 1),
       col = c("grey", "blue"))

# Binning plot
w <- temp
bin <- 12
temp <- rep(0, times = ceiling(nrow(w)/bin))

# For each interval of "bin" months, get sum of production. Since last bin
# may vary in size, define summation range as beginning of last bin to the
# end of the prodution subset.
for (k in 1:(length(temp)-1)) {
  temp[k] <- sum(w[(bin*(k-1)+1):(bin*k),2])
}
temp[length(temp)] <- sum(w[(bin*k+1):(nrow(w)),2])
barplot(temp,
        #names.arg = seq(1:length(temp)),
        #cex.names = 0.5,
        xlab = "Bins (12-Month Interval)",
        ylab = "Oil Production (bbl)",
        main = "Binning Algorithm Example")
mtext("Oil Production from Well API# 43-013-30801",
      line = 0.3)

dev.off()