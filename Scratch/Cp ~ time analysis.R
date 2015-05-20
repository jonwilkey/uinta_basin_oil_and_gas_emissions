
# Data prep ---------------------------------------------------------------

# Select subset of mo/mg that succesfully fitted both first/last curves, that
# have dates of first production, and whose Cp values are within the CDF limits
# of Cp for oil/gas
v1.ow <- subset(mo, subset = (Qfit.1 == 1 & Qfit.2 == 1 & !is.na(firstprod) & Cp.1 <= 30e3))
v1.gw <- subset(mg, subset = (Qfit.1 == 1 & Qfit.2 == 1 & !is.na(firstprod) & Cp.1 <= 200e3))

# Add diff time value in months since Jan. 1984
v1.ow$dt <- with(v1.ow, round(as.numeric(difftime(firstprod, as.Date("1984-01-01"), units = "days"))*(12/365.25)))
v1.gw$dt <- with(v1.gw, round(as.numeric(difftime(firstprod, as.Date("1984-01-01"), units = "days"))*(12/365.25)))

# Aggregate to annual basis
vow <- v1.ow
vgw <- v1.gw
for(i in 1:ceiling(max(v1.ow$dt)/12)) {
  ind.ow <- which(vow$dt >= (12*(i-1)+1) & vow$dt <= (12*i))
  ind.gw <- which(vgw$dt >= (12*(i-1)+1) & vgw$dt <= (12*i))
  vow$dt[ind.ow] <- i
  vgw$dt[ind.gw] <- i
}

# # Oil ----------------------------------------------------------------
# 
# # start pdf plot
# pdf(file.path(path$plot, "Cp values vs time.pdf"))
# 
# # Entire basin
# plot(Cp.1 ~ dt, v1.ow,
#      col = "#00000033",
#      xlab = "Time (months since Jan. 1984)",
#      ylab = "Cp value",
#      main = "Oil Cp Values as f(t) - Basin")
# 
# points(aggregate(Cp.1~dt, v1.ow, median), pch = 20, col = "red", cex = 0.75)
# 
# # Entire basin
# plot(Cp.1 ~ dt, v1.ow,
#      xlim = c(200, 359),
#      ylim = c(0, 7.5e3),
#      col = "#00000033",
#      xlab = "Time (months since Jan. 1984)",
#      ylab = "Cp value",
#      main = "Oil Cp Values as f(t) - Basin - Zoomed In")
# 
# points(aggregate(Cp.1~dt, v1.ow, median), pch = 20, col = "red", cex = 0.75)
# tfit <- lm(Cp.1~dt,v1.ow[v1.ow$dt>=225,])
# lines(225:359, 225:359*coef(tfit)[2]+coef(tfit)[1], col = "blue")
# 
# # # Fit
# # basin <- lm(Cp.1~dt-1, v1.ow)
# # 
# # # Time steps
# # tsteps <- min(v1.ow$dt):max(v1.ow$dt)
# # 
# # # Lines
# # lines(tsteps, coef(basin)*tsteps, col = "red", lwd = 2)
# # mtext(paste("Slope =", round(coef(basin), 3)))
# 
# v1.ow999 <- v1.ow
# 
# # Each Field
# for (i in 1:(length(field)-1)) {
#   
#   ind <- which(v1.ow$w_field_num == field[i])
#   
#   plot(Cp.1 ~ dt, v1.ow[ind,],
#        col = "#00000033",
#        xlab = "Time (months since Jan. 1984)",
#        ylab = "Cp value",
#        main = paste("Oil Cp Values as f(t) - Field", field[i]))
#   
#   #points(aggregate(Cp.1~dt, v1.ow[ind,], median), pch = 20, col = "red", cex = 0.75)
#   
# #   # Fit
# #   temp <- lm(Cp.1~dt-1, v1.ow[ind,])
# #   
# #   # Time steps
# #   tsteps <- min(v1.ow$dt[ind]):max(v1.ow$dt[ind])
# #   
# #   # Fit line
# #   lines(tsteps, coef(temp)*tsteps, col = "red", lwd = 2)
# #   mtext(paste("Slope =", round(coef(temp), 3)))
#   
#   # Drop elements from v1.ow999
#   v1.ow999 <- v1.ow999[-ind,]
# }
# 
# # Field 999
# plot(Cp.1 ~ dt, v1.ow999,
#      col = "#00000033",
#      xlab = "Time (months since Jan. 1984)",
#      ylab = "Cp value",
#      main = paste("Oil Cp Values as f(t) - Field", field[i+1]))
# points(aggregate(Cp.1~dt, v1.ow999, median), pch = 20, col = "red", cex = 0.75)
# 
# 
# # Gas ---------------------------------------------------------------------
# 
# # Entire basin
# plot(Cp.1 ~ dt, v1.gw,
#      col = "#00000033",
#      xlab = "Time (months since Jan. 1984)",
#      ylab = "Cp value",
#      main = "Gas Cp Values as f(t) - Basin")
# 
# points(aggregate(Cp.1~dt, v1.gw, median), pch = 20, col = "red", cex = 0.75)
# 
# # # Fit
# # basin <- lm(Cp.1~dt-1, v1.ow)
# # 
# # # Time steps
# # tsteps <- min(v1.ow$dt):max(v1.ow$dt)
# # 
# # # Lines
# # lines(tsteps, coef(basin)*tsteps, col = "red", lwd = 2)
# # mtext(paste("Slope =", round(coef(basin), 3)))
# 
# v1.gw999 <- v1.gw
# 
# # Each Field
# for (i in 1:(length(field)-1)) {
#   
#   ind <- which(v1.gw$w_field_num == field[i])
#   
#   plot(Cp.1 ~ dt, v1.gw[ind,],
#        col = "#00000033",
#        xlab = "Time (months since Jan. 1984)",
#        ylab = "Cp value",
#        main = paste("Gas Cp Values as f(t) - Field", field[i]))
#   
#   #points(aggregate(Cp.1~dt, v1.ow[ind,], median), pch = 20, col = "red", cex = 0.75)
#   
#   #   # Fit
#   #   temp <- lm(Cp.1~dt-1, v1.ow[ind,])
#   #   
#   #   # Time steps
#   #   tsteps <- min(v1.ow$dt[ind]):max(v1.ow$dt[ind])
#   #   
#   #   # Fit line
#   #   lines(tsteps, coef(temp)*tsteps, col = "red", lwd = 2)
#   #   mtext(paste("Slope =", round(coef(temp), 3)))
#   
#   # Drop elements from v1.ow999
#   v1.gw999 <- v1.gw999[-ind,]
# }
# 
# # Field 999
# plot(Cp.1 ~ dt, v1.gw999,
#      col = "#00000033",
#      xlab = "Time (months since Jan. 1984)",
#      ylab = "Cp value",
#      main = paste("Gas Cp Values as f(t) - Field", field[i+1]))
# points(aggregate(Cp.1~dt, v1.gw999, median), pch = 20, col = "red", cex = 0.75)
# 
# dev.off()


# Oil - Annual Basis ------------------------------------------------------

lc <- rainbow(5)

# Start PDF plot
pdf(file.path(path$plot, "Cp CDFs vs time p3.pdf"))

# Plot Baseline
plot(CDFq(vow$Cp.1[vow$dt == 11], opt$xq),
     type = "l",
     col = lc[1],
     xlim = c(0, 30e3),
     xlab = "Cp Value",
     ylab = "Cumulative Probability",
     main = "CDF for Cp for Oil Wells by Time (years) Since 1984")

for (i in 12:15) {
  lines(CDFq(vow$Cp.1[vow$dt == i], opt$xq), col = lc[i-10])
}

for (i in 16:20) {
  lines(CDFq(vow$Cp.1[vow$dt == i], opt$xq), col = lc[i-15], lty = 2)
}

legend("bottomright",
       ncol = 2,
       as.character(11:20),
       col = rep(lc,2),
       lty = c(rep(1,5),rep(2,5)))


# Gas - Annual Basis ------------------------------------------------------

# Plot Baseline
plot(CDFq(vgw$Cp.1[vgw$dt == 11], opt$xq),
     type = "l",
     col = lc[1],
     xlim = c(0, 200e3),
     xlab = "Cp Value",
     ylab = "Cumulative Probability",
     main = "CDF for Cp for Gas Wells by Time (years) Since 1984")

for (i in 12:15) {
  lines(CDFq(vgw$Cp.1[vgw$dt == i], opt$xq), col = lc[i-10])
}

for (i in 16:20) {
  lines(CDFq(vgw$Cp.1[vgw$dt == i], opt$xq), col = lc[i-15], lty = 2)
}

legend("bottomright",
       ncol = 2,
       as.character(11:20),
       col = rep(lc,2),
       lty = c(rep(1,5),rep(2,5)))

dev.off()


# Annual trends -----------------------------------------------------------

aow <- data.frame(CDF = opt$xq, CDFq(vow$Cp.1[vow$dt == 1], opt$xq)[,1])
for(i in 2:30) {
  aow <- cbind(aow, CDFq(vow$Cp.1[vow$dt == i], opt$xq)[,1])
}
names(aow) <- c("CDF", "v1984", "v1985", "v1986", "v1987", "v1988", "v1989",
                "v1990", "v1991", "v1992", "v1993", "v1994", "v1995", "v1996", "v1997", "v1998", "v1999",
                "v2000", "v2001","v2002", "v2003", "v2004", "v2005", "v2006", "v2007", "v2008", "v2009",
                "v2010", "v2011", "v2012", "v2013")

taw <- rep(0, ncol(aow)-1)
for (i in 1:(ncol(aow)-1)) {
  taw[i] <- summary(aow[,i+1]/aow[,i])[4]
}

