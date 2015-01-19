pdf(file.path(path$plot, "Revised plots for AQU part 1.pdf"))

# Font/Axis size increase options -----------------------------------------
labSize <- 1.25
axisSize <- 1.25
lineSize <- 1.5


# Slide 3 -----------------------------------------------------------------
ps <- subset(p, subset = (p_api == 4301330781), select = c("time","p_oil_prod"))
plot(ps,
     cex.lab = labSize,
     cex.axis = axisSize,
     xlab = "Time Since First Production (months)",
     ylab = "Oil Production (bbl)",
     main = "Oil Production from API # 4301330781")

# Slide 8 - Wells drilled fit vs. actual ----------------------------------
# Add column 'drill_month' as truncated to month version of h_first_prod
p$drill_month <- as.yearmon(p[,"h_first_prod"])

# Create dataframe containing dates of 1st production for each unique APD #
# and the field it is located in
well <- sqldf("select distinct(p_api), drill_month, h_well_type, h_td_md
              from p")

# Drop NA observations
well <- na.omit(well)

# Only wells (1) inside price history timeframe (and prior month), (2) with
# depths > 0, and (3) that were drilled with the intention of being producing
# wells (i.e. oil wells, gas wells, or dry wells).
well <- subset(well, subset = (drill_month >= (min(eia.hp$month)-1/12) &
                                 drill_month <= max(eia.hp$month) &
                                 h_td_md > opt$min.well.depth &
                                 (h_well_type == "OW" |
                                    h_well_type == "GW" |
                                    h_well_type == "D")))

# Determine total number of wells drilled each year
drill <- sqldf("select drill_month, count(p_api)
                 from well
                 group by drill_month")

m <- merge(x = eia.hp, y = drill,
           by.x = "month", by.y = "drill_month",
           all = TRUE)

# Save two vectors for (1) wells drilled in prior year (prior) and (2) wells
# drilled in current year (wells)
prior <- m[(1:(nrow(m)-1)),4]
wells <- m[(2:nrow(m)),4]

# Create data.frame with all the data need to run lm()
analysis <- data.frame(as.Date(eia.hp[,1]), wells, prior, eia.hp[,c(2,3)])
names(analysis) <- c("month", "wells", "prior", "OP", "GP")

# Drop NA values
analysis <- na.omit(analysis)

# Run lm()
drillModel <- lm(formula = (wells ~ OP + GP + prior),
                 data = analysis)

# Plot - Uncomment if plot is desired
with(analysis,
     plot(month, wells,
          cex.lab = labSize,
          cex.axis = axisSize,
          type = "l",
          lwd = lineSize,
          xlab = "Year",
          ylab = "Total Wells Drilled (oil, gas, or dry)",
          main = "Drilling Schedule Model")
)
with(analysis,
     lines(month, fitted(drillModel),
           lwd = lineSize,
           col = "red")
)
mtext(expression(W==a%.%OP+b%.%GP+c%.%W[n-1]+d))
legend("topleft",
       lwd = lineSize,
       c("Actual", "Fit"),
       lty = c(1,1),
       col = c("black","red"))


# Slide 9 -----------------------------------------------------------------
ps <- subset(p,
             subset = (time != 0 &
                         (p$h_well_type == "OW" |
                            p$h_well_type == "GW") &
                         p$p_days_prod >= opt$minDayProd),
             select = c("p_api",
                        "p_oil_prod",
                        "p_gas_prod",
                        "p_water_prod",
                        "time",
                        "h_well_type",
                        "h_first_prod",
                        "w_field_num",
                        "w_totcum_oil",
                        "w_totcum_gas",
                        "nrec"))

w <- subset(p,
            subset = (p_api == 4301315782 & p$p_days_prod >= opt$minDayProd),
            select = c("time", "p_oil_prod"))

names(w) <- c("time","prod")

source(file.path(path$fun, "binStartStop.R"))

# Run binning algorithm to get first decline curve and last delcine curve
stsp <- binStartStop(w = w,
                     bin = opt$bin,
                     diff.bin.cutoff = opt$diff.bin.cutoff,
                     n.stopB.min = opt$n.stopB.min,
                     n.startT.search = opt$n.startT.search)

# Set hyp object to initial value of NULL
hyp = NULL

# Fit using nlsLM solver with try() wrapper to keep errors from breaking
# the loop
try(hyp <- nlsLM(formula = prod ~ qo*(1+b*Di*time)^(-1/b),
                 data =    w[stsp[1,1]:stsp[1,2],],
                 start =   list(qo = w[stsp[1,1],2], b = opt$b.start.oil, Di = opt$Di.start.oil),
                 lower =   opt$lower.oil,
                 upper =   opt$upper.oil,
                 control = list(maxiter=1000)),
    silent = TRUE)

# Main Plot
plot(w$time, w$prod,
     main = "Oil Production from API # 4301315782",
     xlab = "Time Since First Production (months)",
     ylab = "Oil Production (bbl)",
     cex.lab = labSize,
     cex.axis = axisSize,
     col = "grey")
# Fit line
lines(w[stsp[1,1]:stsp[1,2],1], fitted(hyp),
      col = "blue",
      lwd = lineSize)
# Add legend
legend("topright",
       c("Actual", "Fit"),
       pch = c(1, NA),
       lty = c(NA, 1),
       col = c("grey", "blue"))

w <- subset(ps,
            subset = (p_api == 4301331588),
            select = c("time", "p_oil_prod"))

names(w) <- c("time","prod")

# Run binning algorithm to get first decline curve and last delcine curve
stsp <- binStartStop(w = w,
                     bin = opt$bin,
                     diff.bin.cutoff = opt$diff.bin.cutoff,
                     n.stopB.min = opt$n.stopB.min,
                     n.startT.search = opt$n.startT.search)
hyp1 = NULL

# Fit using nlsLM solver with try() wrapper to keep errors from breaking
# the loop
try(hyp1 <- nlsLM(formula = prod ~ qo*(1+b*Di*time)^(-1/b),
                  data =    w[stsp[1,1]:stsp[1,2],],
                  start =   list(qo = w[stsp[1,1],2], b = opt$b.start.oil, Di = opt$Di.start.oil),
                  lower =   opt$lower.oil,
                  upper =   opt$upper.oil,
                  control = list(maxiter=1000)),
    silent = TRUE)
hyp2 = NULL

# Fit using nlsLM solver with try() wrapper to keep errors from breaking
# the loop
try(hyp2 <- nlsLM(formula = prod ~ qo*(1+b*Di*time)^(-1/b),
                  data =    w[stsp[2,1]:stsp[2,2],],
                  start =   list(qo = w[stsp[2,1],2], b = opt$b.start.oil, Di = opt$Di.start.oil),
                  lower =   opt$lower.oil,
                  upper =   opt$upper.oil,
                  control = list(maxiter=1000)),
    silent = TRUE)

# Main Plot
plot(w$time, w$prod,
     main = "Oil Production from API # 4301331588",
     xlab = "Time Since First Production (months)",
     ylab = "Oil Production (bbl)",
     cex.lab = labSize,
     cex.axis = axisSize,
     col = "grey")

# Else, first curve fitted if hyp1 exists and converged if not NULL
if (exists("hyp1")) {
  if (!is.null(hyp1)) {
    lines(w[stsp[1,1]:stsp[1,2],1], fitted(hyp1), col = "red", lwd = lineSize)
    abline(v = w$time[stsp[1,1]], col = "red", lty = 2, lwd = lineSize)
    abline(v = w$time[stsp[1,2]], col = "red", lty = 2, lwd = lineSize) 
  }
}

# Else, last curve fitted if hyp1 exists and converged if not NULL
if (exists("hyp2")) {
  if (!is.null(hyp2)) {
    lines(w[stsp[2,1]:stsp[2,2],1], fitted(hyp2), col = "green", lwd = lineSize)
    abline(v = w$time[stsp[2,1]], col = "green", lty = 2, lwd = lineSize)
    abline(v = w$time[stsp[2,2]], col = "green", lty = 2, lwd = lineSize)
  }
}

# Add legend
legend("topright",
       c("Actual    ", "Both    ", "First    ", "Last    "),
       pch = c(1, NA, NA, NA),
       lty = c(NA, 1, 1, 1),
       col = c("grey", "blue", "red", "green"))

dev.off()