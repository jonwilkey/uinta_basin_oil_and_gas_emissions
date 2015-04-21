# Get production of oil/gas from all wells
all.p <- sqldf("select p_rpt_period, sum(p_oil_prod), sum(p_gas_prod)
              from p
              group by p_rpt_period")
names(all.p) <- c("date", "oil", "gas")
all.p <- all.p[which(all.p$date >= opt$tstart & all.p$date <= opt$tstop),]

# Get production of oil/gs from new wells
temp <- subset(p,
               subset = (h_first_prod >= opt$tstart),
               select = c("p_rpt_period", "p_oil_prod", "p_gas_prod"))
new.p <- sqldf("select p_rpt_period, sum(p_oil_prod), sum(p_gas_prod)
              from temp
              group by p_rpt_period")
names(new.p) <- c("date", "oil", "gas")
new.p <- new.p[which(new.p$date >= opt$tstart & new.p$date <= opt$tstop),]

pdf(file.path(path$plot, "priorProd.pdf"))

# Plots
plot(new.p$date, (all.p$oil-new.p$oil),
     ylim = c(500e3, 1.6e6),
     type = "l",
     xlab = "Date (months)",
     ylab = "Oil Production (bbl)",
     main = "Oil from Existing Wells")
prior <- priorProd(hypFF =     hypFF,
                   mo =        mo,
                   mg =        mg,
                   MC.tsteps = opt$MC.tsteps,
                   acut =      0)
lines(new.p$date, colSums(prior$oil), col = "red")
prior <- priorProd(hypFF =     hypFF,
                   mo =        mo,
                   mg =        mg,
                   MC.tsteps = opt$MC.tsteps,
                   acut =      opt$acut)
lines(new.p$date, colSums(prior$oil), col = "blue")
legend("topright",
       c("Actual", "Predicted", "Predicted + Abandoment"),
       lty = 1,
       col = c("black", "red", "blue"))

plot(new.p$date, (all.p$gas-new.p$gas),
     type = "l",
     xlab = "Date (months)",
     ylab = "Gas Production (MCF)",
     main = "Gas from Existing Wells")
lines(new.p$date, colSums(prior$gas), col = "red")
legend("topright",
       c("Actual", "Predicted"),
       lty = 1,
       col = c("black", "red"))

dev.off()
