pdf(file.path(path$plot, "DCA binning algorithm v2.pdf"))

apilist <- unique(stsp$well.p_api.i.)
for (i in 1:length(apilist)) {
  # Select production records for well "i"
  w <- subset(ps,
              subset = (p_api == apilist[i]),
              select = c("time", "p_oil_prod"))
  
  # Remove zeros in production record (and single negative production record)
  w <- w[-which(w$p_oil_prod <= 0),]
  
  # If statement to restrict possibility that too few records exist
  if (nrow(w) >= minProdRec) {
    par(mfrow = c(2,2))
    
    plot(w$p_oil_prod,
         col = "cadetblue4",
         main = paste("Oil Production from API#", apilist[i]),
         xlab = "Time in Production (months)",
         ylab = "Oil Production (bbl)")
    lines(smooth(w$p_oil_prod))
    
    plot(w$p_oil_prod,
         col = "cadetblue4",
         main = "6-Month Binning Breaks",
         xlab = "Time in Production (months)",
         ylab = "Oil Production (bbl)")
    lines(smooth(w$p_oil_prod))
    abline(v = stsp$startT[which(stsp[,1] == apilist[i] & stsp[,2] == 6)],
           col = "green")
    abline(v = stsp$stopT[which(stsp[,1] == apilist[i] & stsp[,2] == 6)],
           col = "green", lty = 2)
    
    plot(w$p_oil_prod,
         col = "cadetblue4",
         main = "12-Month Binning Breaks",
         xlab = "Time in Production (months)",
         ylab = "Oil Production (bbl)")
    lines(smooth(w$p_oil_prod))
    abline(v = stsp$startT[which(stsp[,1] == apilist[i] & stsp[,2] == 12)],
           col = "red")
    abline(v = stsp$stopT[which(stsp[,1] == apilist[i] & stsp[,2] == 12)],
           col = "red", lty = 2)
    
    plot(w$p_oil_prod,
         col = "cadetblue4",
         main = "24-Month Binning Breaks",
         xlab = "Time in Production (months)",
         ylab = "Oil Production (bbl)")
    lines(smooth(w$p_oil_prod))
    abline(v = stsp$startT[which(stsp[,1] == apilist[i] & stsp[,2] == 24)],
           col = "blue")
    abline(v = stsp$stopT[which(stsp[,1] == apilist[i] & stsp[,2] == 24)],
           col = "blue", lty = 2)
  }
}

dev.off()