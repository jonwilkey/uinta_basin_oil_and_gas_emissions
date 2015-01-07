# Load data
load(file.path(path$data, "DCA_fits_v2.rda"))

# Only wells that were fit
mo <- mo[which(mo$fit.1 == 1),]
mg <- mg[which(mg$fit.1 == 1),]

# Start PDF printer
pdf(file.path(path$plot, "DF for DCA coefficients fits vs MC.pdf"), width = 17, height = 11)

# For each field
for (i in 1:length(opt$field)) {
  
  # Get data from fits
  xo <- subset(mo,
               subset = (w_field_num == opt$field[i]),
               select = c("qo.1", "b.1", "Di.1", "tdelay"))
  xg <- subset(mg,
               subset = (w_field_num == opt$field[i]),
               select = c("qo.1", "b.1", "Di.1", "tdelay"))
  
  
  # Get data from simulation
  xoMC <- subset(wsim,
                 subset = (wsim$fieldnum == opt$field[i]),
                 select = c("qo.oil", "b.oil", "Di.oil", "td.oil"))
  xgMC <- subset(wsim,
                 subset = (wsim$fieldnum == opt$field[i]),
                 select = c("qo.gas", "b.gas", "Di.gas", "td.gas"))
  
  # String inputs
  xolabel <- c("Initial Production Rate (bbl)",
              "Decline Exponent (dimensionless)",
              "Initial Decline Rate (dimensionless)",
              "Time Delay (months)")
  xglabel <- c("Initial Production Rate (MCF)",
               "Decline Exponent (dimensionless)",
               "Initial Decline Rate (dimensionless)",
               "Time Delay (months)")
  xomain <- c("qo for oil", "b for oil", "Di for oil", "time delay")
  xgmain <- c("qo for gas", "b for gas", "Di for gas", "time delay")
  
# Plot function -----------------------------------------------------------

funplot <- function(x, xf, xt, xn, xlabel, xmain, xMC) {
  # For each coefficient
  for (j in 1:4) {
    
    # PDF plot of fitted values
    plot(density(x[,j],
                 from = xf[j],
                 to =   xt[j],
                 n =    xn[j]),
         col = "blue",
         xlab = xlabel[j],
         main = paste("PDF for", xmain[j], "from Field", opt$field[i]))
    grid()
    
    # PDF line of MC values
    lines(density(xMC[,j],
                  from = xf[j],
                  to =   xt[j],
                  n =    xn[j]),
          col = "red")
    
    # Legend
    legend("topright",
           c("Fits", "MC"),
           lty = c(1,1),
           col = c("blue", "red"))
    
    #CDF plot of fitted values
    plot(CDF(vector = x[,j],
             from = xf[j],
             to =   xt[j],
             np =   xn[j]),
         type = "l",
         col = "blue",
         xlab = xlabel[j],
         main = paste("CDF for", xmain[j], "from Field", opt$field[i]))
    grid()
    
    # CDF line of MC values
    lines(CDF(vector = xMC[,j],
              from = xf,
              to =   xt[j],
              n =    xn[j]),
          col = "red")
    
    # Legend
    legend("bottomright",
           c("Fits", "MC"),
           lty = c(1,1),
           col = c("blue", "red"))
  }  
}

# Function call to plot oil coefficients
funplot(x      = xo,
        xf     = opt$cdf.oil.from,
        xt     = opt$cdf.oil.to,
        xn     = opt$cdf.oil.np,
        xlabel = xolabel,
        xmain  = xomain,
        xMC    = xoMC)

# Function call to plot oil coefficients
funplot(x      = xg,
        xf     = opt$cdf.gas.from,
        xt     = opt$cdf.gas.to,
        xn     = opt$cdf.gas.np,
        xlabel = xglabel,
        xmain  = xgmain,
        xMC    = xgMC)
}

# Close PDF printer
dev.off()

