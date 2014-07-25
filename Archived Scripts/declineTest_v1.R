#-------------------------------------------------------------------------------
# Script Info
#-------------------------------------------------------------------------------
# declineTest_v1.R (Monte Carlo decline curve field simulation test)
# Version 1
# 04/29/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Version History
#-------------------------------------------------------------------------------
# --- Version 1 ---
# 1. Generates sythetic set of coefficients for hyperbolic decline curve,
#    generates CDF, runs MC simulation.

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Load required data files
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Generate Coefficients for Hyperbolic Decline Curve
#-------------------------------------------------------------------------------
a <- rnorm(n = 10^3, mean = 100, sd = 20)
b <- rnorm(n = 10^3, mean = 0.5, sd = 0.1)
c <- rnorm(n = 10^3, mean = 0.5, sd = 0.1)

#-------------------------------------------------------------------------------
# Create CDF
#-------------------------------------------------------------------------------
# Density criteria
# Number of points
n = 10^4
# Lower and upper limits for each coefficient
from.a = 0;
from.b = 0; to.b = 1;
from.c = 0; to.c = 1;

# Determine the PDF for each coefficient
pdf.a <- density(a,  from = from.a, n = n)
pdf.b <- density(b,  from = from.b, to = to.b, n = n)
pdf.c <- density(c,  from = from.c, to = to.c, n = n)

# Create dataframe of pdf x-values
pdf <- data.frame(pdf.a$x, pdf.b$x, pdf.c$x)

# Determine the CDF for each coefficient
cdf.a <- cumsum(pdf.a$y * diff(pdf.a$x[1:2]))
cdf.b <- cumsum(pdf.b$y * diff(pdf.b$x[1:2]))
cdf.c <- cumsum(pdf.c$y * diff(pdf.c$x[1:2]))

# Create matrix for CDF values
cdf <- matrix(0, nrow = n, ncol = 3)

# Assign cdf results to each column
cdf[, 1] <- cdf.a
cdf[, 2] <- cdf.b
cdf[, 3] <- cdf.c

# Normalize in case of rounding error
for (i in 1:length(cdf[1,])) {
  cdf[,i] <- cdf[,i] / max(cdf[,i])
}

# Redefine as dataframe and name columns for each field
cdf <- data.frame(cdf)
names(cdf) <- c("a", "b", "c")

#-------------------------------------------------------------------------------
# Run MC Simulation
#-------------------------------------------------------------------------------
# Number of simulation runs for each field
nrun <- 10^3
# Define list of Monte-Carlo simulation results for each field
mc <- matrix(0, nrow = length(t), ncol = nrun)

for (i in 1:1) {
  # Define timestep sequence
  t <- seq(from = 1, to = 100, by = 1)
  
  # Define temporary matrix for holding simulation run results for field i
  temp <- matrix(0, nrow = length(t), ncol = nrun)
  
  for (h in 1:nrun) {
    # Generate random numbers for each coefficient for this well
    rn <- runif(3, min = 0, max = 1)
    
    # Use findInterval to pick index and assign value to each variable
    a.rn <- pdf[findInterval(rn[1], c(0, cdf[,(i-1)*3+1])), (i-1)*3+1]
    b.rn <- pdf[findInterval(rn[2], c(0, cdf[,(i-1)*3+2])), (i-1)*3+2]
    c.rn <- pdf[findInterval(rn[3], c(0, cdf[,(i-1)*3+3])), (i-1)*3+3]
    
    # Determine oil production over time from this well
    oil <- a.rn*(1+b.rn*c.rn*t)^(-1/b.rn)
    
    # Rewrite values < 0 and "NaN"
    for (j in 1:length(oil)) {
      if (oil[j] < 0 | oil[j] == "NaN") {
        oil[j] <- 0
      }
      temp[j,h] <- oil[j]
    }
  }
  
  # Save all results to MC matrix
  mc <- temp
}

#-------------------------------------------------------------------------------
# Determine oil production history of sample set and it's percentiles
#-------------------------------------------------------------------------------
# Create dataframe with a, b, and c vectors
coef <- data.frame(a, b, c)
actual <- matrix(0, nrow = length(t), ncol = length(coef[,1]))

for (i in 1:length(coef[,1])) {
  a.act <- coef[i,1]
  b.act <- coef[i,2]
  c.act <- coef[i,3]
  actual[,i] <- a.act*(1+b.act*c.act*t)^(-1/b.act)  
}

#-------------------------------------------------------------------------------
# Pull stats for MC and actual curves
#-------------------------------------------------------------------------------

# Define list to hold quantile stats for each field
quant <- matrix(0, nrow = length(t), ncol = 6)

# Pull stats on MC runs
for (i in 1:length(t)) {
  temp.mc  <- quantile(mc[i,],     c(0.05, 0.50, 0.95))
  temp.act <- quantile(actual[i,], c(0.05, 0.50, 0.95))
  quant[i,1] <- temp.mc[1]
  quant[i,2] <- temp.mc[2]
  quant[i,3] <- temp.mc[3]
  quant[i,4] <- temp.act[1]
  quant[i,5] <- temp.act[2]
  quant[i,6] <- temp.act[3]
}

#-------------------------------------------------------------------------------
# Plot Results
#-------------------------------------------------------------------------------
# Save results to PDF
pdf(file.path(plot_root, "declineTest_v1 Results.pdf"))

# Plot of quantiles actual dataset vs. MC simulation
plot(t, quant[,6],
     type = "l",
     lty = 1,
     col = "blue",
     xlab = "Time (steps)",
     ylab = "Production (units per time)",
     main = "Actual vs. Monte-Carlo Decline Curve (n = 10^3)")
lines(quant[,5], col = "black")
lines(quant[,4], col = "red")
lines(quant[,3], lty = 2, col = "blue")
lines(quant[,2], lty = 2, col = "black")
lines(quant[,1], lty = 2, col = "red")
legend("topright",
       c("Actual 95%", "Actual 50%", "Actual 5%", "MC 95%", "MC 50%", "MC 5%"),
       lty = c(1,1,1,2,2,2),
       col = c("blue", "black", "red", "blue", "black", "red"))

dev.off()