# Function ----------------------------------------------------------------
CDF <- function(vector, from, to, np) {
  PDF <- density(vector, from = from, to = to, n = np)
  CDF <- cumsum(PDF$y*diff(PDF$x[1:2]))
  CDF <- CDF/max(CDF)
  return(data.frame(PDF$x, CDF))
}

# Random normal sample
test <- rnorm(1000, 50, 10)

# Get CDF
ctest <- CDF(test, 0, 100, 10e3)
rtest <- ctest$PDF.x[findInterval(runif(10e3), c(0, ctest$CDF))]

# Plot result
plot(density(test), col = "green")
lines(density(rtest), col = "blue")

# Get coefficient values at every 0.1% quantile
xq <- seq(0,1,0.001)
qtest <- quantile(test, probs = xq, names = FALSE)
qdf <- data.frame(qtest, xq)
names(qdf) <- c("x", "CDF")
qtv <- qdf$x[findInterval(runif(10e3), c(0, qdf$CDF))]

# Put plots side-by-side
par(mfrow = c(1,2))

# Plot comparison of CDF from density() vs. quantile()
plot(ctest, type = "l", col = "blue")
lines(qdf, col = "red")

# Add replot density results with additional line for quantile() results
# Plot result
plot(density(test), col = "green")
lines(density(rtest), col = "blue")
lines(density(qtv), col = "red")

# Load data in the "DCA_fits_v2.rda" file, change "path$data" to whatever
# filepath leads to the file on your machine.
load(file.path(path$data, "DCA_fits_v2.rda"))

# Only wells whose first decline curve was successfully fitted
mo <- mo[which(mo$fit.1 == 1),]
mg <- mg[which(mg$fit.1 == 1),]

# Take the subset of fitted qo values for oil production from Field 630 and with
# qo <= 30e6 bbl (the upper limit for qo I'm currently using)
test <- mo$qo.1[which(mo$w_field_num == 630)]
subt <- test[which(test <= 30e6)]

# Get MC coefficients using density() and CDF() functions
ctest <- CDF(vector = subt, from = opt$cdf.oil.from[1], to = opt$cdf.oil.to[1], np = opt$cdf.oil.np[1])
rtest <- ctest$PDF.x[findInterval(runif(10e3), c(0, ctest$CDF))]

# Get MC coefficients using quantile() function
xq <- seq(0,1,0.0001)
qtest <- quantile(subt, probs = xq, names = FALSE)
qdf <- data.frame(qtest, xq)
names(qdf) <- c("x", "CDF")
qtv <- qdf$x[findInterval(runif(10e3), c(0, qdf$CDF))]

# Put results into data.frame for boxplotting
rdf <- data.frame(c(rep("fit", times = length(subt)),
                    rep("density()", times = length(rtest)),
                    rep("quantile()", times = length(qtv))),
                  c(subt, rtest, qtv))
names(rdf) <- c("type", "qo")

# Make boxplot and CDF plot
# Plot comparison of CDF from density() vs. quantile()
plot(ctest, type = "l", col = "blue")
lines(qdf, col = "red")

# Boxplot
boxplot(qo ~ type,
        rdf,
        log = "y",
        range = 0,
        ylim = c(1, 1.1*max(rdf$qo)),
        xlab = "Source",
        ylab = "qo value for oil (bbl)",
        main = "Comparison of Oil qo values - fit vs. MC from density/quantile")