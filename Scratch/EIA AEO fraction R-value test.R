# Predefine shape matrix
gshape <- matrix(0, nrow = 5, ncol = 2)
oshape <- gshape

# Get fitted coefficients
for (i in 1:5) {
  
  # For each year
  gshape[i,] <- coef(fitdist(gas$r[gas$year == i], "beta"))
  oshape[i,] <- coef(fitdist(oil$r[oil$year == i], "beta"))
}

linecolor <- rainbow(5)

# Plot
plot(qbeta(opt$xq, gshape[1,1], gshape[1,2]), opt$xq,
     type = "l",
     col = linecolor[1],
     xlim = c(0,1),
     xlab = "R",
     ylab = "Cumulative Probability",
     main = "Gas Beta Distributions")
for(i in 2:5) {lines(qbeta(opt$xq, gshape[i,1], gshape[i,2]), opt$xq, col = linecolor[i])}

plot(qbeta(opt$xq, oshape[1,1], oshape[1,2]), opt$xq,
     type = "l",
     col = linecolor[1],
     xlim = c(0,1),
     xlab = "R",
     ylab = "Cumulative Probability",
     main = "Oil Beta Distributions")
for(i in 2:5) {lines(qbeta(opt$xq, oshape[i,1], oshape[i,2]), opt$xq, col = linecolor[i])}