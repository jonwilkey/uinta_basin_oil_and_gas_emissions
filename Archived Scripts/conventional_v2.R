#-------------------------------------------------------------------------------
# conventional_v2.R (Conventional Oil and Gas Model)
# Version 2
# 01/16/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Version History
#-------------------------------------------------------------------------------
# --- Version 1 ---
# Loads *.rda files, finds APDs approved in each field at each timestep
# according to Model 8, determines monthly production by field, plots predicted
# vs. actual APD approvals, plots predicted production.
# --- Version 1 ---
# Added sqldf query to pull actual production history of gas and oil in Basin

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Change this to the folder where you keep your *.rda files
# For Mac:
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data"
# For Windows:
data_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(data_root)

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------
# Copy/Paste function
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------
library(sqldf)

#-------------------------------------------------------------------------------
# Load required data files
#-------------------------------------------------------------------------------
# Load *.rda files
load(file.path(data_root, "compare_ow.rda"))
load(file.path(data_root, "compare_gw.rda"))
load(file.path(data_root, "decline_field_ow.rda"))
load(file.path(data_root, "decline_field_gw.rda"))
load(file.path(data_root, "m_fit_ow.rda"))
load(file.path(data_root, "m_fit_gw.rda"))
load(file.path(data_root, "production.rda"))
# Rename some dataframes for brevity
dco <- decline.field.ow
dcg <- decline.field.gw
p <- production

#-------------------------------------------------------------------------------
# Determine APDs approved as f(time) and select field they will be drilled in
#-------------------------------------------------------------------------------
# Oil Wells
# Pull coefficients out of linear model
coef.ow <- coef(m.fit.ow)
# Define field selection percentages
pw.ow <- decline.field.ow[,2]/sum(decline.field.ow[,2])
cum.pw.ow <- cumsum(pw.ow)
norm.cum.pw.ow <- cum.pw.ow/max(cum.pw.ow)
# Define size of pred.ow
pred.ow <- matrix(0, nrow = length(compare.ow[,1]), ncol = length(dco[,1]))
Wo <- 0
for (j in 1:length(compare.ow$apd_date)) {
  pred <- coef.ow[1] +
          coef.ow[2] * compare.ow$oil_price[j] +
          coef.ow[3] * compare.ow$gas_price[j] +
          coef.ow[4] * Wo
  Wo <- pred
  pred <- round(pred)
  if (pred > 0) {
    for (k in 1:pred) {
      rn <- runif(1, min = 0, max = 1)
      m <- findInterval(rn,c(0,norm.cum.pw.ow))
      pred.ow[j,m] <- pred.ow[j,m] + 1
    }
  }
}

# Gas Wells
# Pull coefficients out of linear model
coef.gw <- coef(m.fit.gw)
# Define field selection percentages
pw.gw <- decline.field.gw[,2]/sum(decline.field.gw[,2])
cum.pw.gw <- cumsum(pw.gw)
norm.cum.pw.gw <- cum.pw.gw/max(cum.pw.gw)
# Define size of pred.gw
pred.gw <- matrix(0, nrow = length(compare.gw[,1]), ncol = length(dco[,1]))
Wo <- 0
for (j in 1:length(compare.gw$apd_date)) {
  pred <- coef.gw[1] +
    coef.gw[2] * compare.gw$oil_price[j] +
    coef.gw[3] * compare.gw$gas_price[j] +
    coef.gw[4] * Wo
  Wo <- pred
  pred <- round(pred)
  if (pred > 0) {
    for (k in 1:pred) {
      rn <- runif(1, min = 0, max = 1)
      m <- findInterval(rn,c(0,norm.cum.pw.gw))
      pred.gw[j,m] <- pred.gw[j,m] + 1
    }
  }
}

#-------------------------------------------------------------------------------
# Determine oil and gas production as f(time) for each field and entire Basin
#-------------------------------------------------------------------------------
# Set time delay between APD approval and production (median time delay for all
# steps as outlined in "20NOV13 Meeting Notes.pdf"). tdelay units are months,
# assuming 30 days per month, rounded to nearest month (can't do partial time
# steps)
tdelay.ow <- round((228+14+31+299+2)/30)
tdelay.gw <- round((249+23+63+352+5)/30)

# Oil production
t <- 1:length(compare.ow[,1])
p.ow <- matrix(0, nrow = length(t), ncol = length(dco[,1]))
for (k in 1:length(dco[,1])) {
  alpha <- dco[k,4]
  theta <- dco[k,5]
  delta <- dco[k,6]
  prod <- alpha * (1 + theta * delta * t)^(-1/theta)
  table <- matrix(0, nrow = tdelay.ow + 2 * length(t) - 1, ncol = length(t))
  for (j in 1:length(t)) {
    for (i in 1:length(prod)) {
      table[(i+tdelay.ow+(j-1)),j] <- prod[i] * pred.ow[j,k]
    }
  }
  table <- table[1:length(t),]
  p.ow[,k] <- rowSums(table)
}

# Gas production
t <- 1:length(compare.gw[,1])
p.gw <- matrix(0, nrow = length(t), ncol = length(dcg[,1]))
for (k in 1:length(dcg[,1])) {
  alpha <- dcg[k,7]
  theta <- dcg[k,8]
  delta <- dcg[k,9]
  prod <- alpha * (1 + theta * delta * t)^(-1/theta)
  table <- matrix(0, nrow = tdelay.gw + 2 * length(t) - 1, ncol = length(t))
  for (j in 1:length(t)) {
    for (i in 1:length(prod)) {
      table[(i+tdelay.gw+(j-1)),j] <- prod[i] * pred.gw[j,k]
    }
  }
  table <- table[1:length(t),]
  p.gw[,k] <- rowSums(table)
}

# Determine actual oil and gas production
p.actual.o <- sqldf("select proddata_rpt_period, sum(proddata_oil_prod)
                    from p
                    where histdata_apd_aprovd > '1999-01-01'
                    group by proddata_rpt_period")
p.actual.g <- sqldf("select proddata_rpt_period, sum(proddata_gas_prod)
                    from p
                    where histdata_apd_aprovd > '1999-01-01'
                    group by proddata_rpt_period")
# Change proddata_rpt_period (column 1) variable type from character to date
p.actual.o[,1] <- as.Date(p.actual.o[,1])
p.actual.g[,1] <- as.Date(p.actual.g[,1])


#-------------------------------------------------------------------------------
# Output - Plots and Tables
#-------------------------------------------------------------------------------
# Save plots to PDF
pdf(file = paste("conventional_v2 Results.pdf"))
# Plot of Oil APDs Approved (Predicted vs. Actual)
plot(compare.ow$apd_date, rowSums(pred.ow),
     type = "l",
     lty = 1,
     col = "blue",
     xlab = "Date",
     ylab = "Number of Oil Well APDs Approved",
     main = "Comparison of Actual vs. Predicted Oil APD Approvals")
lines(compare.ow$apd_date, compare.ow$APDs_approved, col = "black")
legend("topleft",
       c("Predicted", "Actual"),
       lty = c(1,1),
       col = c("blue", "black"))
# Plot of Predicted vs Actual Oil Production
plot(compare.ow$apd_date, rowSums(p.ow),
     type = "l",
     lty = 1,
     col = "blue",
     xlab = "Date",
     ylab = "Oil Production (bbl per month)",
     main = "Predicted Oil Production for Basin")
lines(p.actual.o[,1], p.actual.o[,2], col = "black")
legend("topleft",
       c("Predicted", "Actual"),
       lty = c(1,1),
       col = c("blue", "black"))

# Plot of Gas APDs Approved (Predicted vs. Actual)
plot(compare.gw$apd_date, compare.gw$APDs_approved,
     type = "l",
     lty = 1,
     col = "black",
     xlab = "Date",
     ylab = "Number of Gas Well APDs Approved",
     main = "Comparison of Actual vs. Predicted Gas APD Approvals")
lines(compare.gw$apd_date, rowSums(pred.gw), col = "blue")
legend("topleft",
       c("Actual", "Predicted"),
       lty = c(1,1),
       col = c("black", "blue"))
# Plot of Predicted vs. Actual Gas Production
plot(compare.gw$apd_date, rowSums(p.gw),
     type = "l",
     lty = 1,
     col = "blue",
     xlab = "Date",
     ylab = "Gas Production (MCF per month)",
     main = "Predicted Gas Production for Basin")
lines(p.actual.g[,1], p.actual.g[,2], col = "black")
legend("topleft",
       c("Predicted", "Actual"),
       lty = c(1,1),
       col = c("blue", "black"))
dev.off()