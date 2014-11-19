# === Load required files ===
# PDF x-values & CDF y-values for MC simulation of DC coefficients
load(file.path(path$data, "cdf_oow.rda"))
load(file.path(path$data, "pdf_oow.rda"))

# === Prep CDF data ===
# Extract and reorder desired decline curve coefficients for selected fields
cdf.oow <- cdf.oow[,c(34, 35, 36, 1, 2, 3, 4, 5, 6, 16, 17, 18, 22, 23, 24,
                      28, 29, 30, 25, 26, 27, 7, 8, 9, 19, 20, 21, 13, 14,
                      15)]
pdf.oow <- pdf.oow[,c(34, 35, 36, 1, 2, 3, 4, 5, 6, 16, 17, 18, 22, 23, 24,
                      28, 29, 30, 25, 26, 27, 7, 8, 9, 19, 20, 21, 13, 14,
                      15)]

# CDF Function for new data
cdf <- function(stuff, from, to, np) {
  PDF <- density(stuff, from = from, to = to, n = np)
  CDF <- cumsum(PDF$y*diff(PDF$x[1:2]))
  CDF <- CDF/max(CDF)
  return(data.frame(PDF$x, CDF))
}

# Plots for CDFs
pdf(file.path(path$plot, "CDF comparison - FDC vs hyp.pdf"))
for (i in 1:(length(field)-1)) {
  # For qo
  plot(pdf.oow[,(1+3*(i-1))], cdf.oow[,(1+3*(i-1))],
       type = "l",
       xlim = c(0, 10^5),
       main = paste("CDF for 'qo' for Oil Production in Field", field[i]),
       xlab = "'qo' (Initial Production Rate - bbl/month)",
       ylab = "Cumulative Probability")
  # First
  temp <- cdf(stuff = m$qo.1[which(m$fit.1 == 1 & m$w_field_num == field[i])], from = 0, to = 10^5)
  lines(temp[,1], temp[,2], col = "red")
  # Last
  temp <- cdf(stuff = m$qo.2[which(m$fit.2 == 1 & m$w_field_num == field[i])], from = 0, to = 10^5)  
  lines(temp[,1], temp[,2], col = "green")
  legend("bottomright",
         c("Old", "First", "Last"),
         lty = c(1, 1, 1),
         col = c("black", "red", "green"))
  
  # For b
  plot(pdf.oow[,(2+3*(i-1))], cdf.oow[,(2+3*(i-1))],
       type = "l",
       xlim = c(-10, 10),
       main = paste("CDF for 'b' for Oil Production in Field", field[i]),
       xlab = "'b' (Decline Exponent)",
       ylab = "Cumulative Probability")
  # First
  temp <- cdf(stuff = m$b.1[which(m$fit.1 == 1 & m$w_field_num == field[i])], from = -10, to = 10)
  lines(temp[,1], temp[,2], col = "red")
  # Last
  temp <- cdf(stuff = m$b.2[which(m$fit.2 == 1 & m$w_field_num == field[i])], from = -10, to = 10)  
  lines(temp[,1], temp[,2], col = "green")
  legend("bottomright",
         c("Old", "First", "Last"),
         lty = c(1, 1, 1),
         col = c("black", "red", "green"))
  
  # For Di
  plot(pdf.oow[,(3+3*(i-1))], cdf.oow[,(3+3*(i-1))],
       type = "l",
       xlim = c(0.01, 50e3),
       log = "x",
       main = paste("CDF for 'Di' for Oil Production in Field", field[i]),
       xlab = "'Di' (Initial Decline Rate - log scale)",
       ylab = "Cumulative Probability")
  # First
  temp <- cdf(stuff = m$Di.1[which(m$fit.1 == 1 & m$w_field_num == field[i])], from = 0.01, to = 50e3)
  lines(temp[,1], temp[,2], col = "red")
  # Last
  temp <- cdf(stuff = m$Di.2[which(m$fit.2 == 1 & m$w_field_num == field[i])], from = 0.01, to = 50e3)
  lines(temp[,1], temp[,2], col = "green")
  legend("bottomright",
         c("Old", "First", "Last"),
         lty = c(1, 1, 1),
         col = c("black", "red", "green"))
}
dev.off()