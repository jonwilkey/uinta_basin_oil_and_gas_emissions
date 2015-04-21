# Define start and stop time points
tstart <- as.Date(c("1985-01-01", "1990-01-01", "1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01"))
tstop <-  as.Date(c("1990-01-01", "1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01"))

# Get count of number of wells drilled over all time
wellcount <- sqldf("select distinct w_field_num, count(distinct(p_api))
                   from p
                   where h_well_type = 'OW' or h_well_type = 'GW'
                   group by w_field_num")

# Get list of wells with greater than "x" number of wells drilled
fl <- wellcount[wellcount[,2]>=200,1]

# Cumulative production CDF function
testf <- function(tstart, tstop, fl) {
  
  # Predefine results
  coil <- matrix(0, nrow = length(opt$xq), ncol = length(fl))
  cgas <- coil
  
  # Get subset of production data
  ps <- subset(p,
               subset = (time != 0 &
                           (p$h_well_type == "OW" |
                              p$h_well_type == "GW") &
                           p_days_prod >= opt$minDayProd &
                           p_rpt_period >= tstart &
                           p_rpt_period < tstop &
                           h_first_prod >= tstart &
                           h_first_prod < tstop),
               select = c("p_api",
                          "p_oil_prod",
                          "p_gas_prod",
                          "w_field_num"))
  
  # Get list of unique wells and calculate cumulative oil and gas production
  total <- sqldf("select distinct p_api, w_field_num, sum(p_oil_prod), sum(p_gas_prod)
                from ps
                group by p_api")
  
  names(total) <- c("api", "field", "coil", "cgas")
  
  # For each field...
  for (i in 1:ncol(coil)) {
    
    # Get index of wells located in field i
    ind <- which(total$field == fl[i])
    
    # Get cumulative oil/gas CDF for field i
    coil[,i] <- CDFq(total$coil[ind], xq = opt$xq)[,1]
    cgas[,i] <- CDFq(total$cgas[ind], xq = opt$xq)[,1]
    
    # Drop rows for field i
    if (length(ind) > 0) {
      total <- total[-ind,]
    }
  }
  
  # Any remaining wells are Field 999
  coil <- cbind(coil, CDFq(total$coil, xq = opt$xq)[,1])
  cgas <- cbind(cgas, CDFq(total$cgas, xq = opt$xq)[,1])
  
  # Return result
  return(list(coil = coil, cgas = cgas))
}

# Predefine coil/cgas arrays with dimensions (CDF probability, field, time interval)
coil <- array(rep(0, times = (length(fl)+1)*length(opt$xq)*length(tstart)),
              dim = c(length(opt$xq), (length(fl)+1), length(tstart)))
cgas <- coil

# Perform CDF calculation
for (i in 1:length(tstart)) {
  
  temp <- testf(tstart[i], tstop[i], fl)
  coil[,,i] <- temp$coil
  cgas[,,i] <- temp$cgas
}

# Get well counts as f(time) for each field
wc <- sqldf("select distinct(p_api), w_field_num, h_first_prod
            from p
            where h_well_type = 'OW' or h_well_type = 'GW'")

# Drop NAs
wc <- na.omit(wc)

# Change names
names(wc) <- c("api", "field", "drill")

# Convert drill from date to year
wc$drill <- year(wc$drill)

# Only from tstart onwards
wc <- wc[wc$drill > year(tstart[1]),]

yrs <- 1984:2014
wcr <- matrix(0, nrow = length(yrs), ncol = (length(fl)+1))

# For each field
for (i in 1:length(fl)) {
  
  ind <- which(wc$field == fl[i])
  temp <- wc[ind,]
  
  # For each year
  for (j in 1:length(yrs)) {
    wcr[j,i] <- length(which(temp$drill == yrs[j]))
  }
  
  # Drop ind rows
  wc <- wc[-ind,]
}

# For 999
for (j in 1:length(yrs)) {
  wcr[j,i+1] <- length(which(wc$drill == yrs[j]))
}

# Add in Field 999
fl <- c(fl, 999)

pdf(file = file.path(path$plot, "Cprod and Wcount by field as f_time.pdf"),
    width = 30,
    height = 10)

opar <- par(no.readonly = T)
par(mfcol = c(1,3))
linecolor <- rainbow(length(tstart))

# Plot everything
for (i in 1:length(fl)) {
  
  # Well Counts Plots
  barplot(wcr[,i],
          names.arg = yrs,
          xlab = "Year",
          ylab = "# of Well Drilled",
          main = paste("Number of Wells Drilled in Field", fl[i], "by Year"))
  
  # Oil CDF
  plot(coil[,i,1], opt$xq,
       type = "l",
       col = linecolor[1],
       xlim = c(0, 150e3),
       xlab = "Cumulative Oil Production (bbl)",
       ylab = "Cumulative Probability",
       main = paste("CDF for Cumulative Oil Produced in Field", fl[i]))
  
  # Other oil CDF lines
  for (j in 2:length(tstart)) {lines(coil[,i,j], opt$xq, col = linecolor[j])}
  
  legend("bottomright", c("85-90", "90-95", "95-00", "00-05", "05-10", "10-15"),
         ncol = 2, col = linecolor, lty = 1)
  
  # Gas CDF
  plot(cgas[,i,1], opt$xq,
       type = "l",
       col = linecolor[1],
       xlim = c(0, 1e6),
       xlab = "Cumulative Gas Production (MCF)",
       ylab = "Cumulative Probability",
       main = paste("CDF for Cumulative Gas Produced in Field", fl[i]))
  
  # Other oil CDF lines
  for (j in 2:length(tstart)) {lines(cgas[,i,j], opt$xq, col = linecolor[j])}
  
  legend("bottomright", c("85-90", "90-95", "95-00", "00-05", "05-10", "10-15"),
         ncol = 2, col = linecolor, lty = 1)
}

par(opar)

dev.off()


