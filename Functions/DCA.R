# Function Info -----------------------------------------------------------
# Name:      DCA.R (Decline Curve Analysis)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# 


# Outputs -----------------------------------------------------------------

# 


# Description -------------------------------------------------------------

# blah  


# Function ----------------------------------------------------------------
DCA <- function(path, p, tsteps, field, min.depth, max.depth,
                           well.depth.step, version) {}


# Options Input - Replace with opt$___ after finished coding funct --------

minProdRec        <- 12   # Minimum number of non-zero production records
oil.measure.error <- 1    # Assuming +/- 1 bbl accuracy in production records for oil
diff.bin.cutoff   <- 0.25 # Minimum production differential on normalized scale required to consider a well as being restarted
bin               <- 12   # Bin size
DCAplot           <- TRUE # True/False flag indicating whether or not to print
b.start.oil       <- 1.78
Di.start.oil      <- 1.16
lower.oil         <- c(0, 0, 0)
upper.oil         <- c(10^5, 10, Inf)
field             <- opt$field
ver               <- opt$file_ver


# Subset data -------------------------------------------------------------

ps <- subset(p,
             subset = (time != 0 &
                       (p$h_well_type == "OW" |
                        p$h_well_type == "GW") &
                       p$p_days_prod >= 28),
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

# Get list of unique wells and order by cumulative oil and gas production
well <- sqldf("select distinct p_api, w_field_num, w_totcum_oil, w_totcum_gas, nrec
              from ps
              order by w_totcum_oil DESC, w_totcum_gas DESC")

# Add two columns for cumulative production fraction (CPF) of oil and gas
well$CPFo <- well$w_totcum_oil/sum(well$w_totcum_oil)
well$CPFg <- well$w_totcum_gas/sum(well$w_totcum_gas)



# DCA Fitting - Oil -------------------------------------------------------

# Predefine index of wells to skip for generating list of wells located in Field
# 999
ind999 <- NULL

# Predefine results data.frame for oil and gas
ro <- data.frame(api= as.character(rep(0, times = nrow(well)*2)),
                 qo = rep(0, times = nrow(well)*2),
                 b = rep(0, times = nrow(well)*2),
                 Di = rep(0, times = nrow(well)*2),
                 red.chi = rep(0, times = nrow(well)*2),
                 tdelay = rep(0, times = nrow(well)*2),
                 fitFirst = rep(0, times = nrow(well)*2),
                 fitLast = rep(0, times = nrow(well)*2),
                 skipped = rep(0, times = nrow(well)*2),
                 failed = rep(0, times = nrow(well)*2))

rg <- ro

# Set initial value for row counters "row1" and "row2"
row1 <- 1; row2 <- 2

# For each field to be analyzed individually
for (g in 1:(length(field)-1)) {
  
  # If printing, initialize PDF devices
  if (DCAplot == TRUE) {
    pdf(file.path(path$plot, paste("Field ", field[g], " oil DCA ", ver, ".pdf", sep = "")))
  }
  
  # Get row indices of wells located just in field[g]
  apilist <- which(well$w_field_num == field[g])
  
  # For each well locatd in field[g], fit oil production
  for (h in 1:length(apilist)) {
    # Get subset of production records for this individual well
    w <- subset(ps,
                subset = (p_api == well$p_api[apilist[h]]),
                select = c("time", "p_oil_prod"))
    
    # Add subsets of oil and gas production records which are non-zero
    w <- w[-which(w$p_oil_prod <= 0),c("time","p_oil_prod")]
    
    # Check - is number of rows in w >= minProdRec requirement?
    if (nrow(w) >= minProdRec) {
      
      # Rename to columns to fit hypfit function requirements
      names(w) <- c("time", "prod")
      
      # Run hypfit function
      ro[row1:row2,] <- hypfit(ws = w,
                               bin = bin,
                               diff.bin.cutoff = diff.bin.cutoff,
                               minProdRec = minProdRec,
                               api = well$p_api[apilist[h]],
                               b.start = b.start.oil,
                               Di.start = Di.start.oil,
                               lower = lower.oil,
                               upper = upper.oil,
                               plotFlag = DCAplot,
                               type = "Oil")
    } else {
      
      # Skip and note failure
      ro$skipped[row1:row2] <- 1
    }
    
    # Increment row counters (hopefully this is faster than using rbind)
    row1 <- row1+2; row2 <- row2+2
  }
  
  # If printing, close PDF devices
  if (DCAplot == TRUE) {
    dev.off()
  }
  
  # Add indices of wells located in field[g] to the skip list for Field 999
  ind999 <- c(ind999, apilist)
}

# --- For Field 999 ---
# If printing, initialize PDF devices
if (DCAplot == TRUE) {
  pdf(file.path(path$plot, paste("Field ", field[g+1], " oil DCA ", ver, ".pdf", sep = "")))
}

# Row indices are everything which is not in ind999
apilist <- well$p_api[-ind999]

# For each well locatd in Field 999, fit oil production
for (h in 1:length(apilist)) {
  # Get subset of production records for this individual well
  w <- subset(ps,
              subset = (p_api == apilist[h]),
              select = c("time", "p_oil_prod"))
  
  # Add subsets of oil and gas production records which are non-zero
  w <- w[-which(w$p_oil_prod <= 0),c("time","p_oil_prod")]
  
  # Check - is number of rows in w >= minProdRec requirement?
  if (nrow(w) >= minProdRec) {
    
    # Rename to columns to fit hypfit function requirements
    names(w) <- c("time", "prod")
    
    # Run hypfit function
    ro[row1:row2,] <- hypfit(ws = w,
                             bin = bin,
                             diff.bin.cutoff = diff.bin.cutoff,
                             minProdRec = minProdRec,
                             api = apilist[h],
                             b.start = b.start.oil,
                             Di.start = Di.start.oil,
                             lower = lower.oil,
                             upper = upper.oil,
                             plotFlag = DCAplot,
                             type = "Oil")
  } else {
    
    # Skip and note failure
    ro$skipped[row1:row2] <- 1
  }
  
  # Increment row counters (hopefully this is faster than using rbind)
  row1 <- row1+2; row2 <- row2+2
}

# If printing, close PDF devices
if (DCAplot == TRUE) {
  dev.off()
}


# Analysis ----------------------------------------------------------------

# Split ro into first decline curve fits and last decline curve fits
ro.first <- ro[seq(from = 1, to = nrow(ro)-1, by = 2),]
ro.last  <- ro[seq(from = 2, to = nrow(ro), by = 2),]

# Drop repeat columns
ro.first <- ro.first[,-8]
ro.last  <- ro.last[,c(-6, -7)]

# Drop 0 API columns
ro.first <- ro.first[which(ro.first$api != 0),]
ro.last  <- ro.last[which(ro.last$api != 0),]

# Change names
names(ro.first) <- c("api", "qo.1", "b.1", "Di.1", "red.chi.1", "tdelay.1", "fit.1", "skip.1", "fail.1")
names(ro.last)  <- c("api", "qo.2", "b.2", "Di.2", "red.chi.2", "fit.2", "skip.2", "fail.2")

# Merge with well data.frame
m <- merge(x = well, y = ro.first, by.x = "p_api", by.y = "api", all.x = TRUE)
m <- merge(x = m, y = ro.last, by.x = "p_api", by.y = "api", all.x = TRUE)

# Add completion dates for one-off analysis
temp <- sqldf("select distinct p_api, h_first_prod from p")
temp <- na.omit(temp)
temp$prod_date <- as.numeric(floor(as.yearmon(temp[,"h_first_prod"])))
m <- merge(x = m, y = temp, by.x = "p_api", by.y = "p_api", all.x = TRUE)
names(m)[14] <- "skip_1"; names(m)[21] <- "skip_2"

pdf(file.path(path$plot, "Fit vs Skipped Diagnostic Plots.pdf"))

temp <- m[which(is.na(m$skip_1) | (m$skip_1 == 1 & m$skip_2 == 1)),]
temp <- sqldf("select prod_date, count(p_api)
              from temp
              group by prod_date")
temp <- na.omit(temp)
test <- data.frame(1948:2013)
names(test) <- "prod_date"
test <- merge(x = test, y = temp, all.x = TRUE)
test[which(is.na(test[,2])),2] <- 0
names(test)[2] <- "skip"

temp1 <- sqldf("select prod_date, count(p_api)
              from m
              where skip_1 = 0 or skip_2 = 0
               group by prod_date")
temp1 <- na.omit(temp1)
test <- merge(x = test, y = temp1, all.x = TRUE)
names(test)[3] <- "fit"
test[which(is.na(test[,3])),3] <- 0
barplot(test[,2], names.arg = test[,1], xlab = "Date of First Production (Year)", ylab = "Number of Wells Skipped", main = "Comparison of Skip-Rate vs. Well Age")
barplot(test[,3], names.arg = test[,1], xlab = "Date of First Production (Year)", ylab = "Number of Wells Fit", main = "Comparison of Fit-Rate vs. Well Age")
plot(test[,1], cumsum(test[,2])/max(cumsum(test[,2])),
     type = "l",
     col = "red",
     xlab = "Date of First Production (Year)",
     ylab = "Cumulative Well Fraction",
     main = "Comparison of Distribution of Wells Skipped/Fit vs. Well Age")
lines(test[,1], cumsum(test[,3])/max(cumsum(test[,3])), col = "blue")
legend("topleft",
       c("Skip", "Fit"),
       lty = c(1, 1),
       col = c("red", "blue"))

temp <- m[which(is.na(m$skip_1) | (m$skip_1 == 1 & m$skip_2 == 1)),"w_totcum_oil"]
temp <- temp[which(temp <= 3e5)]
hist(temp, breaks = 25, main = "Histogram of Skipped Wells Cumulative Oil Production", xlab = "Cumulative Oil Production (bbl)")

temp1 <- m[which(m$skip_1 == 0 | m$skip_2 == 0),"w_totcum_oil"]
temp1 <- temp1[which(temp1 <= 3e5)]
hist(temp1, breaks = 25, main = "Histogram of Fitted Wells Cumulative Oil Production", xlab = "Cumulative Oil Production (bbl)")

temp <- cdf(stuff = temp, from = 1, to = 1e6, np = 2^10)
plot(temp,
     type = "l",
     log = "x",
     xlim = c(10,1e6),
     col = "red",
     xlab = "Cumulative Oil Production (bbl) - Log Scale",
     ylab = "Cumulative Probability",
     main = "Comparison of Cumulative Oil Production")
temp1 <- cdf(stuff = temp1, from = 1, to = 1e6, np = 2^10)
lines(temp1, col = "blue")
legend("topleft",
       c("Skip", "Fit"),
       lty = c(1, 1),
       col = c("red", "blue"))

temp <- m[which(is.na(m$skip_1) | (m$skip_1 == 1 & m$skip_2 == 1)),]
temp <- temp$w_totcum_oil/temp$nrec
temp <- temp[which(temp <= 1e4)]
hist(temp, main = "Histogram of Skipped Wells Avg. Cumulative Oil Production", xlab = "Record-Averagd Cumulative Oil Production (bbl/record)")

temp1 <- m[which(m$skip_1 == 0 | m$skip_2 == 0),]
temp1 <- temp1$w_totcum_oil/temp1$nrec
temp1 <- temp1[which(temp1 <= 1e4)]
hist(temp1, main = "Histogram of Fitted Wells Avg. Cumulative Oil Production", xlab = "Record-Averagd Cumulative Oil Production (bbl/record)")

temp <- cdf(stuff = temp, from = 1, to = 1e4, np = 2^10)
plot(temp,
     type = "l",
     log = "x",
     xlim = c(1,1e4),
     col = "red",
     xlab = "Record-Averagd Cumulative Oil Production (bbl/record) - Log Scale",
     ylab = "Cumulative Probability",
     main = "Comparison of Record-Averaged Cumulative Oil Production")
temp1 <- cdf(stuff = temp1, from = 1, to = 1e4, np = 2^10)
lines(temp1, col = "blue")
legend("topleft",
       c("Skip", "Fit"),
       lty = c(1, 1),
       col = c("red", "blue"))

dev.off()