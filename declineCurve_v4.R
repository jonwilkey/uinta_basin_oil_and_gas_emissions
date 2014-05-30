#-------------------------------------------------------------------------------
# declineCurve_v4.R (DOGM Decline Curve Fitting)
# Version 4
# 02/11/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Version History
#-------------------------------------------------------------------------------
# --- Version 1 ---
# 1. Coded for fits for oil and gas for basin, eight most active oil and gas
#    fields, and individual wells in each of those fields.
# --- Version 2 ---
# 1. Updated to have field level fit all fields in production.rda, dropped
#    individual well fits.
# --- Version 3 ---
# 1. Commented out PDF outputs of v2.
# 2. Added export segment to save hyperbolic fit coefficients for field level as
#    *.rda files for use in Conventional_v1.R.
# --- Version 4 ---
# 1. Rewrote script to fit individual oil well decline curves and determine 
#    probability and cumulative distribution functions for each coefficient in
#    hyperbolic decline curve equation for all oil fields at the 95% cutoff.

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(work_root)

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
# 'minpack.lm' for nonlinear least-squares regression using Levenberg-Marquardt
# algorithm
library(minpack.lm)
# 'sqldf' for SQL queries
library(sqldf)

#-------------------------------------------------------------------------------
# Load DOGM data tables
#-------------------------------------------------------------------------------
# Load the dataframe named 'production'
p <- load(file.path(data_root, "production.rda"))
# Rename dataframe from brevity
p <- production
remove("production")
# Drop first month of production since it can be partial month
p <- subset(p, time != 0)
# Create subset dataframe which contains only API#s and oil prod. vs. time data
p.oow <- subset(p, w_well_type=="OW")
p.oow <- p.oow[, c(1, 44, 9)]
# Same for gas wells
p.ggw <- subset(p, w_well_type == "GW")
p.ggw <- p.ggw[, c(1, 44, 10)]

#-------------------------------------------------------------------------------
# Regression to Individual Oil Wells by Field
#-------------------------------------------------------------------------------

# Create list of unique API #'s and their Field # in p that are oil wells
api.ow <- sqldf("select distinct(p_api), w_field_num from p where w_well_type = 'OW'")

# Get api list for each field (cutoff of 95% of APD count & 95% of production)
api.list.105 <- subset(api.ow, w_field_num == 105, row.names = FALSE); api.list.105 <- api.list.105[,1]
api.list.72  <- subset(api.ow, w_field_num == 72,  row.names = FALSE); api.list.72  <- api.list.72[,1]
api.list.590 <- subset(api.ow, w_field_num == 590, row.names = FALSE); api.list.590 <- api.list.590[,1]
api.list.117 <- subset(api.ow, w_field_num == 117, row.names = FALSE); api.list.117 <- api.list.117[,1]
api.list.718 <- subset(api.ow, w_field_num == 718, row.names = FALSE); api.list.718 <- api.list.718[,1]
api.list.55  <- subset(api.ow, w_field_num == 55,  row.names = FALSE); api.list.55  <- api.list.55[,1]
api.list.60  <- subset(api.ow, w_field_num == 60,  row.names = FALSE); api.list.60  <- api.list.60[,1]
api.list.65  <- subset(api.ow, w_field_num == 65,  row.names = FALSE); api.list.65  <- api.list.65[,1]
api.list.665 <- subset(api.ow, w_field_num == 665, row.names = FALSE); api.list.665 <- api.list.665[,1]
api.list.710 <- subset(api.ow, w_field_num == 710, row.names = FALSE); api.list.710 <- api.list.710[,1]
api.list.80  <- subset(api.ow, w_field_num == 80,  row.names = FALSE); api.list.80  <- api.list.80[,1]
api.list.630 <- subset(api.ow, w_field_num == 630, row.names = FALSE); api.list.630 <- api.list.630[,1]
api.list.605 <- subset(api.ow, w_field_num == 605, row.names = FALSE); api.list.605 <- api.list.605[,1]
api.list.560 <- subset(api.ow, w_field_num == 560, row.names = FALSE); api.list.560 <- api.list.560[,1]
api.list.101 <- subset(api.ow, w_field_num == 101, row.names = FALSE); api.list.101 <- api.list.101[,1]
api.list.610 <- subset(api.ow, w_field_num == 610, row.names = FALSE); api.list.610 <- api.list.610[,1]
api.list.692 <- subset(api.ow, w_field_num == 692, row.names = FALSE); api.list.692 <- api.list.692[,1]
api.list.695 <- subset(api.ow, w_field_num == 695, row.names = FALSE); api.list.695 <- api.list.695[,1]

# Enter indices here of wells that fail to converge to skip them in subsequent
# iterations of for-loop
api.list.105 <- api.list.105[c(-2,    -8,    -11,   -14,   -15,   -17,   -36,
                               -194,  -215,  -323,  -491,  -516,  -534,  -550,
                               -578,  -582,  -586,  -596,  -835,  -848,  -936,
                               -941,  -952,  -957,  -969,  -988,  -1005, -1011,
                               -1153, -1271, -1279)]
api.list.72  <- api.list.72 [c(-7,    -10,   -12,   -15,   -24,   -26,   -37,
                               -184,  -194,  -279,  -286,  -321,  -325,  -355,
                               -364,  -368,  -391,  -399,  -403,  -450,  -567,
                               -604,  -625,  -645,  -660,  -662)]
api.list.590 <- api.list.590[c(-2,    -16,   -22,   -71,   -155)]
# No skips for Field 117
api.list.718 <- api.list.718[c(-43,   -150,  -151)]
api.list.55  <- api.list.55 [c(-12,   -25,   -27,   -35,   -44,   -47,   -49,
                               -50,   -54,   -55,   -64,   -65,   -67,   -73,
                               -77,   -78,   -84,   -85,   -89,   -91,   -96,
                               -102,  -104,  -106,  -108,  -110,  -112,  -116,
                               -124,  -125,  -128,  -139,  -142,  -149,  -165,
                               -170,  -180,  -183,  -191,  -195,  -199,  -202,
                               -207,  -208,  -213,  -222,  -227,  -233,  -258,
                               -311,  -323,  -329)]
api.list.60  <- api.list.60 [c(-4)]
api.list.65  <- api.list.65 [c(-3,    -12,   -18,   -22,   -39,   -40,   -42,
                               -45,   -48,   -52,   -58,   -61,   -63,   -64,
                               -66,   -69,   -70,   -77,   -78,   -95,   -97,
                               -100,  -101,  -120,  -131,  -134,  -139,  -141,
                               -144,  -244,  -245,  -250,  -268,  -279,  -295,
                               -300,  -338)]
api.list.665 <- api.list.665[c(-3,    -10,   -17,   -23,   -24,   -25,   -28,
                               -32,   -33,   -34,   -40,   -41,   -45,   -51,
                               -58,   -59,   -61,   -65,   -69,   -71,   -77,
                               -85,   -89,   -92,   -93,   -97,   -99,   -100,
                               -102,  -104,  -105,  -108,  -114,  -125,  -130,
                               -131,  -132,  -137,  -147,  -171,  -173)]
api.list.710 <- api.list.710[c(-1,    -5,    -6,    -9,    -13,   -20,   -23,
                               -33,   -37,   -43,   -54,   -60,   -68)]
api.list.80  <- api.list.80 [c(-6,    -7,    -8,    -10,   -20,   -48,   -55)]
api.list.630 <- api.list.630[c(-4,    -8,    -10,   -15,   -21,   -36,   -37,
                               -39,   -43,   -52,   -58,   -65,   -74)]
api.list.605 <- api.list.605[c(-32)]
# No skips for Field 560
api.list.101 <- api.list.101[c(-15,   -19,   -55)]
api.list.610 <- api.list.610[c(-9)]
# No skips for Field 692
api.list.695 <- api.list.692[c(-6,    -24)]

# Create list with all the API field vectors
api.list.oow <- list(api.list.105,
                     api.list.72,
                     api.list.590,
                     api.list.117,
                     api.list.718,
                     api.list.55,
                     api.list.60,
                     api.list.65,
                     api.list.665,
                     api.list.710,
                     api.list.80,
                     api.list.630,
                     api.list.605,
                     api.list.560,
                     api.list.101,
                     api.list.610,
                     api.list.692,
                     api.list.695)

# Load results of loop below, or comment out and rerun to update
load(file.path(data_root, "coef_hyp_oow.rda"))

# # Define dataframes that will hold fit coefficients
# coef.hyp.oow <- vector("list", length(api.list.oow))
# 
# # Begin for-loop
# for (j in 1:length(api.list.oow)) {
#   api.list <- api.list.oow[[j]]
#   temp <- matrix(0, nrow = length(api.list), ncol = 4)
#   for (a in 1:length(api.list)) {
#     # Creates a subset 'w' from dataframe 'p.DAQ.0312' whose API# is = to the API#
#     # in step 'a' of the for-loop
#     w <- subset(p.oow, p_api == api.list[a])
#     if (length(w[,1]) >= 4) {
#       # Smooth the dataset and redefine smooth spline as dataframe
#       sm <- smooth.spline(w$time, w$p_oil_prod)
#       sm <- data.frame(sm$x, sm$y)
#       # Perform regression for oil from well
#       fit.hyp.oow <- nlsLM(sm.y ~ alpha*(1+theta*delta*sm.x)^(-1/theta),
#                            data = sm,
#                            start = list(alpha = sm$sm.y[1], theta = 1.78, delta = 1.16),
#                            control = list(maxiter=1000))
#       temp[a,1] <- coef(fit.hyp.oow)[1]
#       temp[a,2] <- coef(fit.hyp.oow)[2]
#       temp[a,3] <- coef(fit.hyp.oow)[3]
#       temp[a,4] <- summary(fit.hyp.oow)$sigma
#     }
#   }
#   coef.hyp.oow[[j]] <- temp
# }
# 
# # Save a list of the coefficient results so you don't have to do this again
# save(file=file.path(data_root, "coef_hyp_oow.rda"), list=c("coef.hyp.oow"))

# Reject outliers in coefficient dataset
for (i in 1:length(coef.hyp.oow)) {
  temp <- coef.hyp.oow[[i]]
  temp <- temp[which(temp[,1] < 100*summary(temp[,1])[5] &
                     temp[,2] < 100*summary(temp[,2])[5] &
                     temp[,3] < 100*summary(temp[,3])[5]),]
  coef.hyp.oow[[i]] <- temp
}

# Determine the PDF for each coefficient for each field
pdf.105a <- density(coef.hyp.oow[[1]][,1], from = 0);  pdf.105b <- density(coef.hyp.oow[[1]][,2], from = 0);  pdf.105c <- density(coef.hyp.oow[[1]][,3], from = 0)
pdf.72a  <- density(coef.hyp.oow[[2]][,1], from = 0);  pdf.72b  <- density(coef.hyp.oow[[2]][,2], from = 0);  pdf.72c  <- density(coef.hyp.oow[[2]][,3], from = 0)
pdf.590a <- density(coef.hyp.oow[[3]][,1], from = 0);  pdf.590b <- density(coef.hyp.oow[[3]][,2], from = 0);  pdf.590c <- density(coef.hyp.oow[[3]][,3], from = 0)
pdf.117a <- density(coef.hyp.oow[[4]][,1], from = 0);  pdf.117b <- density(coef.hyp.oow[[4]][,2], from = 0);  pdf.117c <- density(coef.hyp.oow[[4]][,3], from = 0)
pdf.718a <- density(coef.hyp.oow[[5]][,1], from = 0);  pdf.718b <- density(coef.hyp.oow[[5]][,2], from = 0);  pdf.718c <- density(coef.hyp.oow[[5]][,3], from = 0)
pdf.55a  <- density(coef.hyp.oow[[6]][,1], from = 0);  pdf.55b  <- density(coef.hyp.oow[[6]][,2], from = 0);  pdf.55c  <- density(coef.hyp.oow[[6]][,3], from = 0)
pdf.60a  <- density(coef.hyp.oow[[7]][,1], from = 0);  pdf.60b  <- density(coef.hyp.oow[[7]][,2], from = 0);  pdf.60c  <- density(coef.hyp.oow[[7]][,3], from = 0)
pdf.65a  <- density(coef.hyp.oow[[8]][,1], from = 0);  pdf.65b  <- density(coef.hyp.oow[[8]][,2], from = 0);  pdf.65c  <- density(coef.hyp.oow[[8]][,3], from = 0)
pdf.665a <- density(coef.hyp.oow[[9]][,1], from = 0);  pdf.665b <- density(coef.hyp.oow[[9]][,2], from = 0);  pdf.665c <- density(coef.hyp.oow[[9]][,3], from = 0)
pdf.710a <- density(coef.hyp.oow[[10]][,1], from = 0); pdf.710b <- density(coef.hyp.oow[[10]][,2], from = 0); pdf.710c <- density(coef.hyp.oow[[10]][,3], from = 0)
pdf.80a  <- density(coef.hyp.oow[[11]][,1], from = 0); pdf.80b  <- density(coef.hyp.oow[[11]][,2], from = 0); pdf.80c  <- density(coef.hyp.oow[[11]][,3], from = 0)
pdf.630a <- density(coef.hyp.oow[[12]][,1], from = 0); pdf.630b <- density(coef.hyp.oow[[12]][,2], from = 0); pdf.630c <- density(coef.hyp.oow[[12]][,3], from = 0)
pdf.605a <- density(coef.hyp.oow[[13]][,1], from = 0); pdf.605b <- density(coef.hyp.oow[[13]][,2], from = 0); pdf.605c <- density(coef.hyp.oow[[13]][,3], from = 0)
pdf.560a <- density(coef.hyp.oow[[14]][,1], from = 0); pdf.560b <- density(coef.hyp.oow[[14]][,2], from = 0); pdf.560c <- density(coef.hyp.oow[[14]][,3], from = 0)
pdf.101a <- density(coef.hyp.oow[[15]][,1], from = 0); pdf.101b <- density(coef.hyp.oow[[15]][,2], from = 0); pdf.101c <- density(coef.hyp.oow[[15]][,3], from = 0)
pdf.610a <- density(coef.hyp.oow[[16]][,1], from = 0); pdf.610b <- density(coef.hyp.oow[[16]][,2], from = 0); pdf.610c <- density(coef.hyp.oow[[16]][,3], from = 0)
pdf.692a <- density(coef.hyp.oow[[17]][,1], from = 0); pdf.692b <- density(coef.hyp.oow[[17]][,2], from = 0); pdf.692c <- density(coef.hyp.oow[[17]][,3], from = 0)
pdf.695a <- density(coef.hyp.oow[[18]][,1], from = 0); pdf.695b <- density(coef.hyp.oow[[18]][,2], from = 0); pdf.695c <- density(coef.hyp.oow[[18]][,3], from = 0)

# Create dataframe of pdf x-values
pdf.oow <- data.frame(pdf.105a$x, pdf.105b$x, pdf.105c$x,
                      pdf.72a$x,  pdf.72b$x,  pdf.72c$x,
                      pdf.590a$x, pdf.590b$x, pdf.590c$x,
                      pdf.117a$x, pdf.117b$x, pdf.117c$x,
                      pdf.718a$x, pdf.718b$x, pdf.718c$x,
                      pdf.55a$x,  pdf.55b$x,  pdf.55c$x,
                      pdf.60a$x,  pdf.60b$x,  pdf.60c$x,
                      pdf.65a$x,  pdf.65b$x,  pdf.65c$x,
                      pdf.665a$x, pdf.665b$x, pdf.665c$x,
                      pdf.710a$x, pdf.710b$x, pdf.710c$x,
                      pdf.80a$x,  pdf.80b$x,  pdf.80c$x,
                      pdf.630a$x, pdf.630b$x, pdf.630c$x,
                      pdf.605a$x, pdf.605b$x, pdf.605c$x,
                      pdf.560a$x, pdf.560b$x, pdf.560c$x,
                      pdf.101a$x, pdf.101b$x, pdf.101c$x,
                      pdf.610a$x, pdf.610b$x, pdf.610c$x,
                      pdf.692a$x, pdf.692b$x, pdf.692c$x,
                      pdf.695a$x, pdf.695b$x, pdf.695c$x)

# Determine the CDF for each coefficient for each field
cdf.105a <- cumsum(pdf.105a$y * diff(pdf.105a$x[1:2])); cdf.105b <- cumsum(pdf.105b$y * diff(pdf.105b$x[1:2])); cdf.105c <- cumsum(pdf.105c$y * diff(pdf.105c$x[1:2]))
cdf.72a  <- cumsum(pdf.72a$y  * diff(pdf.72a$x[1:2]));  cdf.72b  <- cumsum(pdf.72b$y  * diff(pdf.72b$x[1:2]));  cdf.72c  <- cumsum(pdf.72c$y  * diff(pdf.72c$x[1:2]))
cdf.590a <- cumsum(pdf.590a$y * diff(pdf.590a$x[1:2])); cdf.590b <- cumsum(pdf.590b$y * diff(pdf.590b$x[1:2])); cdf.590c <- cumsum(pdf.590c$y * diff(pdf.590c$x[1:2]))
cdf.117a <- cumsum(pdf.117a$y * diff(pdf.117a$x[1:2])); cdf.117b <- cumsum(pdf.117b$y * diff(pdf.117b$x[1:2])); cdf.117c <- cumsum(pdf.117c$y * diff(pdf.117c$x[1:2]))
cdf.718a <- cumsum(pdf.718a$y * diff(pdf.718a$x[1:2])); cdf.718b <- cumsum(pdf.718b$y * diff(pdf.718b$x[1:2])); cdf.718c <- cumsum(pdf.718c$y * diff(pdf.718c$x[1:2]))
cdf.55a  <- cumsum(pdf.55a$y  * diff(pdf.55a$x[1:2]));  cdf.55b  <- cumsum(pdf.55b$y  * diff(pdf.55b$x[1:2]));  cdf.55c  <- cumsum(pdf.55c$y  * diff(pdf.55c$x[1:2]))
cdf.60a  <- cumsum(pdf.60a$y  * diff(pdf.60a$x[1:2]));  cdf.60b  <- cumsum(pdf.60b$y  * diff(pdf.60b$x[1:2]));  cdf.60c  <- cumsum(pdf.60c$y  * diff(pdf.60c$x[1:2]))
cdf.65a  <- cumsum(pdf.65a$y  * diff(pdf.65a$x[1:2]));  cdf.65b  <- cumsum(pdf.65b$y  * diff(pdf.65b$x[1:2]));  cdf.65c  <- cumsum(pdf.65c$y  * diff(pdf.65c$x[1:2]))
cdf.665a <- cumsum(pdf.665a$y * diff(pdf.665a$x[1:2])); cdf.665b <- cumsum(pdf.665b$y * diff(pdf.665b$x[1:2])); cdf.665c <- cumsum(pdf.665c$y * diff(pdf.665c$x[1:2]))
cdf.710a <- cumsum(pdf.710a$y * diff(pdf.710a$x[1:2])); cdf.710b <- cumsum(pdf.710b$y * diff(pdf.710b$x[1:2])); cdf.710c <- cumsum(pdf.710c$y * diff(pdf.710c$x[1:2]))
cdf.80a  <- cumsum(pdf.80a$y  * diff(pdf.80a$x[1:2]));  cdf.80b  <- cumsum(pdf.80b$y  * diff(pdf.80b$x[1:2]));  cdf.80c  <- cumsum(pdf.80c$y  * diff(pdf.80c$x[1:2]))
cdf.630a <- cumsum(pdf.630a$y * diff(pdf.630a$x[1:2])); cdf.630b <- cumsum(pdf.630b$y * diff(pdf.630b$x[1:2])); cdf.630c <- cumsum(pdf.630c$y * diff(pdf.630c$x[1:2]))
cdf.605a <- cumsum(pdf.605a$y * diff(pdf.605a$x[1:2])); cdf.605b <- cumsum(pdf.605b$y * diff(pdf.605b$x[1:2])); cdf.605c <- cumsum(pdf.605c$y * diff(pdf.605c$x[1:2]))
cdf.560a <- cumsum(pdf.560a$y * diff(pdf.560a$x[1:2])); cdf.560b <- cumsum(pdf.560b$y * diff(pdf.560b$x[1:2])); cdf.560c <- cumsum(pdf.560c$y * diff(pdf.560c$x[1:2]))
cdf.101a <- cumsum(pdf.101a$y * diff(pdf.101a$x[1:2])); cdf.101b <- cumsum(pdf.101b$y * diff(pdf.101b$x[1:2])); cdf.101c <- cumsum(pdf.101c$y * diff(pdf.101c$x[1:2]))
cdf.610a <- cumsum(pdf.610a$y * diff(pdf.610a$x[1:2])); cdf.610b <- cumsum(pdf.610b$y * diff(pdf.610b$x[1:2])); cdf.610c <- cumsum(pdf.610c$y * diff(pdf.610c$x[1:2]))
cdf.692a <- cumsum(pdf.692a$y * diff(pdf.692a$x[1:2])); cdf.692b <- cumsum(pdf.692b$y * diff(pdf.692b$x[1:2])); cdf.692c <- cumsum(pdf.692c$y * diff(pdf.692c$x[1:2]))
cdf.695a <- cumsum(pdf.695a$y * diff(pdf.695a$x[1:2])); cdf.695b <- cumsum(pdf.695b$y * diff(pdf.695b$x[1:2])); cdf.695c <- cumsum(pdf.695c$y * diff(pdf.695c$x[1:2]))

# Create CDF matrix for export
cdf.oow <- matrix(0, nrow = 512, ncol = 54)

# Assign cdf results to each column
cdf.oow[, 1] <- cdf.105a; cdf.oow[, 2] <- cdf.105b; cdf.oow[, 3] <- cdf.105c
cdf.oow[, 4] <- cdf.72a;  cdf.oow[, 5] <- cdf.72b;  cdf.oow[, 6] <- cdf.72c
cdf.oow[, 7] <- cdf.590a; cdf.oow[, 8] <- cdf.590b; cdf.oow[, 9] <- cdf.590c
cdf.oow[,10] <- cdf.117a; cdf.oow[,11] <- cdf.117b; cdf.oow[,12] <- cdf.117c
cdf.oow[,13] <- cdf.718a; cdf.oow[,14] <- cdf.718b; cdf.oow[,15] <- cdf.718c
cdf.oow[,16] <- cdf.55a;  cdf.oow[,17] <- cdf.55b;  cdf.oow[,18] <- cdf.55c
cdf.oow[,19] <- cdf.60a;  cdf.oow[,20] <- cdf.60b;  cdf.oow[,21] <- cdf.60c
cdf.oow[,22] <- cdf.65a;  cdf.oow[,23] <- cdf.65b;  cdf.oow[,24] <- cdf.65c
cdf.oow[,25] <- cdf.665a; cdf.oow[,26] <- cdf.665b; cdf.oow[,27] <- cdf.665c
cdf.oow[,28] <- cdf.710a; cdf.oow[,29] <- cdf.710b; cdf.oow[,30] <- cdf.710c
cdf.oow[,31] <- cdf.80a;  cdf.oow[,32] <- cdf.80b;  cdf.oow[,33] <- cdf.80c
cdf.oow[,34] <- cdf.630a; cdf.oow[,35] <- cdf.630b; cdf.oow[,36] <- cdf.630c
cdf.oow[,37] <- cdf.605a; cdf.oow[,38] <- cdf.605b; cdf.oow[,39] <- cdf.605c
cdf.oow[,40] <- cdf.560a; cdf.oow[,41] <- cdf.560b; cdf.oow[,42] <- cdf.560c
cdf.oow[,43] <- cdf.101a; cdf.oow[,44] <- cdf.101b; cdf.oow[,45] <- cdf.101c
cdf.oow[,46] <- cdf.610a; cdf.oow[,47] <- cdf.610b; cdf.oow[,48] <- cdf.610c
cdf.oow[,49] <- cdf.692a; cdf.oow[,50] <- cdf.692b; cdf.oow[,51] <- cdf.692c
cdf.oow[,52] <- cdf.695a; cdf.oow[,53] <- cdf.695b; cdf.oow[,54] <- cdf.695c

# Normalize in case of rounding error
for (a in 1:length(cdf.oow[1,])) {
  cdf.oow[,a] <- cdf.oow[,a] / max(cdf.oow[,a])
}

# Redefine as dataframe and name columns for each field
cdf.oow <- data.frame(cdf.oow)
names(cdf.oow) <- c("105a", "105b", "105c",
                    "72a",  "72b",   "72c",
                    "590a", "590b", "590c",
                    "117a", "117b", "117c",
                    "718a", "718b", "718c",
                    "55a",  "55b",  "55c",
                    "60a",  "60b",  "60c",
                    "65a",  "65b",  "65c",
                    "665a", "665b", "665c",
                    "710a", "710b", "710c",
                    "80a",  "80b",  "80c",
                    "630a", "630b", "630c",
                    "605a", "605b", "605c",
                    "560a", "560b", "560c",
                    "101a", "101b", "101c",
                    "610a", "610b", "610c",
                    "692a", "692b", "692c",
                    "695a", "695b", "695c")

# # Export PDF and CDF dataframes
# save(file=file.path(data_root, "pdf_oow.rda"), list=c("pdf.oow"))
# save(file=file.path(data_root, "cdf_oow.rda"), list=c("cdf.oow"))

#-------------------------------------------------------------------------------
# Regression to Individual Gas Wells by Field
#-------------------------------------------------------------------------------
# # Gas field number listing (95% of APD count & 95% of production)
# field.gw <- c(630, 710, 600, 617, 640, 547, 670, 635, 618, 622, 610, 590, 791,
#               72, 105, 55)

# Create list of unique API #'s and their Field # in p that are gas wells
api.gw <- sqldf("select distinct(p_api), w_field_num from p where w_well_type = 'GW'")

# Get API list for each field (cutoff of 95% of APD count & 95% of production)
api.list.630 <- subset(api.gw, w_field_num == 630, row.names = FALSE); api.list.630 <- api.list.630[,1]
api.list.710 <- subset(api.gw, w_field_num == 710, row.names = FALSE); api.list.710 <- api.list.710[,1]
api.list.600 <- subset(api.gw, w_field_num == 600, row.names = FALSE); api.list.600 <- api.list.600[,1]
api.list.617 <- subset(api.gw, w_field_num == 617, row.names = FALSE); api.list.617 <- api.list.617[,1]
api.list.640 <- subset(api.gw, w_field_num == 640, row.names = FALSE); api.list.640 <- api.list.640[,1]
api.list.547 <- subset(api.gw, w_field_num == 547, row.names = FALSE); api.list.547 <- api.list.547[,1]
api.list.670 <- subset(api.gw, w_field_num == 670, row.names = FALSE); api.list.670 <- api.list.670[,1]
api.list.635 <- subset(api.gw, w_field_num == 635, row.names = FALSE); api.list.635 <- api.list.635[,1]
api.list.618 <- subset(api.gw, w_field_num == 618, row.names = FALSE); api.list.618 <- api.list.618[,1]
api.list.622 <- subset(api.gw, w_field_num == 622, row.names = FALSE); api.list.622 <- api.list.622[,1]
api.list.610 <- subset(api.gw, w_field_num == 610, row.names = FALSE); api.list.610 <- api.list.610[,1]
api.list.590 <- subset(api.gw, w_field_num == 590, row.names = FALSE); api.list.590 <- api.list.590[,1]
api.list.791 <- subset(api.gw, w_field_num == 791, row.names = FALSE); api.list.791 <- api.list.791[,1]
api.list.72  <- subset(api.gw, w_field_num == 72,  row.names = FALSE); api.list.72  <- api.list.72[,1]
api.list.105 <- subset(api.gw, w_field_num == 105, row.names = FALSE); api.list.105 <- api.list.105[,1]
api.list.55  <- subset(api.gw, w_field_num == 55,  row.names = FALSE); api.list.55  <- api.list.55[,1]

# Enter indices here of wells that fail to converge to skip them in subsequent
# iterations of for-loop
api.list.630 <- api.list.630[c(-3,    -44,   -69,   -88,   -118,  -162,  -187,
                               -259,  -286,  -306,  -384,  -509,  -681,  -1089,
                               -1592, -2787, -3173, -3608, -4034, -4063, -4339,
                               -4714, -4826, -5008, -5012, -5013, -5027, -5028,
                               -5046, -5106, -5142, -5147)]
api.list.600 <- api.list.600[c(-2,    -21,  -22,    -25,   -54)]
api.list.635 <- api.list.635[c(-29)]
api.list.105 <- api.list.105[c(-2)]
# No skips for Fields 710, 617, 640, 547, 670, 618, 622, 610, 590, 791, 72, 55

# Create list with all the API field vectors
api.list.ggw <- list(api.list.630,
                     api.list.710,
                     api.list.600,
                     api.list.617,
                     api.list.640,
                     api.list.547,
                     api.list.670,
                     api.list.635,
                     api.list.618,
                     api.list.622,
                     api.list.610,
                     api.list.590,
                     api.list.791,
                     api.list.72,
                     api.list.105,
                     api.list.55)

# Load results of loop below, or comment out and rerun to update
load(file.path(data_root, "coef_hyp_ggw.rda"))

# # Define dataframes that will hold fit coefficients
# coef.hyp.ggw <- vector("list", length(api.list.ggw))
# 
# # Begin for-loop
# for (j in 1:length(api.list.ggw)) {
#   api.list <- api.list.ggw[[j]]
#   temp <- matrix(0, nrow = length(api.list), ncol = 4)
#   for (a in 1:length(api.list)) {
#     # Creates a subset 'w' from dataframe 'p.DAQ.0312' whose API# is = to the API#
#     # in step 'a' of the for-loop
#     w <- subset(p.ggw, p_api == api.list[a])
#     if (length(w[,1]) >= 4) {
#       # Smooth the dataset and redefine smooth spline as dataframe
#       sm <- smooth.spline(w$time, w$p_gas_prod)
#       sm <- data.frame(sm$x, sm$y)
#       # Perform regression for oil from well
#       fit.hyp.ggw <- nlsLM(sm.y ~ alpha*(1+theta*delta*sm.x)^(-1/theta),
#                            data = sm,
#                            start = list(alpha = 33000, theta = 1.32, delta = 0.24),
#                            control = list(maxiter=1000))
#       temp[a,1] <- coef(fit.hyp.ggw)[1]
#       temp[a,2] <- coef(fit.hyp.ggw)[2]
#       temp[a,3] <- coef(fit.hyp.ggw)[3]
#       temp[a,4] <- summary(fit.hyp.ggw)$sigma
#     }
#   }
#   coef.hyp.ggw[[j]] <- temp
# }
# 
# # Save a list of the coefficient results so you don't have to do this again
# save(file=file.path(data_root, "coef_hyp_ggw.rda"), list=c("coef.hyp.ggw"))

# Reject outliers in coefficient dataset
for (i in 1:length(coef.hyp.ggw)) {
  temp <- coef.hyp.ggw[[i]]
  temp <- temp[which(temp[,1] < 100*summary(temp[,1])[5] &
                     temp[,2] < 100*summary(temp[,2])[5] &
                     temp[,3] < 100*summary(temp[,3])[5]),]
  coef.hyp.ggw[[i]] <- temp
}


# Determine the PDF for each coefficient for each field
pdf.630a <- density(coef.hyp.ggw[[1]][,1], from = 0);  pdf.630b <- density(coef.hyp.ggw[[1]][,2], from = 0);  pdf.630c <- density(coef.hyp.ggw[[1]][,3], from = 0)
pdf.710a <- density(coef.hyp.ggw[[2]][,1], from = 0);  pdf.710b <- density(coef.hyp.ggw[[2]][,2], from = 0);  pdf.710c <- density(coef.hyp.ggw[[2]][,3], from = 0)
pdf.600a <- density(coef.hyp.ggw[[3]][,1], from = 0);  pdf.600b <- density(coef.hyp.ggw[[3]][,2], from = 0);  pdf.600c <- density(coef.hyp.ggw[[3]][,3], from = 0)
pdf.617a <- density(coef.hyp.ggw[[4]][,1], from = 0);  pdf.617b <- density(coef.hyp.ggw[[4]][,2], from = 0);  pdf.617c <- density(coef.hyp.ggw[[4]][,3], from = 0)
pdf.640a <- density(coef.hyp.ggw[[5]][,1], from = 0);  pdf.640b <- density(coef.hyp.ggw[[5]][,2], from = 0);  pdf.640c <- density(coef.hyp.ggw[[5]][,3], from = 0)
pdf.547a <- density(coef.hyp.ggw[[6]][,1], from = 0);  pdf.547b <- density(coef.hyp.ggw[[6]][,2], from = 0);  pdf.547c <- density(coef.hyp.ggw[[6]][,3], from = 0)
pdf.670a <- density(coef.hyp.ggw[[7]][,1], from = 0);  pdf.670b <- density(coef.hyp.ggw[[7]][,2], from = 0);  pdf.670c <- density(coef.hyp.ggw[[7]][,3], from = 0)
pdf.635a <- density(coef.hyp.ggw[[8]][,1], from = 0);  pdf.635b <- density(coef.hyp.ggw[[8]][,2], from = 0);  pdf.635c <- density(coef.hyp.ggw[[8]][,3], from = 0)
pdf.618a <- density(coef.hyp.ggw[[9]][,1], from = 0);  pdf.618b <- density(coef.hyp.ggw[[9]][,2], from = 0);  pdf.618c <- density(coef.hyp.ggw[[9]][,3], from = 0)
pdf.622a <- density(coef.hyp.ggw[[10]][,1], from = 0);  pdf.622b <- density(coef.hyp.ggw[[10]][,2], from = 0);  pdf.622c <- density(coef.hyp.ggw[[10]][,3], from = 0)
pdf.610a <- density(coef.hyp.ggw[[11]][,1], from = 0);  pdf.610b <- density(coef.hyp.ggw[[11]][,2], from = 0);  pdf.610c <- density(coef.hyp.ggw[[11]][,3], from = 0)
pdf.590a <- density(coef.hyp.ggw[[12]][,1], from = 0);  pdf.590b <- density(coef.hyp.ggw[[12]][,2], from = 0);  pdf.590c <- density(coef.hyp.ggw[[12]][,3], from = 0)
pdf.791a <- density(coef.hyp.ggw[[13]][,1], from = 0);  pdf.791b <- density(coef.hyp.ggw[[13]][,2], from = 0);  pdf.791c <- density(coef.hyp.ggw[[13]][,3], from = 0)
pdf.72a  <- density(coef.hyp.ggw[[14]][,1], from = 0);  pdf.72b  <- density(coef.hyp.ggw[[14]][,2], from = 0);  pdf.72c  <- density(coef.hyp.ggw[[14]][,3], from = 0)
pdf.105a <- density(coef.hyp.ggw[[15]][,1], from = 0);  pdf.105b <- density(coef.hyp.ggw[[15]][,2], from = 0);  pdf.105c <- density(coef.hyp.ggw[[15]][,3], from = 0)
pdf.55a  <- density(coef.hyp.ggw[[16]][,1], from = 0);  pdf.55b  <- density(coef.hyp.ggw[[16]][,2], from = 0);  pdf.55c  <- density(coef.hyp.ggw[[16]][,3], from = 0)

# Create dataframe of pdf x-values
pdf.ggw <- data.frame(pdf.630a$x, pdf.630b$x, pdf.630c$x,
                      pdf.710a$x, pdf.710b$x, pdf.710c$x,
                      pdf.600a$x, pdf.600b$x, pdf.600c$x,
                      pdf.617a$x, pdf.617b$x, pdf.617c$x,
                      pdf.640a$x, pdf.640b$x, pdf.640c$x,
                      pdf.547a$x, pdf.547b$x, pdf.547c$x,
                      pdf.670a$x, pdf.670b$x, pdf.670c$x,
                      pdf.635a$x, pdf.635b$x, pdf.635c$x,
                      pdf.618a$x, pdf.618b$x, pdf.618c$x,
                      pdf.622a$x, pdf.622b$x, pdf.622c$x,
                      pdf.610a$x, pdf.610b$x, pdf.610c$x,
                      pdf.590a$x, pdf.590b$x, pdf.590c$x,
                      pdf.791a$x, pdf.791b$x, pdf.791c$x,
                      pdf.72a$x,  pdf.72b$x,  pdf.72c$x,
                      pdf.105a$x, pdf.105b$x, pdf.105c$x,
                      pdf.55a$x,  pdf.55b$x,  pdf.55c$x)

# Determine the CDF for each coefficient for each field
cdf.630a <- cumsum(pdf.630a$y * diff(pdf.630a$x[1:2])); cdf.630b <- cumsum(pdf.630b$y * diff(pdf.630b$x[1:2])); cdf.630c <- cumsum(pdf.630c$y * diff(pdf.630c$x[1:2]))
cdf.710a <- cumsum(pdf.710a$y * diff(pdf.710a$x[1:2])); cdf.710b <- cumsum(pdf.710b$y * diff(pdf.710b$x[1:2])); cdf.710c <- cumsum(pdf.710c$y * diff(pdf.710c$x[1:2]))
cdf.600a <- cumsum(pdf.600a$y * diff(pdf.600a$x[1:2])); cdf.600b <- cumsum(pdf.600b$y * diff(pdf.600b$x[1:2])); cdf.600c <- cumsum(pdf.600c$y * diff(pdf.600c$x[1:2]))
cdf.617a <- cumsum(pdf.617a$y * diff(pdf.617a$x[1:2])); cdf.617b <- cumsum(pdf.617b$y * diff(pdf.617b$x[1:2])); cdf.617c <- cumsum(pdf.617c$y * diff(pdf.617c$x[1:2]))
cdf.640a <- cumsum(pdf.640a$y * diff(pdf.640a$x[1:2])); cdf.640b <- cumsum(pdf.640b$y * diff(pdf.640b$x[1:2])); cdf.640c <- cumsum(pdf.640c$y * diff(pdf.640c$x[1:2]))
cdf.547a <- cumsum(pdf.547a$y * diff(pdf.547a$x[1:2])); cdf.547b <- cumsum(pdf.547b$y * diff(pdf.547b$x[1:2])); cdf.547c <- cumsum(pdf.547c$y * diff(pdf.547c$x[1:2]))
cdf.670a <- cumsum(pdf.670a$y * diff(pdf.670a$x[1:2])); cdf.670b <- cumsum(pdf.670b$y * diff(pdf.670b$x[1:2])); cdf.670c <- cumsum(pdf.670c$y * diff(pdf.670c$x[1:2]))
cdf.635a <- cumsum(pdf.635a$y * diff(pdf.635a$x[1:2])); cdf.635b <- cumsum(pdf.635b$y * diff(pdf.635b$x[1:2])); cdf.635c <- cumsum(pdf.635c$y * diff(pdf.635c$x[1:2]))
cdf.618a <- cumsum(pdf.618a$y * diff(pdf.618a$x[1:2])); cdf.618b <- cumsum(pdf.618b$y * diff(pdf.618b$x[1:2])); cdf.618c <- cumsum(pdf.618c$y * diff(pdf.618c$x[1:2]))
cdf.622a <- cumsum(pdf.622a$y * diff(pdf.622a$x[1:2])); cdf.622b <- cumsum(pdf.622b$y * diff(pdf.622b$x[1:2])); cdf.622c <- cumsum(pdf.622c$y * diff(pdf.622c$x[1:2]))
cdf.610a <- cumsum(pdf.610a$y * diff(pdf.610a$x[1:2])); cdf.610b <- cumsum(pdf.610b$y * diff(pdf.610b$x[1:2])); cdf.610c <- cumsum(pdf.610c$y * diff(pdf.610c$x[1:2]))
cdf.590a <- cumsum(pdf.590a$y * diff(pdf.590a$x[1:2])); cdf.590b <- cumsum(pdf.590b$y * diff(pdf.590b$x[1:2])); cdf.590c <- cumsum(pdf.590c$y * diff(pdf.590c$x[1:2]))
cdf.791a <- cumsum(pdf.791a$y * diff(pdf.791a$x[1:2])); cdf.791b <- cumsum(pdf.791b$y * diff(pdf.791b$x[1:2])); cdf.791c <- cumsum(pdf.791c$y * diff(pdf.791c$x[1:2]))
cdf.72a  <- cumsum(pdf.72a$y  * diff(pdf.72a$x[1:2]));  cdf.72b  <- cumsum(pdf.72b$y  * diff(pdf.72b$x[1:2]));  cdf.72c  <- cumsum(pdf.72c$y  * diff(pdf.72c$x[1:2]))
cdf.105a <- cumsum(pdf.105a$y * diff(pdf.105a$x[1:2])); cdf.105b <- cumsum(pdf.105b$y * diff(pdf.105b$x[1:2])); cdf.105c <- cumsum(pdf.105c$y * diff(pdf.105c$x[1:2]))
cdf.55a  <- cumsum(pdf.55a$y  * diff(pdf.55a$x[1:2]));  cdf.55b  <- cumsum(pdf.55b$y  * diff(pdf.55b$x[1:2]));  cdf.55c  <- cumsum(pdf.55c$y  * diff(pdf.55c$x[1:2]))


# Create CDF matrix for export
cdf.ggw <- matrix(0, nrow = 512, ncol = 3*length(api.list.ggw))

# Assign cdf results to each column
cdf.ggw[, 1] <- cdf.630a; cdf.ggw[, 2] <- cdf.630b; cdf.ggw[, 3] <- cdf.630c
cdf.ggw[, 4] <- cdf.710a; cdf.ggw[, 5] <- cdf.710b; cdf.ggw[, 6] <- cdf.710c
cdf.ggw[, 7] <- cdf.600a; cdf.ggw[, 8] <- cdf.600b; cdf.ggw[, 9] <- cdf.600c
cdf.ggw[,10] <- cdf.617a; cdf.ggw[,11] <- cdf.617b; cdf.ggw[,12] <- cdf.617c
cdf.ggw[,13] <- cdf.640a; cdf.ggw[,14] <- cdf.640b; cdf.ggw[,15] <- cdf.640c
cdf.ggw[,16] <- cdf.547a; cdf.ggw[,17] <- cdf.547b; cdf.ggw[,18] <- cdf.547c
cdf.ggw[,19] <- cdf.670a; cdf.ggw[,20] <- cdf.670b; cdf.ggw[,21] <- cdf.670c
cdf.ggw[,22] <- cdf.635a; cdf.ggw[,23] <- cdf.635b; cdf.ggw[,24] <- cdf.635c
cdf.ggw[,25] <- cdf.618a; cdf.ggw[,26] <- cdf.618b; cdf.ggw[,27] <- cdf.618c
cdf.ggw[,28] <- cdf.622a; cdf.ggw[,29] <- cdf.622b; cdf.ggw[,30] <- cdf.622c
cdf.ggw[,31] <- cdf.610a; cdf.ggw[,32] <- cdf.610b; cdf.ggw[,33] <- cdf.610c
cdf.ggw[,34] <- cdf.590a; cdf.ggw[,35] <- cdf.590b; cdf.ggw[,36] <- cdf.590c
cdf.ggw[,37] <- cdf.791a; cdf.ggw[,38] <- cdf.791b; cdf.ggw[,39] <- cdf.791c
cdf.ggw[,40] <- cdf.72a;  cdf.ggw[,41] <- cdf.72b;  cdf.ggw[,42] <- cdf.72c
cdf.ggw[,43] <- cdf.105a; cdf.ggw[,44] <- cdf.105b; cdf.ggw[,45] <- cdf.105c
cdf.ggw[,46] <- cdf.55a;  cdf.ggw[,47] <- cdf.55b;  cdf.ggw[,48] <- cdf.55c

# Normalize in case of rounding error
for (a in 1:length(cdf.ggw[1,])) {
  cdf.ggw[,a] <- cdf.ggw[,a] / max(cdf.ggw[,a])
}

# Redefine as dataframe and name columns for each field
cdf.ggw <- data.frame(cdf.ggw)
names(cdf.ggw) <- c("630a", "630b", "630c",
                    "710a", "710b", "710c",
                    "600a", "600b", "600c",
                    "617a", "617b", "617c",
                    "640a", "640b", "640c",
                    "547a", "547b", "547c",
                    "670a", "670b", "670c",
                    "635a", "635b", "635c",
                    "618a", "618b", "618c",
                    "622a", "622b", "622c",
                    "610a", "610b", "610c",
                    "590a", "590b", "590c",
                    "791a", "791b", "791c",
                    "72a",  "72b",  "72c",
                    "105a", "105b", "105c",
                    "55a",  "55b",  "55c")

# # Export PDF and CDF dataframes
# save(file=file.path(data_root, "pdf_ggw.rda"), list=c("pdf.ggw"))
# save(file=file.path(data_root, "cdf_ggw.rda"), list=c("cdf.ggw"))

#-------------------------------------------------------------------------------
# Plot results
#-------------------------------------------------------------------------------
labels.ow <- c("105 Coefficient a", "105 Coefficient b", "105 Coefficient c",
               "72 Coefficient a",  "72 Coefficient b",  "72 Coefficient c",
               "590 Coefficient a", "590 Coefficient b", "590 Coefficient c",
               "117 Coefficient a", "117 Coefficient b", "117 Coefficient c",
               "718 Coefficient a", "718 Coefficient b", "718 Coefficient c",
               "55 Coefficient a",  "55 Coefficient b",  "55 Coefficient c",
               "60 Coefficient a",  "60 Coefficient b",  "60 Coefficient c",
               "65 Coefficient a",  "65 Coefficient b",  "65 Coefficient c",
               "665 Coefficient a", "665 Coefficient b", "665 Coefficient c",
               "710 Coefficient a", "710 Coefficient b", "710 Coefficient c",
               "80 Coefficient a",  "80 Coefficient b",  "80 Coefficient c",
               "630 Coefficient a", "630 Coefficient b", "630 Coefficient c",
               "605 Coefficient a", "605 Coefficient b", "605 Coefficient c",
               "560 Coefficient a", "560 Coefficient b", "560 Coefficient c",
               "101 Coefficient a", "101 Coefficient b", "101 Coefficient c",
               "610 Coefficient a", "610 Coefficient b", "610 Coefficient c",
               "692 Coefficient a", "692 Coefficient b", "692 Coefficient c",
               "695 Coefficient a", "695 Coefficient b", "695 Coefficient c")

labels.gw <- c("630 Coefficient a", "630 Coefficient b", "630 Coefficient c",
               "710 Coefficient a", "710 Coefficient b", "710 Coefficient c",
               "600 Coefficient a", "600 Coefficient b", "600 Coefficient c",
               "617 Coefficient a", "617 Coefficient b", "617 Coefficient c",
               "640 Coefficient a", "640 Coefficient b", "640 Coefficient c",
               "547 Coefficient a", "547 Coefficient b", "547 Coefficient c",
               "670 Coefficient a", "670 Coefficient b", "670 Coefficient c",
               "635 Coefficient a", "635 Coefficient b", "635 Coefficient c",
               "618 Coefficient a", "618 Coefficient b", "618 Coefficient c",
               "622 Coefficient a", "622 Coefficient b", "622 Coefficient c",
               "610 Coefficient a", "610 Coefficient b", "610 Coefficient c",
               "590 Coefficient a", "590 Coefficient b", "590 Coefficient c",
               "791 Coefficient a", "791 Coefficient b", "791 Coefficient c",
               "72 Coefficient a",  "72 Coefficient b",  "72 Coefficient c",
               "105 Coefficient a", "105 Coefficient b", "105 Coefficient c",
               "55 Coefficient a",  "55 Coefficient b",  "55 Coefficient c")

pdf.plot.list.ow <- list(pdf.105a, pdf.105b, pdf.105c,
                         pdf.72a,  pdf.72b,  pdf.72c,
                         pdf.590a, pdf.590b, pdf.590c,
                         pdf.117a, pdf.117b, pdf.117c,
                         pdf.718a, pdf.718b, pdf.718c,
                         pdf.55a,  pdf.55b,  pdf.55c,
                         pdf.60a,  pdf.60b,  pdf.60c,
                         pdf.65a,  pdf.65b,  pdf.65c,
                         pdf.665a, pdf.665b, pdf.665c,
                         pdf.710a, pdf.710b, pdf.710c,
                         pdf.80a,  pdf.80b,  pdf.80c,
                         pdf.630a, pdf.630b, pdf.630c,
                         pdf.605a, pdf.605b, pdf.605c,
                         pdf.560a, pdf.560b, pdf.560c,
                         pdf.101a, pdf.101b, pdf.101c,
                         pdf.610a, pdf.610b, pdf.610c,
                         pdf.692a, pdf.692b, pdf.692c,
                         pdf.695a, pdf.695b, pdf.695c)

pdf.plot.list.gw <- list(pdf.630a, pdf.630b, pdf.630c,
                         pdf.710a, pdf.710b, pdf.710c,
                         pdf.600a, pdf.600b, pdf.600c,
                         pdf.617a, pdf.617b, pdf.617c,
                         pdf.640a, pdf.640b, pdf.640c,
                         pdf.547a, pdf.547b, pdf.547c,
                         pdf.670a, pdf.670b, pdf.670c,
                         pdf.635a, pdf.635b, pdf.635c,
                         pdf.618a, pdf.618b, pdf.618c,
                         pdf.622a, pdf.622b, pdf.622c,
                         pdf.610a, pdf.610b, pdf.610c,
                         pdf.590a, pdf.590b, pdf.590c,
                         pdf.791a, pdf.791b, pdf.791c,
                         pdf.72a,  pdf.72b,  pdf.72c,
                         pdf.105a, pdf.105b, pdf.105c,
                         pdf.55a,  pdf.55b,  pdf.55c)

pdf(file = "PDF results for OOW from declineCurve_v4.pdf")
for (i in 1:length(pdf.plot.list.ow)) {
  plot(pdf.plot.list.ow[[i]],
       main = paste("PDF of Oil from Oil Wells for Field", labels.ow[i], sep = " "))
}
dev.off()

pdf(file = "PDF results for GGW from declineCurve_v4.pdf")
for (i in 1:length(pdf.plot.list.gw)) {
  plot(pdf.plot.list.gw[[i]],
       main = paste("PDF of Gas from Gas Wells for Field", labels.gw[i], sep = " "))
}
dev.off()