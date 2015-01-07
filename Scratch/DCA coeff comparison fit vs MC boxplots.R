# Load fit results --------------------------------------------------------
load(file.path(path$data, "DCA_fits_v2.rda"))

# Only fitted wells
mo <- mo[which(mo$fit.1 == 1),]
mg <- mg[which(mg$fit.1 == 1),]


# Data for oil ------------------------------------------------------------

# Get row index of fits for fields we're specifically fitting
ind <- which(mo$w_field_num == opt$field[1] |
               mo$w_field_num == opt$field[2] |
               mo$w_field_num == opt$field[3] |
               mo$w_field_num == opt$field[4] |
               mo$w_field_num == opt$field[5] |
               mo$w_field_num == opt$field[6] |
               mo$w_field_num == opt$field[7] |
               mo$w_field_num == opt$field[8] |
               mo$w_field_num == opt$field[9] |
               mo$w_field_num == opt$field[10])

# Make data.frame of field and DCA coefficient values, then rename
ocf <- data.frame(mo$w_field_num[ind],
                  mo$qo.1[ind],
                  mo$b.1[ind],
                  mo$Di.1[ind],
                  mo$tdelay[ind])
names(ocf) <- c("field", "qo", "b", "Di", "td")

# Do same for MC results
ocfMC <- subset(wsim,
                subset = (fieldnum != 999),
                select = c("fieldnum", "qo.oil", "b.oil", "Di.oil", "td.oil"))
names(ocfMC) <- c("field", "qo", "b", "Di", "td")

# Increment field numbers in MC set by 1 to differentiate them from fit results
ocfMC$field <- ocfMC$field+1

# Row bind two data.frames together
m <- rbind(ocf, ocfMC)


# Data for gas ------------------------------------------------------------

# Get row index of fits for fields we're specifically fitting
ind <- which(mg$w_field_num == opt$field[1] |
               mg$w_field_num == opt$field[2] |
               mg$w_field_num == opt$field[3] |
               mg$w_field_num == opt$field[4] |
               mg$w_field_num == opt$field[5] |
               mg$w_field_num == opt$field[6] |
               mg$w_field_num == opt$field[7] |
               mg$w_field_num == opt$field[8] |
               mg$w_field_num == opt$field[9] |
               mg$w_field_num == opt$field[10])

# Make data.frame of field and DCA coefficient values, then rename
gcf <- data.frame(mg$w_field_num[ind],
                  mg$qo.1[ind],
                  mg$b.1[ind],
                  mg$Di.1[ind],
                  mg$tdelay[ind])
names(gcf) <- c("field", "qo", "b", "Di", "td")

# Do same for MC results
gcfMC <- subset(wsim,
                subset = (fieldnum != 999),
                select = c("fieldnum", "qo.gas", "b.gas", "Di.gas", "td.gas"))
names(gcfMC) <- c("field", "qo", "b", "Di", "td")

# Increment field numbers in MC set by 1 to differentiate them from fit results
gcfMC$field <- gcfMC$field+1

# Row bind two data.frames together
gm <- rbind(gcf, gcfMC)


# Plots -------------------------------------------------------------------

# # Start PDF printer
# pdf(file.path(path$plot, "DCA coefficients boxplot comparison - fit vs MC.pdf"),
#     width = 11, height = 8.5)

# Plots for oil -----------------------------------------------------------

# qo for oil
boxplot(qo ~ field,
        m,
        log = "y",
        range = 0,
        ylim = c(1, 1.1*max(m$qo)),
        xlab = "Field Number (by pairs - fit then MC)",
        ylab = "qo value for oil (bbl)",
        main = "Comparison of Oil qo values - fit vs. MC")

# b for oil
boxplot(b ~ field,
        m,
        range = 0,
        ylim = c(0, 1.1*max(m$b)),
        xlab = "Field Number (by pairs - fit then MC)",
        ylab = "b value for oil (dimensionless)",
        main = "Comparison of Oil b values - fit vs. MC")

# Di for oil
boxplot(Di ~ field,
        m,
        log = "y",
        range = 0,
        ylim = c(0.001, 1.1*max(m$Di)),
        xlab = "Field Number (by pairs - fit then MC)",
        ylab = "Di value for oil (dimensionless)",
        main = "Comparison of Oil Di values - fit vs. MC")

# td for oil
boxplot(td ~ field,
        m,
        range = 0,
        ylim = c(0, 30),
        xlab = "Field Number (by pairs - fit then MC)",
        ylab = "Time delay value for oil production (months)",
        main = "Comparison of Oil Production Time Delay Values - fit vs. MC")


# Plots for gas -----------------------------------------------------------

# # qo for gas
# boxplot(qo ~ field,
#         gm,
#         log = "y",
#         range = 0,
#         ylim = c(1, 1.1*max(gm$qo)),
#         xlab = "Field Number (by pairs - fit then MC)",
#         ylab = "qo value for gas (MCF)",
#         main = "Comparison of Gas qo values - fit vs. MC")
# 
# # b for gas
# boxplot(b ~ field,
#         gm,
#         range = 0,
#         ylim = c(0, 1.1*max(gm$b)),
#         xlab = "Field Number (by pairs - fit then MC)",
#         ylab = "b value for gas (dimensionless)",
#         main = "Comparison of Gas b values - fit vs. MC")
# 
# # Di for gas
# boxplot(Di ~ field,
#         gm,
#         log = "y",
#         range = 0,
#         ylim = c(0.001, 1.1*max(gm$Di)),
#         xlab = "Field Number (by pairs - fit then MC)",
#         ylab = "Di value for gas (dimensionless)",
#         main = "Comparison of Gas Di values - fit vs. MC")
# 
# # td for oil
# boxplot(td ~ field,
#         gm,
#         range = 0,
#         ylim = c(0, 30),
#         xlab = "Field Number (by pairs - fit then MC)",
#         ylab = "Time delay value for gas production (months)",
#         main = "Comparison of Gas Production Time Delay Values - fit vs. MC")

# dev.off()
