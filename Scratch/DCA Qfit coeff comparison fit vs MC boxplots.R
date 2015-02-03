# Load fit results --------------------------------------------------------
load(file.path(path$data, "DCA_fits_v2.rda"))

# Only fitted wells
mo <- mo[which(mo$Qfit.1 == 1),]
mg <- mg[which(mg$Qfit.1 == 1),]


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
                  mo$Cp.1[ind],
                  mo$c1.1[ind])
names(ocf) <- c("field", "Cp", "c1")

# # Do same for MC results
# ocfMC <- subset(wsim,
#                 subset = (fieldnum != 999),
#                 select = c("fieldnum", "Cp.oil", "c1.oil"))
# names(ocfMC) <- c("field", "Cp", "c1")
# 
# # Increment field numbers in MC set by 1 to differentiate them from fit results
# ocfMC$field <- ocfMC$field+1
# 
# # Row bind two data.frames together
# m <- rbind(ocf, ocfMC)

m <- ocf


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
                  mg$Cp.1[ind],
                  mg$c1.1[ind])
names(gcf) <- c("field", "Cp", "c1")

# # Do same for MC results
# gcfMC <- subset(wsim,
#                 subset = (fieldnum != 999),
#                 select = c("fieldnum", "Cp.gas", "c1.gas"))
# names(gcfMC) <- c("field", "Cp", "c1")
# 
# # Increment field numbers in MC set by 1 to differentiate them from fit results
# gcfMC$field <- gcfMC$field+1
# 
# # Row bind two data.frames together
# gm <- rbind(gcf, gcfMC)

gm <- gcf


# Plots -------------------------------------------------------------------

# Start PDF printer
pdf(file.path(path$plot, "DCA Qfit coefficients boxplot.pdf"))

# Plots for oil -----------------------------------------------------------

# Cp for oil
boxplot(Cp ~ field,
        m,
        log = "y",
        range = 0,
        ylim = c(1, 1.1*max(m$Cp)),
        xlab = "Field Number (by pairs - fit then MC)",
        ylab = "Cp value for oil (bbl/month^0.5)",
        main = "Comparison of Oil Cp values - fit vs. MC")

# c1 for oil
boxplot(c1 ~ field,
        m,
        range = 0,
        ylim = c(0, 1.1*max(m$c1)),
        xlab = "Field Number (by pairs - fit then MC)",
        ylab = "c1 value for oil (bbl)",
        main = "Comparison of Oil c1 values - fit vs. MC")


# Plots for gas -----------------------------------------------------------

# qo for gas
boxplot(Cp ~ field,
        gm,
        log = "y",
        range = 0,
        ylim = c(1, 1.1*max(gm$Cp)),
        xlab = "Field Number (by pairs - fit then MC)",
        ylab = "Cp value for gas (MCF/month^0.5)",
        main = "Comparison of Gas Cp values - fit vs. MC")

# c1 for gas
boxplot(c1 ~ field,
        gm,
        range = 0,
        ylim = c(0, 1.1*max(gm$c1)),
        xlab = "Field Number (by pairs - fit then MC)",
        ylab = "c1 value for gas (MCF)",
        main = "Comparison of Gas c1 values - fit vs. MC")

dev.off()
