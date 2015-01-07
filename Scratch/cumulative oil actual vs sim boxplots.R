ind <- which(mo$fit.1 == 1 & (mo$w_field_num == opt$field[1] |
               mo$w_field_num == opt$field[2] |
               mo$w_field_num == opt$field[3] |
               mo$w_field_num == opt$field[4] |
               mo$w_field_num == opt$field[5] |
               mo$w_field_num == opt$field[6] |
               mo$w_field_num == opt$field[7] |
               mo$w_field_num == opt$field[8] |
               mo$w_field_num == opt$field[9] |
               mo$w_field_num == opt$field[10]))

boxplot(w_totcum_oil ~ w_field_num, mo[ind,], range = 0, log = "y")

m <- data.frame(wsim$fieldnum, rowSums(osim))
names(m) <- c("field", "totoil")
m <- m[-which(m$field == 999),]
m$field <- m$field+1

mmo <- mo[ind,2:3]
names(mmo) <- c("field", "totoil")
mmo <- rbind(mmo, m)

pdf(file.path(path$plot, "Cumulative oil comparison.pdf"))
boxplot(totoil ~ field,
        mmo,
        range = 0,
        log = "y",
        ylim = c(1, 1.1*max(m[,2])),
        xlab = "Field Number (listed by pairs - actual then simulated)",
        ylab = "Cumulative Oil Production (bbl) - Log Scale",
        main = "Comparison of Cumulative Oil Production - Actual vs. Simulated")
dev.off()
