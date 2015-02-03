# Subset filtered production records
ps <- subset(p,
             subset = (time != 0 &
                         (p$h_well_type == "OW" |
                            p$h_well_type == "GW") &
                         p$p_days_prod >= opt$minDayProd),
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

# Load fit records
load(file.path(path$data, "DCA_fits_v2.rda"))

# Get API List of failed wells
apilist <- mg$p_api[which(mg$w_field_num == 630 & (mg$fail.1 == 1 | mg$fail.2 == 1))]

# Start pdf print
pdf(file.path(path$plot, "Bad well records - gas.pdf"))

# Plot each record
for (i in 1:length(apilist)) {
  
  # Get records just for that well
  ws <- subset(ps, subset = (p_api == apilist[i]), select = c("time", "p_gas_prod"))
  names(ws) <- c("time", "prod")
  
  # Main Plot
  plot(ws$time, ws$prod,
       main = paste("Gas Production from API #", apilist[i]),
       xlab = "Time Since First Production (months)",
       ylab = "Gas Production (MCF)")#,
       #col = "grey")
}

# Close pdf print
dev.off()

# To get records about particular well
t(mg[which(mg$p_api == 4304732427),c("w_totcum_gas","qo.1","b.1","Di.1","tdelay","qo.2","b.2","Di.2")])


