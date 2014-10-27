ps <- subset(p,
            subset = (time != 0 &
                      (p$h_well_type == "OW" |
                       p$h_well_type == "GW" |
                       p$h_well_type == "D")),
            select = c("p_api",
                       "p_oil_prod",
                       "p_gas_prod",
                       "p_water_prod",
                       "time",
                       "h_well_type",
                       "w_field_num",
                       "w_totcum_oil",
                       "w_totcum_gas"))

well <- sqldf("select distinct p_api, w_field_num, w_totcum_oil, w_totcum_gas
              from ps
              order by w_totcum_oil DESC, w_totcum_gas DESC")

linecolor <- gray(0:9/9/2)

temp <- subset(p,
               subset = (p_api == well$p_api[1]),
               select = c("time", "p_oil_prod"))
with(p,
     plot(time, p_oil_prod,
                type = "l",
                col = linecolor[1],
                xlab = "Time (months)",
                ylab = "Oil Production (bbl)",
                main = "blah")
     for (j in 2:length(linecolor)) {
       lines(time)
     }
     )