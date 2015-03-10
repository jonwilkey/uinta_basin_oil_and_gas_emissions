well <- sqldf("select distinct p_api, w_well_type, h_well_type, w_totcum_oil, w_totcum_gas, w_totcum_wtr, h_first_prod
              from p")
names(well) <- c("api", "wType", "hType", "oil", "gas", "water", "tDrill")
well <- subset(well, subset = (oil > 0))
well$r <- well$water/well$oil
test.ow <- subset(well, subset = c(hType == "OW"))
test.gw <- subset(well, subset = c(hType == "GW"))
