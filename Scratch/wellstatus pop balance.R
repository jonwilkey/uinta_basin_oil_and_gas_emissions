# Load histdata
load(file.path(path$data, "histdata.rda"))

# Get list of unique api #s and what counties they are located in
test <- sqldf("select distinct p_api, w_county from p")

# Merge with histdata
test <- merge(x = histdata, y = test, by.x = "h_api", by.y = "p_api", all.x = T)

# Drop any wells not in Uintah or Duchesne county (i.e. w_county == NA) and
# select only desired columns
test <- test[!is.na(test$w_county),
             c("h_api", "h_work_type", "h_compl_date", "h_work_compl",
               "h_well_type", "h_first_prod", "h_wellstatus")]

# Get population of wells with completion dates and work type == DRILLING
test1 <- subset(test, subset = (h_work_type == "DRILL" & h_compl_date > as.Date("1984-01-01")))

# Get subset of wells which were converted
test2 <- subset(test, subset = (h_work_type == "CONVERT" & !is.na(h_work_compl)))


# possible options
# drill   || h_compl_date # 19,665
# reenter || h_compl_date #     88
# convert || h_work_compl #  1,476 (92% of the time convert to a WI well)
# recomp  || h_work_compl #    821
# reperf  || h_work_compl #  1,950
# plug    || h_work_compl #  1,157 (84% of the time permanent)
# deepen  || h_compl_date #    160