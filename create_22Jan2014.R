#-------------------------------------------------------------------------------
# Options
#-------------------------------------------------------------------------------
options(width=200)
options(stringsAsFactors=FALSE)
options(drop=FALSE)

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------
library(plyr)
library(zoo)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# raw data
rdpath <- "/media/Data_Share/data/research/raw_data"
# prepared data
pdpath <- "/media/Data_Share/data/research/prepared_data"
# functions 
fin <- "/home/michael/r_code"
# lookup tables
lin <- "/home/michael/lutables"

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------
flst <- file.path(fin,c("clean_names.R"
                       )
                  )
for (f in flst) source(f)

#-------------------------------------------------------------------------------
# Lookup tables 
#-------------------------------------------------------------------------------
names_pattern_replacement <- read.csv(file.path(lin,"names_pattern_replacement.csv"),
                                      header=TRUE,
                                      colClasses="character"
                                      )
names_pattern_replacement <- as.matrix(names_pattern_replacement) 

#===============================================================================
# Create proddata
#===============================================================================
proddata_colclasses <- c("Date",      # rpt_period
                         "character", # acct_num
                         "character", # alt_addres
                         "character", # api
                         "character", # prod_zone
                         "character", # entity
                         "character", # wellstatus
                         "character", # well_type
                         "numeric",   # days_prod
                         "numeric",   # oil_prod
                         "numeric",   # gas_prod
                         "numeric",   # water_prod
                         "character", # date_recd
                         "character"  # amend_flag
                         )

proddata <- read.csv(file.path(rdpath,"proddata.txt"),
                     header=TRUE,
                     comment.char = "",
                     colClasses = proddata_colclasses,
                     )

new_names <- clean_names(names(proddata), table=names_pattern_replacement)
# prepend 'p_' for 'proddata' to new names
new_names <- gsub(pattern="^", replacement = "p_", x = new_names)
# assign these new names to proddata
names(proddata) <- new_names
# remove unwanted fields p_alt_addres, p_date_recd, and p_amend_flag
omits <- c("p_alt_addres", "p_date_recd", "p_amend_flag")
if (all((omits %in% names(proddata)))) {
  proddata <- proddata[,setdiff(names(proddata), omits)]  
} else stop("Check column names")
# order by api, rpt_period
proddata <- proddata[order(proddata[,"p_api"], proddata[,"p_rpt_period"]),]

#-------------------------------------------------------------------------------
# Combining production for wells producing in more than one zone
#-------------------------------------------------------------------------------
# Extract all and only records for wells with duplicate api-rpt_period
# (these are wells with production in multiple zones).
dd <- duplicated(proddata[,c("p_api","p_rpt_period")])
aa <- unique(proddata[dd,"p_api"])
# Multi zone.
mz <- subset(proddata, subset = (p_api %in% aa))
# Single zone.
sz <- subset(proddata,
             subset = (p_api %in% setdiff(unique(proddata[,"p_api"]), aa))
             )
# Combine production records from different production zones.
# Get fields which will be summed (oil, gas, and water production) or the
# maximum taken (days_prod). 
mz1 <- mz[,c("p_rpt_period","p_api","p_oil_prod","p_gas_prod","p_water_prod",
             "p_days_prod"
             )
          ]
# Do the combining. 
mz1 <- ddply(mz1,
             c("p_api", "p_rpt_period"),
             summarize,
             p_oil_prod = sum(p_oil_prod),
             p_gas_prod = sum(p_gas_prod),
             p_water_prod = sum(p_water_prod),
             p_days_prod = max(p_days_prod)
             )
# Get fields which are being held constant for each distinct api-rpt_period
# value, then remove duplicate rows.
mz2 <- mz[,c("p_rpt_period","p_api","p_acct_num","p_prod_zone","p_entity",
             "p_wellstatus","p_well_type"
             )
          ] 
ww <- which(duplicated(mz2[,c("p_rpt_period","p_api")]))
mz2 <- mz2[-ww,]
# Now merge mz1 and mz2 as mz. 
mz <- merge(mz1,
            mz2,
            by.x = c("p_api","p_rpt_period"),
            by.y = c("p_api","p_rpt_period"),
            sort = FALSE
            )
# Now bind sz and mz as proddata.
proddata <- rbind(sz,mz)
# Reorder. 
proddata <- proddata[order(proddata[,"p_api"], proddata[,"p_rpt_period"]),]

#-------------------------------------------------------------------------------
# welldata
#-------------------------------------------------------------------------------
welldata_colclasses <- c("character", # api
                         "character", # well_name
                         "character", # acct_num
                         "character", # alt_addres
                         "numeric",   # field_num
                         "character", # elevation
                         "character", # locat_foot
                         "character", # utm_surf_n
                         "character", # utm_surf_e
                         "character", # utm_bhl_n
                         "character", # utm_bhl_e
                         "character", # qtr_qtr
                         "character", # section
                         "character", # township
                         "character", # range
                         "character", # meridian
                         "character", # county
                         "character", # dir_horiz
                         "character", # conf_flag
                         "character", # conf_date
                         "character", # lease_num
                         "character", # lease_type
                         "Date",      # abndondate
                         "character", # wellstatus
                         "character", # well_type
                         "numeric",   # totcum_oil
                         "numeric",   # totcum_gas                         
                         "numeric",   # totcum_wtr
                         "character", # ind_tribe
                         "character", # multi_lats
                         "character", # cbmethflag
                         "character", # surfowntyp
                         "character", # bond_num
                         "character", # bond_type
                         "character", # ca_number
                         "character", # field_type
                         "character", # unit_name
                         "character", # lat_surf
                         "character"  # long_surf
                         )

welldata <- read.csv(file.path(rdpath,"welldata.txt"),
                     header=TRUE,
                     comment.char = "",
                     colClasses = welldata_colclasses
                    )
# Clean names.
new_names <- clean_names(names(welldata), table=names_pattern_replacement)
# Prepend 'w_' for 'welldata' to cleaned names.
new_names <- gsub(pattern="^", replacement = "w_", x = new_names)
# Assign these new names to welldata.
names(welldata) <- new_names
# Remove unwanted fields.
omits <- c("w_well_name", "w_alt_addres", "w_elevation", "w_locat_foot",
           "w_utm_bhl_n", "w_utm_bhl_e"
           )
if (all((omits %in% names(welldata)))) {
  welldata <- welldata[,setdiff(names(welldata), omits)]
} else stop("Check column names")


#-------------------------------------------------------------------------------
# histdata 
#-------------------------------------------------------------------------------
histdata_colclasses <- c("character", # api 
                         "Date",      # apd_aprovd
                         "character", # work_type
                         "Date",      # spud_dry
                         "Date",      # spud_rotry                         
                         "character", # prod_zone
                         "Date",      # compl_date
                         "Date",      # intent_rec  
                         "Date",      # work_compl
                         "numeric",   # td_md
                         "numeric",   # td_tvd
                         "numeric",   # pbtd_md
                         "numeric",   # pbtd_tvd
                         "character", # wellstatus
                         "character", # well_type
                         "Date",      # first_prod
                         "character", # testmethod
                         "character", # choke
                         "character", # tubng_prs
                         "character", # casng_prs
                         "numeric",   # oil_24hr
                         "numeric",   # gas_24hr
                         "numeric",   # water_24hr
                         "character", # dir_survey
                         "character", # cored
                         "character", # dst
                         "character", # comp_type
                         "character", # directiona
                         "numeric",   # lat_count
                         "character", # rec_seq
                         "character"  # conf_flag
                         )

histdata <- read.csv(file.path(rdpath,"histdata.txt"),
                     header=TRUE,
                     comment.char = "",
                     colClasses = histdata_colclasses,
                     na.strings="NULL"
                    )
# Clean names.
new_names <- clean_names(names(histdata), table=names_pattern_replacement)
# Prepend 'h_' for 'histdata' to cleaned names.
new_names <- gsub(pattern="^", replacement = "h_", x = new_names)
# Assign these new names to histdata.
names(histdata) <- new_names
omits <- c("h_td_tvd","h_pbtd_tvd","h_rec_seq")
if (all((omits %in% names(histdata)))) {
  histdata <- histdata[,setdiff(names(histdata), omits)]
} else stop("Check column names")

#-------------------------------------------------------------------------------
# Save
#-------------------------------------------------------------------------------
save(file=file.path(pdpath, "proddata.rda"), list=c("proddata"))
save(file=file.path(pdpath, "welldata.rda"), list=c("welldata"))
save(file=file.path(pdpath, "histdata.rda"), list=c("histdata"))





