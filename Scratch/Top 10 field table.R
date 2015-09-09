library(xtable)

# Load field names table
load(file.path(path$data, "fieldnames.rda"))

# Rename for brevity
fn <- fieldnames; remove(fieldnames)

# change names
names(fn) <- c("field", "name")

# Get well counts by type and by field
owc <- sqldf("select w_field_num, count(distinct p_api)
            from p
            where h_well_type = 'OW' group by w_field_num")

# Change names
names(owc) <- c("field", "owc")

gwc <- sqldf("select w_field_num, count(distinct p_api)
            from p
             where h_well_type = 'GW' group by w_field_num")

# Change names
names(gwc) <- c("field", "gwc")

# Merge well counts
wc <- merge(owc, gwc, all = TRUE)

# Get oil and gas production fractions by field
pf <- sqldf("select w_field_num, sum(p_oil_prod), sum(p_gas_prod)
             from p
             group by w_field_num")

# Change names
names(pf) <- c("field", "poil", "pgas")

# Normalize
pf$poil <- pf$poil/sum(pf$poil)
pf$pgas <- pf$pgas/sum(pf$pgas)

# Merge into single table
result <- merge(wc, pf, all = TRUE)
result <- merge(x = result, y = fn, all.x = TRUE)

# Replace NAs
for (i in 2:5) {
  ind <- which(is.na(result[,i]))
  result[ind,i] <- 0
}

# Format
result <- with(result, data.frame(name = name,
                                  number = field,
                                  ow = owc,
                                  gw = gwc,
                                  tw = owc+gwc,
                                  op = round(poil*100,2),
                                  gp = round(pgas*100,2)))


# Sort
result <- result[with(result, order(-tw, -op, -gp)),]

# Catchall calc
temp <- with(result[11:nrow(result),], c(sum(ow), sum(gw), sum(tw), sum(op), sum(gp)))
result <- rbind(result[1:10,],
                data.frame(name = "All Other Fields",
                           number = "---",
                           ow = temp[1],
                           gw = temp[2],
                           tw = temp[3],
                           op = temp[4],
                           gp = temp[5]))

# Make LaTeX table
frt <- xtable(x = result,
              caption = "Top 10 Largest DOGM Fields by Well Counts and Production",
              label = "tab:topfield",
              digits = c(0, 0, 0, 0, 0, 0, 2, 2))
print(frt, include.rownames = FALSE)
