a <- subset(p, select = c("p_api", "w_well_type"))
a <- na.omit(a)
b <- unique(a$p_api)
e <- rep(x = 0, times = length(b))
for (i in 1:length(b)) {
  c <- a[which(a$p_api == b[i]),2]
  d <- unique(c$w_well_type)
  if (length(d) > 1) e[i] <- 1
}

a <- sqldf("select distinct p_api, w_well_type, h_well_type from p")
b <- unique(a[,1])
c <- NULL
for (i in 1:length(b)) {
  temp <- a[which(a$p_api == b[i]),]
  if (nrow(temp) > 1){
    c <- rbind(c,temp)
  }
}
c <- na.omit(c)
c <- c[-which(c[,3] == "NA"),]

d <- NULL
for (i in 1:length(b)) {
  temp <- c[which(c$p_api == b[i]),]
  if (nrow(temp) > 1) {
    d <- rbind(d,temp)
  }
}

e <- d[-which(d[,3] == "WI"),]
f <- NULL
for (i in 1:length(b)) {
  temp <- e[which(e$p_api == b[i]),]
  if (nrow(temp) > 1) {
    f <- rbind(f,temp)
  }
}

b <- subset(a, subset = (a[,3] == 718))
clipboard(summary(b[,2]))

b <- a[c(-which(a[,3] == 630),
         -which(a[,3] == 105),
         -which(a[,3] == 72),
         -which(a[,3] == 55),
         -which(a[,3] == 65),
         -which(a[,3] == 710),
         -which(a[,3] == 665),
         -which(a[,3] == 590),
         -which(a[,3] == 60),
         -which(a[,3] == 718)),]