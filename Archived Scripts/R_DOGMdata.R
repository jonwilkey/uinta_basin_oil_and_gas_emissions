require(RPostgreSQL)
driver = dbDriver("PostgreSQL")
connection = dbConnect(driver,user="postgres",db="DOGM",password="XXXXX")


## to print Jon files do 
#write.csv(cbind(referenceMonths,totProd/1e6),file="DOGMproductionMbblPerMonth.csv",row.names=FALSE)
#write.csv(cbind(referenceMonths,cumsum(totProd)/1e6),file="DOGMcumulativeProductionMbblPerMonth.csv",row.names=FALSE)


Wells = dbGetQuery(connection,"select api 
from histdata 
where api like '43047%'
      and well_type='OW'
      and spud_dry between '2002-01-01' AND '2011-06-01'") 
length(Wells$api)
# tra tutti i wells che hanno inziato la produzione dopo il 2002; filtro i wells che hanno anche il drilling starting after 2002 !!!!!

res<-data.frame()
for (ww in Wells$api) {
	qq<- paste("select api, td_md,spud_dry,compl_date,first_prod,well_type,wellstatus,directiona, trunc(compl_date-spud_dry) AS TotalDays, trunc(first_prod - spud_dry) AS productiondelay, trunc(first_prod - compl_date) AS completiondelay from histdata where api='",ww,"' and td_md!=0 and wellstatus = 'P' and directiona is NULL and spud_dry > '2002-01-01'  order by spud_dry",sep="")
	dd<- dbGetQuery(connection,qq)
	res<-rbind(res,dd)
	}
length(res$api)

res$spud_dry<-as.POSIXct(res$spud_dry)    # da stringa a data vera

dir1 <- "NEWstuff_afterLAPtoHOlmans/Proposals/2010/Utah/Working/Analysis/"

#### DURATION OF DRILLING PHASE

## Drilling phase duration = (completion - spud)


posTime  <- which(res$totaldays > 0)  # check that the duration is positive
negTime  <- which(res$totaldays < 0)  # all wells have positive Time
drillingTime <- res$totaldays[posTime]  ; length(drillingTime)

x <- seq(0,length(drillingTime),by=10)
#pdf(file = paste(dir1,"DrillingDuration.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
hist(drillingTime,breaks=50,xlim=c(0,1600),ylim=c(0,length(drillingTime)),xlab="Time SpudDry to completion [Days]",main="Oil Wells with spud dry > 2002")
legend(500,300,"Median = 43 days",bty="n")
legend(500,250,"Mean = 84 days",bty="n")     # non considerarla perche' ho dei bias; dei pozzi ci impiegano troppo
#dev.off()
median(drillingTime) ; mean(drillingTime) ; var(drillingTime)

drillingTime  <- res$totaldays[posTime]
depthDrillPos <- res$td_md[posTime]

#pdf(file = paste(dir1,"DrillingDurationVSDepth.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(depthDrillPos, drillingTime,ylab="Drilling duration [days]",xlab="Depth [feet]",main="Oil Wells with spud dry > 2002")

lm(formula = drillingTime ~ depthDrillPos) ->regLinTimeDepth
#Coefficients:
# (Intercept)  depthDrillPos  
#     -3.99079        0.01335 

lines(depthDrillPos, -3.99079 +0.01371 * depthDrillPos ,col=2)
legend(10000,1250, c("Linear Regression"), col = c(2),lty = c(1), merge = TRUE, bg='white',bty="n")
text(14000,1100,"y = -4 + 0.013 * x")
#dev.off()


# filtering data 
sd(drillingTime)
upBoundary <- mean(drillingTime) + sd(drillingTime)
filt <- drillingTime < upBoundary
drillingTimeFiltered <- drillingTime[filt]
depthWellFiltered <- depthDrillPos[filt]

#pdf(file = paste(dir1,"DrillingDurationVSDepth_filtered.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")

plot(depthWellFiltered, drillingTimeFiltered,ylab="Drilling duration [days]",xlab="Depth [feet]",main="Oil Wells with spud dry > 2002",ylim=c(0,500))
lm(formula = drillingTimeFiltered ~ depthWellFiltered) -> regLinTimeDepthFiltered
#lines(depthWellFiltered,-27.6491 + 0.0126 * depthWellFiltered,col=3)
#  -26.87181            0.01244  
abline(regLinTimeDepthFiltered,col=3)
legend(2000,400, c("Linear Regression  y = -27 + 0.012 * x"), col = c(3),lty = c(1), merge = TRUE, bg='white',bty="n")
text(6000,350,"Median = 42")
#dev.off()

median(drillingTimeFiltered)   # 42


## faccio il bin della profondita e calcolo la mediana del tempo in ogni bin
sortidx<-order(depthWellFiltered)
sdrillingT<-drillingTime[sortidx]
sdepthDrillPos<-depthWellFiltered[sortidx]
## creo array per data binning
bins<-sort(rep(c(1:20),length=length(sdrillingT)))
medTime<-tapply(sdrillingT,bins,median)
meddepth<-tapply(sdepthDrillPos,bins,median)
lines(meddepth,medTime,col="red")
quartz()
plot(meddepth,medTime,col="red",type="b")



## DEPTH
#pdf(file = paste(dir1,"DrillingStartVSDepth.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(res$spud_dry,res$td_md,type="p",xlab="Drilling Start Time",ylab="Depth [feet]",main="Oil Wells with spud dry > 2002")
#dev.off()

#pdf(file = paste(dir1,"Depth_hist.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
hist(res$td_md,breaks=50,ylim=c(0,200),xlab="Depth [feet]",main="Oil Wells with spud dry > 2002")
#dev.off()

### Average monthly depth calcualtion 
res$spud_dry<-as.POSIXct(res$spud_dry)
neworder<-order(res$spud_dry)
res<-res[neworder,]
wyears<-format(res$spud_dry,"%Y")
wmonths<-format(res$spud_dry,"%m")
levels<-paste(wyears,wmonths,sep="")

monthlymean<-tapply(res$td_md,levels,mean)
monthlyvar<-tapply(res$td_md,levels,var)

# to plot it
levelsMIO<-paste(wyears,"-",wmonths,sep="")
tapply(res$td_md,levelsMIO,mean) -> monthlymean
names(tapply(res$td_md,levelsMIO,mean)) -> pp
paste(pp,"-01",sep="") -> ppD
as.Date(ppD) -> XPlot
#pdf(file = paste(dir1,"Depth_avMonth.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(XPlot, monthlymean,xlab="Time [Years]",ylab="Montly Average Depth [feet]",type="b",main="Oil Wells spud dry after 2002") 
#dev.off()
length(monthlymean)

## quarter average (average per  quadrimestre)
qlev<-quarters(res$spud_dry)
quarterslevels<-paste(wyears,"-",qlev,sep="")
length(unique(quarterslevels))
quarterlymean<-tapply(res$td_md,quarterslevels,mean)
quarterlyvar<-tapply(res$td_md,quarterslevels,var)
timeStr<-gsub("Q1","01",quarterslevels)
timeStr<-gsub("Q2","04",timeStr)
timeStr<-gsub("Q3","07",timeStr)
timeStr<-gsub("Q4","10",timeStr)
timeStr<-paste(timeStr,"-01",sep="")
quarterDate<-as.Date(timeStr)
#pdf(file = paste(dir1,"Depth_avMonthQuart.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(unique(quarterDate),quarterlymean,xlab="Time [Years]",ylab="Average Depth [feet]",type="b",main="Oil Wells spud dry after 2002",ylim=c(5000,max(monthlymean)),col=2,pch=19)
lines(XPlot, monthlymean,col=1,type="b")
legend(XPlot[10], 11500, c("Quarterly Average Depth", "Montly Mean Depth"), col = c(2,1),lty = c(1, 1), merge = TRUE, bg='white',pch = c(19, 1))
#dev.off()

##NON SONO SICURA CHE LA X SIA CORRETTA NEL PLOTTARLA : MONTH AND WEEK ??? SEMBRA OK NEL PLOT


#Conversion quarterDate in weeks from startDate to be read in AnyLogic (converto quarterDate in settimane from strartDate per AnyLogic)
yyear <- as.numeric(format(quarterDate,"%Y"))
yyear <- yyear - yyear[1]
wweak <- yyear * 52 + as.numeric(format(quarterDate,"%U"))

write.csv(cbind(unique(wweak), quarterlymean ),file="depth_mean.csv",row.names=FALSE,col.names=FALSE)




### SCHEDULE: NUMBER OF WELLS DRILLED PER WEEKS

## schedule calcualtion for AnyLogic model: number of wells (OW) that are drilled (spud_dry) by month starting from 2002
# mi calcolo la schedule per il modello: numero di wells (OW) che sono drilled (spud_dry) al mese partendo dal 2002
# ???? quanti di quelli che sono stati drilled prima del 2002 contribuiscono alla produczione totale (quella che poi confronteremo con il nostro risultato); devo avere un'idea
sort(as.Date(res$spud_dry)) -> schedu
tapply(res$spud_dry,res$spud_dry,length) -> scheduCount
scheduleDay <- unique(schedu)

#pdf(file=paste(dir1,"NumberWellsSpuddedPerDay.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(scheduleDay, scheduCount,xlab="Years",ylab="Number of Wells drilled (spud dry) per day",main="Oil Wells spud dry after 2002",type="b")    
#dev.off() 
write.csv(cbind((1 + scheduleDay - scheduleDay[1]),scheduCount),file="schedule_wellsDrilledPerDay.csv",row.names=FALSE,col.names=FALSE) 
 
## Counts Wells by week (per settimana)
neworder<-order(as.Date(res$spud_dry))
res<-res[neworder,]
weeklevels<-format(res$spud_dry,"%Y-%U")
#pdf(file = paste(dir1,"shedule.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(tapply(weeklevels,weeklevels,length),type="b",xlab="Time [weeks]",ylab="Number of Wells weekly spudded",main="Oil Wells spud dry after 2002")
legend(10,6,"Model Input",bty="n")
#dev.off()


tapply(weeklevels,weeklevels,length) -> weeklyCounttt
settimana <- round(1 + (scheduleDay - scheduleDay[1])/7,digits = 0) 
uniqSettimana <- unique(settimana)
length(settimana) ; length(uniqSettimana) ; length(weeklevels) ; length(weeklyCounttt)
plot(uniqSettimana[1:(length(uniqSettimana)-1)]+1, weeklyCounttt,xlab="Time [Weeks]",ylab="Drilled Wells Counts Per Weeks [wells/weeks]",main="Oil Wells spud dry after 2002",type="b")
write.csv(cbind(uniqSettimana[1:(length(uniqSettimana)-1)]+1, weeklyCounttt),file="schedule_wellsDrilledPerWeeks.csv",sep="",row.names=FALSE,col.names=FALSE)
#THAT IS WHAT WE NEED FOR THE MODEL 

## Counts Wells average depth per mese
monthlevels<-format(res$spud_dry,"%Y-%m")
#pdf(file = paste(dir1,"shedule_Month.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(tapply(monthlevels,monthlevels,length),type="b",xlab="Time [Months]",ylab="Number of Wells montly spudded",main="Oil Wells spud dry after 2002") ### the same Olga found
#dev.off()





## Production Curves: 
start = as.Date("2002-01-01")
pres<-c()
compModel <- c()
noRes<-c()
wN<-c()
maxMonths<-1000
##pdf(file = paste(dir1,"Prodution_allOil.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")

#for (ww in Wells$api) {
	for (ww in res$api) {
# cat(ww,"\n")
	tmpProd<-array(0,maxMonths)
	# tmpProd<-array(NA,maxMonths)
	query=paste("select rpt_period, oil_prod from proddata where api ='",ww,"' order by rpt_period",sep="")
	resDb <- dbGetQuery(connection,query)
	if (nrow(resDb)==0) {
		noRes<-c(noRes,ww)
	} else {
		wN<-c(wN,ww)
		## normalizing time in weeks
		resTime<-as.Date(resDb[,1])
		tmin<-min(resTime)
		monthTime<-round((resTime-tmin)/30.41)+1     # when you do a difference btween dates the results is in days 
		tmpProd[monthTime]<-resDb[,2]
		pres<-cbind(pres,tmpProd)
		compModel <- cbind(resDb[,1],resDb[,2])
		if (ww==4304715417){
			#plot(resDb$rpt_period-resDb$rpt_period[1],resDb$oil_prod,type="l",xlim=c(0,3000),ylim=c(0,15000),xlab=("Time from production start (DAYS !!!!!)"),ylab=("Well Monthly Production [bbl/month]"))
			plot(resDb[,2],type="l",xlim=c(0,100),ylim=c(0,15000),xlab=("Time from production start [Months]"),ylab="Production Curve [bbl/month]",main="Oil Wells spud dry after 2002")
			#lines(resDb[,2],col=2)
		} else {
			#lines(resDb$rpt_period-resDb$rpt_period[1],resDb$oil_prod,col=1)
			lines(resDb[,2],col=1)
			#plot(resDb[,2],col=1,type="l",xlim=c(0,200),ylim=c(0,15000))	
abline(h=0,lty=2)

		}
	}
}

lines(apply(pres,1,mean,na.rm = TRUE),col=2,pch=5)

lines(apply(pres,1,mean),col=2,pch=5)
legend(200,12500,c("Production Curves","Median"),col = c(1,2),lty = c(1,1), merge = TRUE, bg='white',bty="n")

#dev.off()

compModelSumProd <- apply(compModel,1,sum)
meanProd<-apply(pres,1, mean,na.rm = TRUE)     		   # mean of all the production curves 
normProd<-meanProd/(sum(meanProd))        # mean normalized so its area is 1

weeksInAMonth <- 4.348
which(normProd == 0) -> indexProdZero     # months from the start of the production
indexProdZero[1] -> wellLivingTimeInMonths
normProd[1:indexProdZero[1]]/weeksInAMonth -> MeanNormProdPerWeek
c(1:indexProdZero[1])*weeksInAMonth -> timeInWeeks
timeInWeeks[length(timeInWeeks)] -> wellLivingTimeInWeeks
weeksInAMonth <- 4.348

write.csv(wellLivingTimeInMonths,file="wellLivingTimeInMonths.csv",row.names=FALSE,col.names=FALSE) 
write.csv(wellLivingTimeInWeeks,file="wellLivingTimeInWeeks.csv",row.names=FALSE,col.names=FALSE) 

write.csv(cbind(c(1:indexProdZero[1]), normProd[1:indexProdZero[1]]),file="mean_productionCurve_Montly.csv",row.names=FALSE) ## e' di fatto quell anormalizzata
write.csv(cbind(c(1:indexProdZero[1])*weeksInAMonth, normProd[1:indexProdZero[1]]/weeksInAMonth),file="mean_productionCurve_Weekly.csv",row.names=FALSE) 

#pdf(file = paste(dir1,"Prod_normAv_Month.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(c(1:indexProdZero[1]), normProd[1:indexProdZero[1]],xlab="Time from the start of production [Months]",ylab="Normalized Mean Montly Production [bbl/month]",main="Oil Wells spud dry after 2002",type="l",pch=20)
abline(h=0,lty=2)
#dev.off()

#pdf(file = paste(dir1,"Prod_normAv_Week.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(c(1:indexProdZero[1])*weeksInAMonth, normProd[1:indexProdZero[1]]/weeksInAMonth,xlab="Time from the start of production [Week]",ylab="Normalized Mean Weekly Production [bbl/week]",main="Oil Wells spud dry after 2002",type="b",pch=20)
abline(h=0,lty=2) ; text(600,0.015,"Model Input")
#dev.off()

#pdf(file = paste(dir1,"Prod_Av_Month.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(c(1:indexProdZero[1]), meanProd[1:indexProdZero[1]],xlab="Time from the start of production [Months]",ylab="Mean Well Montly Production  [bbl/month]",main="Oil Wells spud dry after 2002",type="l",pch=20)
abline(h=0,lty=2)
#dev.off()



--------------------------------------   Total production (sum) of each well as a function of the depth ----

wellTotProd<-apply(pres,2,sum)   		 #total production (sum) for each wells

intApi<-intersect(wN,res$api)
idxProd<- wN %in% intApi
wellToProd<-wellTotProd[idxProd]
wN<-wN[idxProd]
idxDepth<- res$api %in% intApi
res<-res[idxDepth,]
ordProd<-order(wN)
ordDepth<-order(res$api)
wellToProd<-wellToProd[ordProd]
wN<-wN[ordProd]
res <-res[ordDepth,]
y<-wellToProd
x<-res$td_md
plot(x,y)
lm(formula = y ~ x)  -> regLinProdTot   ### linear regression
		# Coefficients:
		#(Intercept)            x  
		# -11049.591        4.714  
#pdf(file = paste(dir1,"Production_SumVSDepth.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
	plot(x,y,xlab="Depth [feet]",ylab="Total Production [bbl]",main="Oil Wells with spud dry > 2002") 
	abline(regLinProdTot,col=2)
	legend(500,210000, c("Linear Regression  y = -11050 + 5 * x"), col = c(3),lty = c(1), merge = TRUE, bg='white',bty="n")
#dev.off()

# production curve as a function of the time and the depth: 
#c(1:indexProdZero[1])*weeksInAMonth * (-11049.591 + 4.714*res$td_md)      prodCurveFuncDepthAndTime
#prodcuction curve of the first drilled wells:

i<-1
plot(c(1:indexProdZero)*weeksInAMonth,normProd/weeksInAMonth * (-11049.591 + 4.714* quarterlymean[1]))
plot(unique(quarterDate),normProd/weeksInAMonth * (-11049.591 + 4.714* quarterlymean))




## aggregate production

start = as.Date("2002-01-01")
stopDate=as.Date("2010-01-01")
wellsProduction<-c()
compModel <- c()
noRes<-c()
wN<-c()
referenceMonths<-seq(start,stopDate,by="month")
maxMonths=length(referenceMonths)
#for (ww in Wells$api) {
for (ww in res$api) {
	# cat(ww,"\n")
	tmpProd<-array(0,maxMonths)
	query=paste("select rpt_period, oil_prod from proddata where api ='",ww,"' and rpt_period>'",start,"' and rpt_period<'",stopDate,"' order by rpt_period",sep="")
	resDb <- dbGetQuery(connection,query)
	if (nrow(resDb)==0) {
		noRes<-c(noRes,ww)
	} else {
		wN<-c(wN,ww)
		## normalizing time in weeks
		resTime<-as.Date(resDb[,1])
		resTimeInRef<- resTime[resTime %in% referenceMonths]
		refMonthIdx<-which(referenceMonths %in% resTimeInRef)
		tmpProd[refMonthIdx]<-resDb[,2]
		wellsProduction<-cbind(wellsProduction,tmpProd)
		compModel <- cbind(resDb[,1],resDb[,2])
	}
}

totProd<-apply(wellsProduction,1,sum)


#pdf(file = paste(dir1,"Production_TotalPerMonth.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
	plot(referenceMonths,totProd/1e6,type="b",col=1,ylab="Total Monthly Production [Mbbl/month]",main="Oil Wells with spud dry > 2002",xlab="Time")
#dev.off()

#pdf(file = paste(dir1,"Production_TotalCumulative.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
	plot(referenceMonths,cumsum(totProd)/1e6,type="l",col=1,ylab="Cumulative Production [Mbbl]",main="Oil Wells with spud dry > 2002",xlab="Time")
#dev.off()

wellsProduction!=0 -> prodnonZero
apply(prodnonZero,1,sum) -> countWellsInProduction
plot(countWellsInProduction)

wapi<-paste("'",Wells$api,"'",sep="")
ff<-function(...) {paste(...,sep=", ")}
instr<-do.call("ff",as.list(wapi))
query<-paste("select rpt_period, sum(oil_prod) from proddata where api in (",instr,") and rpt_period>'",start,"' and rpt_period<'",stopDate,"' group by rpt_period",sep="")
resDb <- dbGetQuery(connection,query)
resDb[,1]<-as.Date(resDb[,1])
idx<-order(resDb[,1])
resDb<-resDb[idx,]
lines(resDb[,1],resDb[,2],col=3)



##### compare simulation 

oo <- paste(dir1,"Output.csv",sep="")
read.csv(oo,sep=",",header=FALSE,skip=4)-> o1   #corretteo
oo <- paste(dir1,"Output_originale.csv",sep="")
read.csv(oo,sep=",",header=FALSE,skip=4)-> o
plot(o$V1,o$V2)

o$V1*7 -> settimInGiorni
startDate1 <- as.Date("2003-01-14")
startDate1 + o$V1*7 -> giorni
plot(giorni,o$V2/1e6)
lines()

#pdf(file = paste(dir1,"Production_TotalPerMonthSimulation.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(referenceMonths,totProd/1e6,type="b",col=1,ylab="Total Monthly Production [Mbbl/month]",main="Oil Wells with spud dry > 2002",xlab="Time")
lines(giorni,o$V2/1e6,col=2)
legend(giorni[20],0.15,c("Data","Simulation"),col=c(1,2),lty=c(1,0), merge = TRUE, bg='white',pch = c(19, 1))
#dev.off()

#pdf(file = paste(dir1,"Production_TotalCumulativeSimulation.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(referenceMonths,cumsum(totProd)/1e6,type="l",col=1,ylab="Cumulative Production [Mbbl]",main="Oil Wells with spud dry > 2002",xlab="Time")
lines(giorni,o$V3,col=2)
legend(giorni[20],6,c("Data","Simulation"),col=c(1,2),lty=c(1,0), merge = TRUE, bg='white',pch = c(19, 1))
#dev.off()

#pdf(file = paste(dir1,"Production_TotalPerMonthSimulationMano.pdf",sep=""), onefile = TRUE, family = "Helvetica",title = "R Graphics Output")
plot(referenceMonths,totProd/1e6,type="b",col=1,ylab="Total Monthly Production [Mbbl/month]",main="Oil Wells with spud dry > 2002",xlab="Time")
lines(giorni,o1$V2/1e6,col=2)
legend(giorni[20],0.15,c("Data","Simulation"),col=c(1,2),lty=c(1,0), merge = TRUE, bg='white',pch = c(19, 1))
#dev.off()