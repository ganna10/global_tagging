library(ncdf)
library(chron)
library(gridExtra)

Merge_data <- function(x,y){
  
  if(!is.null(x)){
    merge(x,y,by="date",all.x=TRUE)
  }
}

source("/Volumes/Seagate/Doc_IASS/BC_analysis/script//Get_Boxplot.R")

nc <- open.ncdf("/Volumes/Seagate/Doc_IASS/BC_analysis/data/BAERLIN2014_Senate_BLUME_observations.nc")
time <- get.var.ncdf(nc,"time")
#Change into Greg.Calendar
tim.vals <- chron(time, origin=c(month = 12, day = 31, year = 2013))
dates    <- as.POSIXlt(tim.vals, "UTC")
dat.time  <-  as.POSIXlt(strftime(dates,format='%Y-%m-%d %H:%M:%S'))

date.vals <-strftime(dat.time,"%Y-%m-%d")

namvar    <- names(nc$var)
nvar   <- vector("list", length(namvar))

for(i in 1:length(namvar)){
  
  nvar[[i]]    <- get.var.ncdf(nc,namvar[i])
  
}

names(nvar) <- namvar
tab.dat <- data.frame(nvar,"dates"=dates)
#Put into a data.frame along with the date:

#Hourly means:
hr.means <- aggregate(tab.dat[,1:15], 
                      list(hour = cut(tab.dat$date, breaks="hour")), 
                      mean, na.rm = TRUE)

#Maximum values:
max.vals <- aggregate(hr.means[,2:16], 
                       list(date = cut(as.POSIXct(hr.means$hour), breaks="day")), 
                       max, na.rm = TRUE)

#Filter and clean Infitive to NA
max.vals <- do.call(data.frame,lapply(max.vals, function(x) replace(x, is.infinite(x),NA)))

#Daily means:
day.means <- aggregate(hr.means[,2:16], 
                      list(date = cut(as.POSIXct(hr.means$hour), breaks="day")), 
                      mean, na.rm = TRUE)


no2.max.day <- aggregate(hr.means[,6], 
                      list(date = cut(as.POSIXct(hr.means$hour), breaks="day")), 
                      max, na.rm = TRUE)

day.means$date <- as.Date(day.means$date)

#For Ozone compute also M8Oh cols=9,10

ma <- function(x,n=8){filter(x,rep(1/n,n), sides=2)}
MA8O_MC042 <- ma(hr.means[,9],8)
MA8O_MWO88 <- ma(hr.means[,10],8)
MA8O_MC042 <- data.frame("MA8O_MC042"=as.numeric(MA8O_MC042),"date"=as.POSIXct(hr.means$hour))
MA8O_MWO88 <- data.frame("MA8O_MWO88"=as.numeric(MA8O_MWO88),"date"=as.POSIXct(hr.means$hour))

#Maximum
Max_MC042 <- aggregate(MA8O_MC042[,1], 
                       list(date = cut(as.POSIXct(MA8O_MC042$date), breaks="day")), 
                       max, na.rm = TRUE)

names(Max_MC042) <-c("date","MA8O_MC042")
#Max_MC042 <- Max_MC042[1:101,]  #think about a better way
Max_MWO88 <- aggregate(MA8O_MWO88[,1], 
                       list(date = cut(as.POSIXct(MA8O_MWO88$date), breaks="day")), 
                       max, na.rm = TRUE)
names(Max_MWO88) <-c("date","MA8O_MWO88")
f <- is.infinite(Max_MWO88$MA8O_MWO88)
Max_MWO88$MA8O_MWO88[f]<-NA

###################
# OPEN Meteo file
###################

#1st of June 0 am until 1st of September 2014 0 am#
ncmet <- open.ncdf("/Volumes//Seagate/Doc_IASS/BC_analysis/data/BAERLIN2014_FUBerlin_radiation_observations.nc")
timmet <- get.var.ncdf(ncmet,"time")
timmet.vals <- chron(timmet, origin=c(month = 12, day = 31, year = 2013))
met.dates    <- as.POSIXlt(timmet.vals, "UTC")
met.time  <-  as.POSIXlt(strftime(met.dates,format='%Y-%m-%d %H:%M:%S'))

#Open variables
nammet    <- names(ncmet$var)
metvar   <- vector("list", length(nammet))

for(i in 1:length(nammet)){
  
  metvar[[i]]    <- get.var.ncdf(ncmet,nammet[i])
  
}
names(metvar) <- nammet
tabmet <- data.frame(metvar,"dates"=met.dates)
#Put into a data.frame along with the date:

#Hourly means:
hr.means.met <- aggregate(tabmet[,1:5], 
                      list(hour = cut(tabmet$date, breaks="hour")), 
                      mean, na.rm = TRUE)
#Daily means:
day.means.met <- aggregate(hr.means.met[,2:6], 
                       list(date = cut(as.POSIXct(hr.means.met$hour), breaks="day")), 
                       mean, na.rm = TRUE)

tmax.day <- aggregate(hr.means.met[,2], 
                      list(date = cut(as.POSIXct(hr.means.met$hour), breaks="day")), 
                      max, na.rm = TRUE)

metdata <- day.means.met

metdata$tmax <- tmax.day[[2]]

###########
#Anomalies
###########
means <- (colMeans(metdata[-1],na.rm=TRUE))
dfmet    <-metdata[-1]
dfmet    <- scale(dfmet, center=TRUE, scale=FALSE)

#Open CWT classification
load("/Volumes//Seagate/Doc_IASS/BC_analysis/cwt/CWT_days.Rda")
d <-seq(as.Date("2014-05-01"), as.Date("2014-08-31"), by = "day")
d <- data.frame("date"=d)
start_day <- "2014-06-01" ##all analysis from 1 june (meteorology **)
f_start <- lapply(f_total,function(x) x[!(x$date <=start_day),])

f0 <- lapply(f_total,AddDate,d)
#Starting on 23 May
f <- lapply(f0, function(x) x[23:123,])

#***Merge types and pollutants

Dat.tot <- day.means
Dat.tot$NO2Maxday  <- no2.max.day$x
Dat.tot$Max_MC042 <- Max_MC042$MA8O_MC042
Dat.tot$Max_MWO88  <- Max_MWO88$MA8O_MWO88
#
period <- (Dat.tot$date>"2014-05-31")&(Dat.tot$date<"2014-09-01")

pollutant <- Dat.tot[period,]
start2 <- "2014-05-31"
dd <- d>start2
cwtJJA <- lapply(f0,function(x,y) x[y,],dd)
numtype  <- seq(1:11)
datamerged <- lapply(f_start,Merge_data,pollutant)

names.cwt<-as.list(names(cwtJJA))
cwtfq <- lapply(cwtJJA,function(x) x[[2]])
cwt.f <- sapply(names(cwtfq), function(id) {ifelse(cwtfq[[id]]!=0, id, 0)}, simplify=FALSE)
cwt.num <- sapply(numtype, function(id) {ifelse(cwtfq[[id]]!=0, id, 0)}, simplify=FALSE)
CWTs <- array()
CWTnum <- array()
for (i in 1:length(cwt.f)) {
  tmp <- cwt.f[[i]]
  nmp <- cwt.num[[i]]
  CWTs <- ifelse(tmp != 0, tmp, CWTs)
  CWTnum <- ifelse(nmp != 0, nmp, CWTnum)
}
meteodat <- metdata[-93,]

#Select 4 pollutant
sel.poll <- subset(pollutant,select=c("date","Max_MC042","PM10_MC042","NO2_MC042","NO2Maxday"))
dataf <- data.frame(sel.poll,CWTs,CWTnum)
datoz <- subset(pollutant,select=c("date","Max_MC042"))
datoz <- data.frame(datoz,CWTs)

new.data <- datoz[ which( datoz$Max_MC042 > 60) , ]
#only ozone
dataf2 <- data.frame(meteodat[-1],CWTs,sel.poll[2])

############
#Regression
###########
#Chose only Tmax
dat.reg <- dataf2[-1]
predictors <- names(dataf2[-which(names(dataf2)=="Max_MC042")])
dCWT       <- model.matrix(~CWTs, data=dataf2)[,-1]  #dummies for CWT
dataf2     <- Filter(is.numeric,dataf2)
dataf2     <- cbind(dataf2,dCWT) 

form  <- (paste("Max_MC042~",paste(predictors,collapse='+'),sep=""))
mod <- lm(as.formula(form),data=dataf2)
mod0  <- stepAIC(mod)

##4 pollutant selected: Max8O3, NO,NO2 and PM10
#Create a new data frame with pollutant+meteo+

df.merged <-  do.call(rbind,datamerged)
namdf <- names(df.merged)
max8o3 <- subset(df.merged, select=c("date", "n","Max_MC042"))
no2  <- subset(df.merged, select=c("date", "n","NO2_MC042"))
no   <-  subset(df.merged, select=c("date", "n","NO_MC042"))
pm10 <-  subset(df.merged, select=c("date", "n","PM10_MC042"))


##########################





pl <-list()
listp<-list(max8o3,pm10,no2,no)
nampl <- lapply(listp,function(x) names(x[3]))
laby <- as.list(c("O3MDA8","PM10","NO2","NO"))
for(i in 1:length(listp)){
  
  
  pl[[i]]<- Get_Boxplot(listp[[i]],nampl[[i]],laby[[i]])
  
}


do.call(grid.arrange,pl)




