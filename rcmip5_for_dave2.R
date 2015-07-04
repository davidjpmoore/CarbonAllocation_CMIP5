###########################################################################
####### RCMIP5 for Dave######################################
#########################################################
########################################################################

#clumsily constructed by Sarah Ivory, 4/14/15

#start with a clean slate
rm(list=ls(all=TRUE))

# #get the right packages
# install.packages('RCMIP5')
# install.packages('ncdf')
# install.packages('raster')
# install.packages('rgeos')
# install.packages('rgdal')
# install.packages('sp')

#load libraries
library(RCMIP5)
library(ncdf)
library(raster)
library(rgeos)
library(rgdal)



#which variable are you looking at (ex. cRoot, cVeg, cSoil, cLitter, gpp, lai)
var1=as.character('gpp')
run='historical'  #change to 'piControl' when using controls

######GFDL-ESM2M##########
model='GFDL-ESM2M'
num=1
assign(paste('model',num,sep=''),model)

#telling RCMIP5 where your files are and check metadata
mypath <- paste("/recal/",model,'/',run, sep='')
c5files <- getFileInfo(mypath)
co2files <- subset(c5files, variable==var1);co2filechk <- checkTimePeriod(co2files)

#load in file;if there are multiple files in the folder for that variable it automatically concatenates them
#I have it just loading 100 yrs of data (1200 monthly timesteps)
#also this command will generate a warning message, but its ok
s=round(co2filechk$endDate[[1]],digits=0)
cvar<- loadCMIP5(var1, model, run, path=mypath, verbose=T, yearRange=c(s-99,s))  ### gpp is in kg/m2/s; c Mass in kg/m2


#verify we did this ok
summary(cvar)

val=length(cvar$time)/12
indic=length(cvar$lat)*length(cvar$lon)

for (j in 1:1200){

mon1=((j-1)*indic)+1
mon2=indic*j

testval=cvar$val$value[mon1:mon2]
testlat=cvar$val$lat[mon1:mon2]
testlon=cvar$val$lon[mon1:mon2]

test=as.data.frame(cbind(testlon,testlat,testval));names(test)=c('lon','lat','value')
test <- test[order(test$lon, test$lat),] 
dat<-matrix(test$value,nrow=length(cvar$lat),ncol=length(cvar$lon));dat=dat[c(length(cvar$lat):1),] 
dat=raster(dat)
e <- extent(min(cvar$lon), max(cvar$lon), min(cvar$lat), max(cvar$lat))
extent(dat) <- e
print(j)
if (j==1){
var=dat
}else{
var=stack(var,dat)
}
}


assign(paste(var1,num,sep=''),var);rm(var) #makes a raster stack called gpp1


#now load in some latlon data and extract time series
sp.dat <- load("C:/Users/sivory/Documents/R/gbif_latlon/latlon.RData")  #this should be formatted as just 2 cols, lon lat
ts=extract(paste(var1,num,sep=''),sp.dat)

save(list=(paste(var1,num,sep='')),ts, file = paste(mypath,'/',var1,'.RData',sep=''))  #saves gpp1 and ts


rm(list=setdiff(ls(), "var1"))



######BNU-ESM######  NOTE: BNU-ESM GPP is wrong in some areas of the world!!!!!!!!###############################
model='BNU-ESM'
num=2
assign(paste('model',num,sep=''),model)

#telling RCMIP5 where your files are and check metadata
mypath <- paste("C:/Users/sivory/Documents/R/recal/",model,'/',run, sep='')
c5files <- getFileInfo(mypath)
co2files <- subset(c5files, variable==var1);co2filechk <- checkTimePeriod(co2files)

#load in file;if there are multiple files in the folder for that variable it automatically concatenates them
#I have it just loading 100 yrs of data (1200 monthly timesteps)
#also this command will generate a warning message, but its ok
s=round(co2filechk$endDate[[1]],digits=0)
cvar<- loadCMIP5(var1, model, run, path=mypath, verbose=T, yearRange=c(s-99,s))  ### gpp is in kg/m2/s; c Mass in kg/m2


#verify we did this ok
summary(cvar)

val=length(cvar$time)
indic=length(cvar$lat)*length(cvar$lon)

for (j in 1:val){
  
  mon1=((j-1)*indic)+1
  mon2=indic*j
  
  testval=cvar$val$value[mon1:mon2]
  testlat=cvar$val$lat[mon1:mon2]
  testlon=cvar$val$lon[mon1:mon2]
  
  test=as.data.frame(cbind(testlon,testlat,testval));names(test)=c('lon','lat','value')
  test <- test[order(test$lon, test$lat),] 
  dat<-matrix(test$value,nrow=length(cvar$lat),ncol=length(cvar$lon));dat=dat[c(length(cvar$lat):1),] 
  dat=raster(dat)
  e <- extent(min(cvar$lon), max(cvar$lon), min(cvar$lat), max(cvar$lat))
  extent(dat) <- e
  print(j)
  if (j==1){
    var=dat
  }else{
    var=stack(var,dat)
  }
}

#plot a couple of layers to make the sure the data looks ok
plot(var[[1]])
plot(var[[1100]])
plot(var[[13]])

assign(paste(var1,num,sep=''),var);rm(var) #makes a raster stack called gpp1


#now load in some latlon data and extract time series
sp.dat <- load("C:/Users/sivory/Documents/R/gbif_latlon/latlon.RData")  #this should be formatted as just 2 cols, lon lat
ts=extract(paste(var1,num,sep=''),sp.dat)

save(list=(paste(var1,num,sep='')),ts, file = paste(mypath,'/',var1,'.RData',sep=''))  #saves gpp1 and ts

rm(list=setdiff(ls(), "var1"))


######CanESM2######################################################################################  
model='CanESM2'
num=3
assign(paste('model',num,sep=''),model)

#telling RCMIP5 where your files are and check metadata
mypath <- paste("C:/Users/sivory/Documents/R/recal/",model,'/',run, sep='')
c5files <- getFileInfo(mypath)
co2files <- subset(c5files, variable==var1);co2filechk <- checkTimePeriod(co2files)

#load in file;if there are multiple files in the folder for that variable it automatically concatenates them
#I have it just loading 100 yrs of data (1200 monthly timesteps)
#also this command will generate a warning message, but its ok
s=round(co2filechk$endDate[[1]],digits=0)
cvar<- loadCMIP5(var1, model, run, path=mypath, verbose=T, yearRange=c(s-99,s))  ### gpp is in kg/m2/s; c Mass in kg/m2


#verify we did this ok
summary(cvar)

val=length(cvar$time)
indic=length(cvar$lat)*length(cvar$lon)

for (j in 1:val){
  
  mon1=((j-1)*indic)+1
  mon2=indic*j
  
  testval=cvar$val$value[mon1:mon2]
  testlat=cvar$val$lat[mon1:mon2]
  testlon=cvar$val$lon[mon1:mon2]
  
  test=as.data.frame(cbind(testlon,testlat,testval));names(test)=c('lon','lat','value')
  test <- test[order(test$lon, test$lat),] 
  dat<-matrix(test$value,nrow=length(cvar$lat),ncol=length(cvar$lon));dat=dat[c(length(cvar$lat):1),] 
  dat=raster(dat)
  e <- extent(min(cvar$lon), max(cvar$lon), min(cvar$lat), max(cvar$lat))
  extent(dat) <- e
  print(j)
  if (j==1){
    var=dat
  }else{
    var=stack(var,dat)
  }
}

#plot a couple of layers to make the sure the data looks ok
plot(var[[1]])
plot(var[[1100]])
plot(var[[13]])

assign(paste(var1,num,sep=''),var);rm(var) #makes a raster stack called gpp1


#now load in some latlon data and extract time series
sp.dat <- load("C:/Users/sivory/Documents/R/gbif_latlon/latlon.RData")  #this should be formatted as just 2 cols, lon lat
ts=extract(paste(var1,num,sep=''),sp.dat)

save(list=(paste(var1,num,sep='')),ts, file = paste(mypath,'/',var1,'.RData',sep=''))  #saves gpp1 and ts

rm(list=setdiff(ls(), "var1"))


######MIROC-ESM################################################################################  
model='MIROC-ESM'
num=4
assign(paste('model',num,sep=''),model)

#telling RCMIP5 where your files are and check metadata
mypath <- paste("C:/Users/sivory/Documents/R/recal/",model,'/',run, sep='')
c5files <- getFileInfo(mypath)
co2files <- subset(c5files, variable==var1);co2filechk <- checkTimePeriod(co2files)

#load in file;if there are multiple files in the folder for that variable it automatically concatenates them
#I have it just loading 100 yrs of data (1200 monthly timesteps)
#also this command will generate a warning message, but its ok
s=round(co2filechk$endDate[[1]],digits=0)
cvar<- loadCMIP5(var1, model, run, path=mypath, verbose=T, yearRange=c(s-99,s)) ### gpp is in kg/m2/s; c Mass in kg/m2


#verify we did this ok
summary(cvar)

val=length(cvar$time)
indic=length(cvar$lat)*length(cvar$lon)

for (j in 1:val){
  
  mon1=((j-1)*indic)+1
  mon2=indic*j
  
  testval=cvar$val$value[mon1:mon2]
  testlat=cvar$val$lat[mon1:mon2]
  testlon=cvar$val$lon[mon1:mon2]
  
  test=as.data.frame(cbind(testlon,testlat,testval));names(test)=c('lon','lat','value')
  test <- test[order(test$lon, test$lat),] 
  dat<-matrix(test$value,nrow=length(cvar$lat),ncol=length(cvar$lon));dat=dat[c(length(cvar$lat):1),] 
  dat=raster(dat)
  e <- extent(min(cvar$lon), max(cvar$lon), min(cvar$lat), max(cvar$lat))
  extent(dat) <- e
  print(j)
  if (j==1){
    var=dat
  }else{
    var=stack(var,dat)
  }
}

#plot a couple of layers to make the sure the data looks ok
plot(var[[1]])
plot(var[[1100]])
plot(var[[13]])

assign(paste(var1,num,sep=''),var);rm(var) #makes a raster stack called gpp1


#now load in some latlon data and extract time series
sp.dat <- load("C:/Users/sivory/Documents/R/gbif_latlon/latlon.RData")  #this should be formatted as just 2 cols, lon lat
ts=extract(paste(var1,num,sep=''),sp.dat)

save(list=(paste(var1,num,sep='')),ts, file = paste(mypath,'/',var1,'.RData',sep=''))  #saves gpp1 and ts

rm(list=setdiff(ls(), "var1"))

######NorESM1-ME#############################################################################  
model='NorESM1-ME'
num=5

assign(paste('model',num,sep=''),model)

#telling RCMIP5 where your files are and check metadata
mypath <- paste("C:/Users/sivory/Documents/R/recal/",model,'/',run, sep='')
c5files <- getFileInfo(mypath)
co2files <- subset(c5files, variable==var1);co2filechk <- checkTimePeriod(co2files)

#load in file;if there are multiple files in the folder for that variable it automatically concatenates them
#I have it just loading 100 yrs of data (1200 monthly timesteps)
#also this command will generate a warning message, but its ok
s=round(co2filechk$endDate[[1]],digits=0)
cvar<- loadCMIP5(var1, model, run, path=mypath, verbose=T, yearRange=c(s-99,s))  ### gpp is in kg/m2/s; c Mass in kg/m2


#verify we did this ok
summary(cvar)

val=length(cvar$time)
indic=length(cvar$lat)*length(cvar$lon)

for (j in 1:val){
  
  mon1=((j-1)*indic)+1
  mon2=indic*j
  
  testval=cvar$val$value[mon1:mon2]
  testlat=cvar$val$lat[mon1:mon2]
  testlon=cvar$val$lon[mon1:mon2]
  
  test=as.data.frame(cbind(testlon,testlat,testval));names(test)=c('lon','lat','value')
  test <- test[order(test$lon, test$lat),] 
  dat<-matrix(test$value,nrow=length(cvar$lat),ncol=length(cvar$lon));dat=dat[c(length(cvar$lat):1),] 
  dat=raster(dat)
  e <- extent(min(cvar$lon), max(cvar$lon), min(cvar$lat), max(cvar$lat))
  extent(dat) <- e
  print(j)
  if (j==1){
    var=dat
  }else{
    var=stack(var,dat)
  }
}

#plot a couple of layers to make the sure the data looks ok
plot(var[[1]])
plot(var[[1100]])
plot(var[[13]])

assign(paste(var1,num,sep=''),var);rm(var) #makes a raster stack called gpp1


#now load in some latlon data and extract time series
sp.dat <- load("C:/Users/sivory/Documents/R/gbif_latlon/latlon.RData")  #this should be formatted as just 2 cols, lon lat
ts=extract(paste(var1,num,sep=''),sp.dat)

save(list=(paste(var1,num,sep='')),ts, file = paste(mypath,'/',var1,'.RData',sep=''))  #saves gpp1 and ts

rm(list=setdiff(ls(), "var1"))


######CESM1-BGC##############################################################################  
model='CESM1-BGC'
num=6

assign(paste('model',num,sep=''),model)

#telling RCMIP5 where your files are and check metadata
mypath <- paste("C:/Users/sivory/Documents/R/recal/",model,'/',run, sep='')
c5files <- getFileInfo(mypath)
co2files <- subset(c5files, variable==var1);co2filechk <- checkTimePeriod(co2files)

#load in file;if there are multiple files in the folder for that variable it automatically concatenates them
#I have it just loading 100 yrs of data (1200 monthly timesteps)
#also this command will generate a warning message, but its ok
s=round(co2filechk$endDate[[1]],digits=0)
cvar<- loadCMIP5(var1, model, run, path=mypath, verbose=T, yearRange=c(s-99,s)) ### gpp is in kg/m2/s; c Mass in kg/m2


#verify we did this ok
summary(cvar)

val=length(cvar$time)
indic=length(cvar$lat)*length(cvar$lon)

for (j in 1:val){
  
  mon1=((j-1)*indic)+1
  mon2=indic*j
  
  testval=cvar$val$value[mon1:mon2]
  testlat=cvar$val$lat[mon1:mon2]
  testlon=cvar$val$lon[mon1:mon2]
  
  test=as.data.frame(cbind(testlon,testlat,testval));names(test)=c('lon','lat','value')
  test <- test[order(test$lon, test$lat),] 
  dat<-matrix(test$value,nrow=length(cvar$lat),ncol=length(cvar$lon));dat=dat[c(length(cvar$lat):1),] 
  dat=raster(dat)
  e <- extent(min(cvar$lon), max(cvar$lon), min(cvar$lat), max(cvar$lat))
  extent(dat) <- e
  print(j)
  if (j==1){
    var=dat
  }else{
    var=stack(var,dat)
  }
}

#plot a couple of layers to make the sure the data looks ok
plot(var[[1]])
plot(var[[1100]])
plot(var[[13]])

assign(paste(var1,num,sep=''),var);rm(var) #makes a raster stack called gpp1


#now load in some latlon data and extract time series
sp.dat <- load("C:/Users/sivory/Documents/R/gbif_latlon/latlon.RData")  #this should be formatted as just 2 cols, lon lat
ts=extract(paste(var1,num,sep=''),sp.dat)

save(list=(paste(var1,num,sep='')),ts, file = paste(mypath,'/',var1,'.RData',sep=''))  #saves gpp1 and ts

rm(list=setdiff(ls(), "var1"))

######HadGEM2-ES#######################################################################  
model='HadGEM2-ES'
num=7

assign(paste('model',num,sep=''),model)

#telling RCMIP5 where your files are and check metadata
mypath <- paste("C:/Users/sivory/Documents/R/recal/",model,'/',run, sep='')
c5files <- getFileInfo(mypath)
co2files <- subset(c5files, variable==var1);co2filechk <- checkTimePeriod(co2files)

#load in file;if there are multiple files in the folder for that variable it automatically concatenates them
#I have it just loading 100 yrs of data (1200 monthly timesteps)
#also this command will generate a warning message, but its ok
s=round(co2filechk$endDate[[1]],digits=0)
cvar<- loadCMIP5(var1, model, run, path=mypath, verbose=T, yearRange=c(s-99,s))  ### gpp is in kg/m2/s; c Mass in kg/m2


#verify we did this ok
summary(cvar)

val=length(cvar$time)
indic=length(cvar$lat)*length(cvar$lon)

for (j in 1:val){
  
  mon1=((j-1)*indic)+1
  mon2=indic*j
  
  testval=cvar$val$value[mon1:mon2]
  testlat=cvar$val$lat[mon1:mon2]
  testlon=cvar$val$lon[mon1:mon2]
  
  test=as.data.frame(cbind(testlon,testlat,testval));names(test)=c('lon','lat','value')
  test <- test[order(test$lon, test$lat),] 
  dat<-matrix(test$value,nrow=length(cvar$lat),ncol=length(cvar$lon));dat=dat[c(length(cvar$lat):1),] 
  dat=raster(dat)
  e <- extent(min(cvar$lon), max(cvar$lon), min(cvar$lat), max(cvar$lat))
  extent(dat) <- e
  print(j)
  if (j==1){
    var=dat
  }else{
    var=stack(var,dat)
  }
}

#plot a couple of layers to make the sure the data looks ok
plot(var[[1]])
plot(var[[1100]])
plot(var[[13]])

assign(paste(var1,num,sep=''),var);rm(var) #makes a raster stack called gpp1


#now load in some latlon data and extract time series
sp.dat <- load("C:/Users/sivory/Documents/R/gbif_latlon/latlon.RData")  #this should be formatted as just 2 cols, lon lat
ts=extract(paste(var1,num,sep=''),sp.dat)

save(list=(paste(var1,num,sep='')),ts, file = paste(mypath,'/',var1,'.RData',sep=''))  #saves gpp1 and ts

rm(list=setdiff(ls(), "var1"))
######IPSL-CM5A-LR##############################################################################  
model='IPSL-CM5A-LR'
num=8

assign(paste('model',num,sep=''),model)

#telling RCMIP5 where your files are and check metadata
mypath <- paste("C:/Users/sivory/Documents/R/recal/",model,'/',run, sep='')
c5files <- getFileInfo(mypath)
co2files <- subset(c5files, variable==var1);co2filechk <- checkTimePeriod(co2files)

#load in file;if there are multiple files in the folder for that variable it automatically concatenates them
#I have it just loading 100 yrs of data (1200 monthly timesteps)
#also this command will generate a warning message, but its ok
s=round(co2filechk$endDate[[1]],digits=0)
cvar<- loadCMIP5(var1, model, run, path=mypath, verbose=T, yearRange=c(s-99,s)) ### gpp is in kg/m2/s; c Mass in kg/m2


#verify we did this ok
summary(cvar)

val=length(cvar$time)
indic=length(cvar$lat)*length(cvar$lon)

for (j in 1:val){
  
  mon1=((j-1)*indic)+1
  mon2=indic*j
  
  testval=cvar$val$value[mon1:mon2]
  testlat=cvar$val$lat[mon1:mon2]
  testlon=cvar$val$lon[mon1:mon2]
  
  test=as.data.frame(cbind(testlon,testlat,testval));names(test)=c('lon','lat','value')
  test <- test[order(test$lon, test$lat),] 
  dat<-matrix(test$value,nrow=length(cvar$lat),ncol=length(cvar$lon));dat=dat[c(length(cvar$lat):1),] 
  dat=raster(dat)
  e <- extent(min(cvar$lon), max(cvar$lon), min(cvar$lat), max(cvar$lat))
  extent(dat) <- e
  print(j)
  if (j==1){
    var=dat
  }else{
    var=stack(var,dat)
  }
}

#plot a couple of layers to make the sure the data looks ok
plot(var[[1]])
plot(var[[1100]])
plot(var[[13]])

assign(paste(var1,num,sep=''),var);rm(var) #makes a raster stack called gpp1


#now load in some latlon data and extract time series
sp.dat <- load("C:/Users/sivory/Documents/R/gbif_latlon/latlon.RData")  #this should be formatted as just 2 cols, lon lat
ts=extract(paste(var1,num,sep=''),sp.dat)

save(list=(paste(var1,num,sep='')),ts, file = paste(mypath,'/',var1,'.RData',sep=''))  #saves gpp1 and ts

rm(list=setdiff(ls(), "var1"))