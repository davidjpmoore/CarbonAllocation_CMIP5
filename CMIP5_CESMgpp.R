###########################################################################
####### RCMIP5 for Dave######################################
#########################################################
########################################################################

#clumsily constructed by Sarah Ivory, 4/14/15
#Edited: Dave Moore

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



######CESM1-BGC##############################################################################  
model='CESM1-BGC'
num=6

assign(paste('model',num,sep=''),model)

#telling RCMIP5 where your files are and check metadata
mypath <- paste("./recal/",model,'/',run, sep='')
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
#   print(j)
  if (j==1){
    var=dat
  }else{
    var=stack(var,dat)
  }
}

#plot a couple of layers to make the sure the data looks ok
plot(var[[30]])
plot(var[[1100]])
plot(var[[13]])

assign(paste(var1,num,sep=''),var);rm(var) #makes a raster stack called gpp1


#now load in some latlon data and extract time series
sp.dat <- read.table("./sites/C_alloc_Sites20150704.txt")
#               latlon.RData")  #this should be formatted as just 2 cols, lon lat

#e.g. Harvard is 42.3744 71.1169
# lat / long for all our sites is here: https://docs.google.com/spreadsheets/d/1fugoqh-CeTNXCvB2jhWwIzZmCxg2-Rt2GxMPn1tP6J4/edit#gid=0


ts=extract(paste(var1,num,sep=''),sp.dat)
# 
# unable to find an inherited method for function ‘extract’ for signature ‘"character", "data.frame"’
#Possible that the file format of the lat/long data is off 
#data appears to be ocean only

save(list=(paste(var1,num,sep='')),ts, file = paste(mypath,'/',var1,'.RData',sep=''))  #saves gpp1 and ts

rm(list=setdiff(ls(), "var1"))
