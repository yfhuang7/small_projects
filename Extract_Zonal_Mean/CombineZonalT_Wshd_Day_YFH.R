### 
#
# Created by Hannah Clilverd, modified by Yu-Fen Huang (2017/7/20)
#

#clean the working space
#list (ls) 
rm(list = ls(all = TRUE))
ls(all = TRUE)

#This code combines all the zonal stats that ArcGIS generated
#Make sure you run Python codes first

#Make sure the package "foreign" is installed
#This allows us to read in dbf files
library(foreign)  #turn on package

#Input Location, set working directory
#Grab rainfall zonal stats output tables from python code:
setwd("G:/My Drive/Lab_Tsang/Project_SedimentFlux/SWAT")

#location:
Tmax<-"./Zonal_AlaWai_Tmax/"
Tmin<-"./Zonal_AlaWai_Tmin/"

#SWAT subbasin data
subbasin<-read.csv("./subbasin_AlaWai.csv")


is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

monthdays<-function(Year,month){
  if(month==2){
    leapyear<-is.leapyear(Year)
    if(leapyear){
      monthDays<-29
    }else{
      monthDays<-28
    }
    
  }else if(month==1|month==3|month==5|month==7|month==8|month==10|month==12){
    monthDays<-31
  }else{
    monthDays<-30
  }
  return(monthDays)
}


i=0  #keep track of rows

# How many watershed you have?
no_watershed<-nrow(subbasin)

#New Data Frame: 36 columns each watershed, 1 extra column for yr_month name
Tmax_mean_ws<-data.frame(matrix(NA,ncol=no_watershed+1))
colnames(Tmax_mean_ws)[1]<-"yr_mo_da"

#Loop through all months & years, skip annual files
for (yr in 1999:2014){
  for (mo in 1:12) {
    monthday<-monthdays(yr,mo)
    for (da in 1:monthday) {
    
    i=i+1  #keep track of rows (should be up to 30 to match data frame)
    
    makedate<-format(as.Date(paste(yr,mo,da,sep="/")),"%Y_%m_%d")
    #print(paste("Tmax_All_",makedate,"_zonal.dbf",sep=""))
    #open all dbf files, grab "MEAN" column
    zonal<-read.dbf(paste(Tmax,"Tmax_oa_",makedate,"_zonal.dbf",sep=""))
    Tmax_mean_ws[i,1]=makedate  #put year_month in first column
    
    for (wshd in 1:no_watershed) {
      Tmax_mean_ws[i,wshd+1]=zonal$MEAN[wshd]  #copy mean val for each watershed into columns of row i
    }
    }
  
  }
}

#Organized the format
date<-as.Date(Tmax_mean_ws$yr_mo_da, "%Y_%m_%d")
Tmax_mean_ws$yr_mo_da<-date


#Put the subbasin number
Tmax_mean_ws_new<-Tmax_mean_ws
siteno<-as.character(zonal$Value)
colnames(Tmax_mean_ws_new)<-c("Date",paste("AlaWai_subbasin_",siteno,sep = ""))
# USGS_wshd_Oahu<-read.csv("../USGS_gage_watershed_Oahu.txt")
# colnames(Tmax_mean_ws_new)<-c("Date",USGS_wshd_Oahu$SITENO[order(USGS_wshd_Oahu$gridcode)])
# USGS_wshd_Oahu$SITENAME<-as.character(USGS_wshd_Oahu$SITENAME)

#Write output table:
write.table(Tmax_mean_ws_new, file = "Tmax_Daily_MeanbySWATsubWshd_AlaWai.csv", sep = ",", col.names = T, row.names = F)

###Tmax output for SWAT (specific watershed)
date_start<-19990101
date_end<-20141231
datechain<-seq(as.Date(as.character(date_start),"%Y%m%d"),as.Date(as.character(date_end),"%Y%m%d"),1)
Tmax_consecutive<-as.data.frame(datechain)
Tmax_consecutive<-merge(Tmax_consecutive,Tmax_mean_ws_new,by.x="datechain",by.y="Date",all.x=TRUE)

#Check na data
i_NA<-which(is.na(Tmax_consecutive$Preciptation_mm))
i_9999<-which(Tmax_consecutive$Preciptation_mm=="-9999")
if(length(i_NA)!=0){Tmax_consecutive$Preciptation_mm[i_NA]<--99.0}
if(length(i_9999)!=0){Tmax_consecutive$Preciptation_mm[i_9999]<--99.0}

###Tmin output for SWAT ------------------------

i=0  #keep track of rows

#New Data Frame: 36 columns each watershed, 1 extra column for yr_month name
Tmin_mean_ws<-data.frame(matrix(NA,ncol=no_watershed+1))
colnames(Tmin_mean_ws)[1]<-"yr_mo_da"

#Loop through all months & years, skip annual files
for (yr in 1999:2014){
  for (mo in 1:12) {
    monthday<-monthdays(yr,mo)
    for (da in 1:monthday) {
      
      i=i+1  #keep track of rows (should be up to 30 to match data frame)
      
      makedate<-format(as.Date(paste(yr,mo,da,sep="/")),"%Y_%m_%d")
      #print(paste("Tmin_All_",makedate,"_zonal.dbf",sep=""))
      #open all dbf files, grab "MEAN" column
      zonal<-read.dbf(paste(Tmin,"Tmin_oa_",makedate,"_zonal.dbf",sep=""))
      Tmin_mean_ws[i,1]=makedate  #put year_month in first column
      
      for (wshd in 1:no_watershed) {
        Tmin_mean_ws[i,wshd+1]=zonal$MEAN[wshd]  #copy mean val for each watershed into columns of row i
      }
    }
    
  }
}

#Organized the format
date<-as.Date(Tmin_mean_ws$yr_mo_da, "%Y_%m_%d")
Tmin_mean_ws$yr_mo_da<-date


#Put the subbasin number
Tmin_mean_ws_new<-Tmin_mean_ws
siteno<-as.character(zonal$Value)
colnames(Tmin_mean_ws_new)<-c("Date",paste("AlaWai_subbasin_",siteno,sep = ""))
# USGS_wshd_Oahu<-read.csv("../USGS_gage_watershed_Oahu.txt")
# colnames(Tmin_mean_ws_new)<-c("Date",USGS_wshd_Oahu$SITENO[order(USGS_wshd_Oahu$gridcode)])
# USGS_wshd_Oahu$SITENAME<-as.character(USGS_wshd_Oahu$SITENAME)

#Write output table:
write.table(Tmin_mean_ws_new, file = "Tmin_Daily_MeanbySWATsubWshd_AlaWai.csv", sep = ",", col.names = T, row.names = F)

###Tmin output for SWAT (specific watershed)
date_start<-19990101
date_end<-20141231
datechain<-seq(as.Date(as.character(date_start),"%Y%m%d"),as.Date(as.character(date_end),"%Y%m%d"),1)
Tmin_consecutive<-as.data.frame(datechain)
Tmin_consecutive<-merge(Tmin_consecutive,Tmin_mean_ws_new,by.x="datechain",by.y="Date",all.x=TRUE)

#Check na data
i_NA<-which(is.na(Tmin_consecutive$Preciptation_mm))
i_9999<-which(Tmin_consecutive$Preciptation_mm=="-9999")
if(length(i_NA)!=0){Tmin_consecutive$Preciptation_mm[i_NA]<--99.0}
if(length(i_9999)!=0){Tmin_consecutive$Preciptation_mm[i_9999]<--99.0}



### Output SWAT file ----------

#One file for each watershed
sitename<-c()
for (i in 1:length(siteno)){
  sitename[i]<-paste("AlaWai_subbasin_",siteno[i],sep="")
#  sitename[i]<-USGS_wshd_Oahu$SITENAME[which(USGS_wshd_Oahu$SITENO==siteno[i])]
  output<-data.frame(Tmax_consecutive[,i+1],Tmin_consecutive[,i+1])
  write.table(date_start,paste("./SWAT_Temp/SWAT_TempData_daily_AlaWai_",siteno[i],".txt",sep=""),row.names=FALSE,col.names=FALSE)
  write.table(output ,paste("./SWAT_Temp/SWAT_TempData_daily_AlaWai_",siteno[i],".txt",sep=""),
              row.names=FALSE,col.names=FALSE,append=TRUE,sep = ",")
}

# #Infor for site number (optional when you have sitename)
# write.table(data.frame(siteno=siteno,sitename=sitename),"./SiteName.txt",row.names = F,quote = F,sep = " >>> ")


### Write pcfork (with location) for SWAT (Data format: ID,NAME,LAT,LONG,ELEVATION)
subbasin_zonal<-merge(as.data.frame(siteno),subbasin, by.x="siteno", by.y="Subbasin")
tfork<-data.frame(ID=subbasin_zonal$siteno,NAME=paste("SWAT_TempData_daily_AlaWai_",subbasin_zonal$siteno,sep=""),
                    LAT=subbasin_zonal$Lat, LONG=subbasin_zonal$Long_,
                    ELEVATION=subbasin_zonal$Elev)
write.table(tfork,"./SWAT_Temp/tfork.txt",quote = F, sep = ",", row.names = F)


### Data check and analysis
png(filename = paste("./Figure/SWAT_AlaWaiSubbasin_Tmax_boxplot.png",sep = ""),
    width = 1600, height = 1200, res = 150)
par(mar=c(10, 5, 2, 2) + 0.1)
boxplot(Tmax_consecutive[,2:length(Tmax_consecutive)], 
        xlab = NA, ylab="Temperature (C)",las=2,
        main = "Tmax from 1999 to 2014 for Each Subbasin in AlaWai")
dev.off()


png(filename = paste("./Figure/SWAT_AlaWaiSubbasin_Tmin_boxplot.png",sep = ""),
    width = 1600, height = 1200, res = 150)
par(mar=c(10, 5, 2, 2) + 0.1)
boxplot(Tmin_consecutive[,2:length(Tmin_consecutive)], 
        xlab = NA, ylab="Temperature (C)",las=2,
        main = "Tmin from 1999 to 2014 for Each Subbasin in AlaWai")
dev.off()

