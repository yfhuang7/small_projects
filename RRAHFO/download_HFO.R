########### download_HFO.r ############
# 
# purpose
#   :Download RRAHFO rainfall daily rainfall summary for HI
#
# created by yfhuang 2017/3/17
# edited by YinPhan 2017/3/22 (Add Library, RCurl)
# edited by yfhuang 2017/3/25 (Add Log)
#######################################

# Clean up
rm(list = ls(all=T))
ls(all=T)

## Environment Setting (NEED MANUAL INPUT)--------
# Library (Please install if you don't have one)

# Working directory
setwd("C:/Users/Yu-Fen Huang/GoogleDrive_yfhuang/R_Script/R_workplace/RRAHFO")  

# Setting
start_date <- Sys.Date()-89
end_date <- Sys.Date()    # 90 days summary
idate <- seq.Date(start_date,end_date,by="1 d")
fdate <- format(idate, "%y%m%d")

# [function] download from RRAHFO
fn_download <- function(i){
  UrlSyntax<-paste("http://www.prh.noaa.gov/hnl/Products/RRAHFO/RRAHFO.",
                   fdate[i],"0800.txt",sep="")
  print(UrlSyntax)

  
  download.file(UrlSyntax,paste("RRAHFO",fdate[i],sep='_'),method="libcurl")
}

library(RCurl)
# run download
write(paste("### Download System Time:",Sys.Date(),"###"),
      "RRAHFO_LOG.txt",sep = "\n",append=T)
for(i in 1:90){
  
  UrlSyntax<-paste("http://www.prh.noaa.gov/hnl/Products/RRAHFO/RRAHFO.",
                   fdate[i],"0800.txt",sep="")
  print(UrlSyntax)
  if(url.exists(UrlSyntax)){
    download.file(UrlSyntax,paste("RRAHFO",fdate[i],sep='_'),method="libcurl")  
  }else
    {write(paste("WARNING: NOAA weather station doesn't have",fdate[i], "data"),
           paste("RRAHFO_LOG_",fdate[90],".txt"),sep = "\n",append=T)
     write(paste("WARNING: NOAA weather station doesn't have",fdate[i], "data"),
            "RRAHFO_LOG.txt",sep = "\n",append=T)
    }
}

