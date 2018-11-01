### Discharge_salt_injection.R ##################
#
#   Purpose: calculate discharge by salt injection
#
#   Input
#     : sp.conductance sheets from YSI, please name "Salt_Injection_YYYYMMDD.csv"
#     : record for stnadard curve
#
#   Created by yfhuang 20170922
#################################################

#clean the working space
rm(list = ls(all = TRUE))
ls(all = TRUE)

### Environment Setting --------
setwd("C:/Users/Yu-Fen Huang/GoogleDrive_yfhuang/R_Script/R_workplace/Salt_Injection")  

### Information Input (need to re-fill everytime)----------
# genral
Date<-"20180516"         # YYYYMMDD
Sensor<-"KUBO"
file.data<-paste("./data/Salt_Injection_",Sensor,"_",Date,".csv",sep = "")
InjectionTime<-"10:12"   # Salt Injection time
LogInterval<-5           # YSI logging interval (s)
Weather<-"cloudy but rain ealier"
discharge_FlowMeter_cms<-0.022  
station<-"Lyon"


# standard curve
StandardCurveTime<-"10:05"

Salt_mg<-c(0,120,240,0)             # Salt amount (mg) for upstream, jar1, jar2, downstream
StreamWater_L<-c(1,0.6,0.6,NA)      # Streamwater (L) for the jars, and upstream != 0, downstream = NA for caluclation
Specific_Conductance_us.cm<-c(109.6,499.8,925,109.5)  # Sp. Conductance (us/cm)
Temperature_C<-c(21.,21.5,21.5,21)   # Temperautre (degree C)
Concentration_mg.L<-Salt_mg/StreamWater_L   # Concentration (mg/L)

StandardCurve<-data.frame(Salt_mg=Salt_mg, StreamWater_L=StreamWater_L, 
                          Concentration_mg.L=Concentration_mg.L, 
                          Specific_Conductance_us.cm=Specific_Conductance_us.cm,
                          Temperature_C=Temperature_C)
 


### Caluclate Standard Curve
# linear fitting
fit<-lm(Concentration_mg.L~Specific_Conductance_us.cm, StandardCurve)

# plot
png(paste("./result/SC",Date,".png",sep = ""),
    width=1000, height=800, units="px",pointsize=26,bg="white")
plot(StandardCurve$Specific_Conductance_us.cm, StandardCurve$Concentration_mg.L,
     main = paste("Standard Curve for Salt Injection at ", InjectionTime, " on ", Date, sep = ""),
     xlab = expression(paste("Sp. Cond (",mu, "s/cm)",sep = "")),
     ylab = "Concentration (mg/L)")
abline(fit$coefficients, lty=6, col="red")
cf <- round(coef(fit), 2)
text(700,50, labels = paste("Concentration =",cf[1], ifelse(sign(cf[2])==1, " + ", " - "), 
                            abs(cf[2]), " Sp.Cond ", sep = " "))
dev.off()


### Read YSI data --------------

ysi<-read.csv(file.data)
ysi$CFK<-(ysi$Specific.Conductance..uS.cm.-ysi$Specific.Conductance..uS.cm.[1])*fit$coefficients[2]

discharge_SaltInjection_cms<-1000/(sum(ysi$CFK)*LogInterval)


### Write the discharge sheets ------------ 
## Maybe can try to use 'append'?
End_Time<-substr(as.character(ysi$Timestamp[nrow(ysi)]),12,16)
output<-data.frame(Date=as.Date(Date,"%Y%m%d"), Station=station,
                   Start_Time=InjectionTime, End_Time=End_Time,
                   FlowMeter_Time=InjectionTime,
                   FlowMeter_Discharge_cms=discharge_FlowMeter_cms,
                   SaltInjection_Discharge_cms=discharge_SaltInjection_cms,
                   Weather=Weather)

data<-read.table("./result/Salt_Injection_Discharge.txt", header = T, sep = "")
data[,c(1:5,8)]<-lapply(data[,c(1:5,8)], as.character)
output[,c(1:5,8)]<-lapply(output[,c(1:5,8)], as.character)
if(nrow(merge(output,data))==0){
  data[nrow(data)+1,]<-output[1,]
} 
data<-data[order(data$Date),]   # Sort data

#Copy to Rmarkdown and plot the accumulated data
#How to add in different site?

write.table(data,"./result/Salt_Injection_Discharge.txt", quote = F,
            col.names = T, row.names = F)
