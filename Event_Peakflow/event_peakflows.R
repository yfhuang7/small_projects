### Finding Event Peakflows ##################
#
# Goal: finding event peakflows
#       1. Maximum peakflows during highflow events (event period is computed)
#       2. The first peakflow for every peakflow events
#
# created by yfhuang 20181030
#############################################

#clean the working space
rm(list = ls(all = T))
ls(all = T)

### Environment Setting (NEED MANUAL INPUT)--------

# Working directory
setwd("C:/Users/Yu-Fen Huang/Dropbox/R_Script/R_workplace/Event_Peakflow")
dir.data <- './Data/'
dir.result <- './Result/'

# Conversion
m2ft = 3.28084
cms2cfs = 35.3146662127

# USGS gage
siteNumber = "16238000"
gage <- read.csv(paste(dir.data,"USGS_",siteNumber,".csv",sep = ""), stringsAsFactors = F)
gage$dateTime <- as.POSIXct(gage$dateTime)

### test --------------------------------
# test short period
# gage.short <- gage[174060:180000,]

# test longer period
gage.short <- gage[!is.na(gage$X_00060_00000),]

# Cannot have NAs
x <- gage[!is.na(gage$X_00060_00000),c(4,5,11)]

# Calculating the differences
x.diff <- diff(x[,2], lag = 1)
x.dataframe <- data.frame(dtime = x[,1], original = x[,2], step = c(NA, x.diff), start = NA)

d = 96  # only count one peak within how much time
threshold.origin = 0  # minimum value of peakflow
threshold.step = 5  # at least need to increase as threshold

# look for the event start point
# i=2136
n = 2 # n+1 consecutive increasing slopes
note = c()
for (i in 2:(nrow(x.dataframe)-n-1)) {
  if(!is.na(x.dataframe$start[i-1])) next
  
  if (x.dataframe$step[i+n] > x.dataframe$step[i+n-1] & 
      x.dataframe$step[i+n-1] > x.dataframe$step[i+n-2] &
      x.dataframe$original[i+1] >= threshold.origin &
      x.dataframe$step[i+1] >= threshold.step){
    x.dataframe$start[i] <- i
    note = rbind(note, i)
  }
}

pks = c()
# look for the peak
for (i in 1:(length(note)-1)){
  pk.max <- max(x.dataframe$original[note[i]:note[i+1]])
  pk.sum <- sum(x.dataframe$original[note[i]:note[i+1]])
  loc <- note[i] + which.max(x.dataframe$original[note[i]:note[i+1]]) - 1
  pk <- cbind(pk.max, pk.sum, loc, note[i], note[i+1])
  pks <- rbind(pks, pk)
}

pks.old <- as.data.frame(pks)
## Sensitive test ------------------------------------
## maximum within assigned period threshold (d)

# first
pks <- pks.old
pks$loc.diff <- c(NA, diff(pks[,3])) # Count the day differences between peaks

# second
any(pks.new$loc.diff[2:nrow(pks.new)] <d)
pks <- pks.new

pks$loc.diff[1] = 9999
pks <- pks[!is.na(pks$pk.max),]
pks.new <- pks[1,]
for (i in 2:(nrow(pks))){
  if (pks$loc.diff[i] < d){
    if (pks$pk.max[i] >= pks$pk.max[i-1]){
      pks.new[nrow(pks.new),] <- pks[i,]
    } else{
      pks.temp <- pks[(i-1)+which.max(c(pks$pk.max[i],pks$pk.max[i-1])),]
      pks.new <- rbind(pks.new, pks.temp)
    }
    
  } else {
    pks.new <- rbind(pks.new, pks[i,])
  }

}
pks.new$loc.diff <- c(NA, diff(pks.new$loc))
pks.new <- pks.new[pks.new$loc.diff!=0 | is.na(pks.new$loc.diff),]


## The first peak within assigned period threshold (d)

# first
pks <- pks.old
pks$loc.diff <- c(NA, diff(pks[,3])) # Count the day differences between peaks

# second
any(pks.new$loc.diff[2:nrow(pks.new)] <d)  # redue the second till "FALSE"
pks <- pks.new

pks$loc.diff[1] = d
pks <- pks[!is.na(pks$pk.max),]
pks <- rbind(pks, pks[nrow(pks),])
pks.new <- pks[1,]
for (i in 2:(nrow(pks)-1)){
  if (pks$loc.diff[i] < d){
    if (pks$loc.diff[i+1] < d){
      if (pks$loc.diff[i+1] > pks$loc.diff[i]){
        pks.temp <- pks[i-1,]
        pks.new <- rbind(pks.new, pks.temp)
      } else {
        pks.temp <- pks[i,]
        pks.new <- rbind(pks.new, pks.temp)
      }
    } else {
      pks.temp <- pks[i-1,]
      pks.new <- rbind(pks.new, pks.temp)
    }
  } else {
    pks.new <- rbind(pks.new, pks[i,])
  }
}

pks.new$loc <- as.factor(pks.new$loc)
pks.new <- pks.new[!duplicated(pks.new$loc),]
pks.new <- pks.new[pks.new$loc.diff!=0 | is.na(pks.new$loc.diff),]
pks.new$loc.diff <- c(NA, diff(as.numeric(as.character(pks.new$loc))))


## peakflow accumulation during the peakflow event

## Plot
pks <- pks.new
pks$loc <- as.numeric(as.character(pks.new$loc))
plot(x$dateTime, x$X_00060_00000, xlim = c(min(x$dateTime),max(x$dateTime)), 
     ylim = c(min(x$X_00060_00000),max(x$X_00060_00000)))
par(new=T)
plot(x$dateTime[pks$loc],x$X_00060_00000[pks$loc], col = "red", xlim = c(min(x$dateTime),max(x$dateTime)), 
                                         ylim = c(min(x$X_00060_00000),max(x$X_00060_00000)))


plot(gage$X_00060_00000, gage$X_80154_00000)
plot(x$X_00060_00000[pks[,3]],x$X_80154_00000[pks[,3]], ylim = c(0, 8000))
plot(gage$X_80154_00000, gage$X_80155_00000)



### comparing to other package ---------------------------------
## pracma
library(pracma)
test <- gage.short[!is.na(gage.short$X_00060_00000),]
test.peak <- as.data.frame(findpeaks(test$X_00060_00000, threshold = 0, nups = 1))

summary(test.peak)

test.pos <- test.peak[test.peak2$V2,]$V2
# test.peak <- test.peak[test.peak$V1 >= mean(test.peak$V1),]

test$dateTime <- as.POSIXct(test$dateTime)
plot(test$dateTime, test$X_00060_00000, xlim = c(min(test$dateTime),max(test$dateTime)),
     ylim = c(min(test$X_00060_00000, na.rm = T),max(test$X_00060_00000, na.rm = T)))
par(new=T)
plot(test$dateTime[test.pos], test$X_00060_00000[test.pos], 
     xlim = c(min(test$dateTime),max(test$dateTime)), 
     ylim = c(min(test$X_00060_00000, na.rm = T),max(test$X_00060_00000, na.rm = T)),
     col = "red")

plot(test$X_00060_00000[test.pos], test$X_80154_00000[test.pos])

## pastecs 
# NOTE: it use turnpoints() to find all the peaks and pits, 
#       yet it's too much since the streamflow vibrate a lot.
# install.packages("pastecs")
library(pastecs)
test <- str(x[,2])

test.tp <- turnpoints(test)
summary(test.tp)
plot(test)
par(new=T)
plot(test[test.tp$pos], col = "red")
