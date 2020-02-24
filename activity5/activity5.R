### ACTIVITY 5 SCRIPT ###
# initial commit #

#install.packages("lubridate")
library(lubridate)





# read streamflow data in and familarize with how it is entered
# NA values marked as Eqp for equipment

datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)  





#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")                          
head(datP)


#the flag value of A represents Approved for publication - Processing and review completed. 
#Other flags indicate that data was estimated or is provisional

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
help(hm) #Transforms a character or numeric vector into a period object with the specified number of hours, minutes, and seconds. 
timesD <- hm(datD$time)


#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE) #year month date _ hour minutes
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)


#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD) + (minute(timesD)/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))       


# INTIAL PLOT OF DISCHARGE #

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))




# question 3 code #

nrow(datD) #393798
head(datD)
datD$date[1] #10/1/2007
datD$date[393798] #1/1/2019


nrow(datP) # 16150
head(datP)
datP[1:10,]
datP$DATE[1] #20070101
datP$DATE[16150] #20131231









































