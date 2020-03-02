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
#calculate a decimal year, but account for leap year year + (doy-1)/365
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$doy - 1) /366),
                       datD$year + ((datD$doy - 1) /365))
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
#################


# question 4 code #
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

help(expression)
help(paste)


#################


#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")


# new plot window 
# show plot in new window with larger margins (no axis label cut off)
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)




# show same plot but include polygon displaying sd of daily temps
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)


# show ame plot but use axis function to manipulate axes instead of default args
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle



# same plot but include legend now -- top right corner
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border


# improve legend appearance. Use square pt trick
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border


### question 5 code ###

head(datD)
colnames(datD)
discharge_2017 <- datD[datD$year == 2017,6]

datD_2017 <- datD[datD$year == 2017,]
daily_discharge_2017 <- aggregate(datD_2017$discharge, by=list(datD_2017$doy), FUN="mean")

dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Time", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
lines(daily_discharge_2017$x, col = "red")
#######################################################

#month_strings <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov","Dec")
#axis(1, at = 1 seq(0,11, by=1), #tick intervals
    # lab=month_strings) #tick labels
#######################################################
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017 Observations"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border




#################################################################################################################################



### Question 7 code ###



#find length of each column of datP. Look for days of year and year where there are full 24 hours of data collected. 

agg_datP <- aggregate(datP, by = list(datP$doy,datP$year), length) 
#rename firs two columns for clarity
colnames(agg_datP) <- c("agg_doy", "agg_year", "STATION","STATION_NAME", "DATE" ,"HPCP","doy","year","hour","decDay","decYear")   
agg_datP
#only keep days where there are 24 hours of data collected
agg_datP_full <- agg_datP[agg_datP$doy ==24, ]
agg_datP_full

#create new dataframe with day of year, year, and then add third column with doy_year (will help with later indexing)
full_days_df <- data.frame(agg_datP_full$agg_doy, agg_datP_full$agg_year)
colnames(full_days_df) <- c("doy", "year")
full_days_df$doy_year <- paste(full_days_df$doy, "_",full_days_df$year)


datP$doy_year <- paste(datP$doy, "_",datP$year)

#index to get rows of datP where doy_year is seen in the full_days_df value
datP_full_days_df <- datP[datP$doy_year %in% full_days_df$doy_year, ]
#datP_full_days_df is dataframe of all columns of datP with only days with all 24 hours of data. 


#make plot of all discharge measurements and mark which ones come from days of full data

#create doy_year for later comparison to track which data comes from days with complete precip data collection
datD$doy_year <- paste(datD$doy,"_",datD$year)
datD_aggregate_by_doy_year <- aggregate(datD$discharge, by= list(datD$doy_year, datD$decYear), FUN = median)
colnames(datD_aggregate_by_doy_year) <- c("doy_year", "dec_year", "median_discharge")



# start plot in new plot window
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
   
plot(datD_aggregate_by_doy_year$dec_year,datD_aggregate_by_doy_year$median_discharge,
     type = "l",
     xlab = "Year",
     ylab = "Daily Median Discharge",
     main = "Question 7 Plot")

#create dataframe of plot info for only days where full precipitation data was collected
datP[datP$doy_year %in% full_days_df$doy_year, ]
full_precip_discharge <- datD_aggregate_by_doy_year[ datD_aggregate_by_doy_year$doy_year %in% full_days_df$doy_year, ]
colnames(full_precip_discharge) <- c("doy_year", "dec_year", "median_discharge")

#overlay red points to indicate the data that came from complete precipiptation data days
points(full_precip_discharge$dec_year, full_precip_discharge$median_discharge, col = "red")
legend("topright", c("Median Discharge","Value from day of full precipitation recording"), #legend items
       lwd=c(2,NA),#lines
       col=c("black","red"),#colors
       pch=c(NA,1),#symbols
       bty="n")#no legend border





