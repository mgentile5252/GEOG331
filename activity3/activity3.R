### START ACTIVITY 3 SCRIPT IN CLASS ##





#create a function. The names of the arguements for your function will be in parentheses. 
# Assert function will return error message if logical statement does not return TRUE

assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement and see error message

assert(1 == 2, "error: unequal values")




#evaluate a true statement --- returns nothing
assert(2 == 2, "error: unequal values")

#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")


#############################################################################################


### BEGIN WORKING WITH DATA ###

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("y:\\Students\\mgentile\\a03\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview first row of data
print(datW[1,]) 


#get sensor info from file
# this data table will contain all relevent units
sensorInfo <-   read.csv("y:\\Students\\mgentile\\a03\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)
#names(sensorInfo)
#names(datW)



# get column names from sensorInfo table
# and set weather station colnames  to be the same

colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])




############## DATA QA/QC ############## 
#install.packages("lubridate")
library("lubridate")


#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")



#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]



# Look into amount of missing data (NA)
#see how many values have missing data for each sensor observation
#air temperature

#how many times is.na returns true for the air temperature column
length(which(is.na(datW$air.temperature)))

length(which(is.na(datW$wind.speed)))

length(which(is.na(datW$precipitation)))

length(which(is.na(datW$soil.moisture)))

length(which(is.na(datW$soil.temp)))



#make a plot with filled in points (using pch)
#line lines

#data abruptly stops in the second week of July
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")



#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")



#make a new column to work with that indicates that I am conducting QAQC
#if air temp is below 0, recird NA
#if air temp is not below 0 record actual air temp
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)






#check the values at the extreme range of the data and throughout the percentiles
quantile(datW$air.tempQ1)




#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  



# QUESTION 4
datW[datW$air.tempQ1 < 8, 1]  

datW[datW$air.tempQ1 < 8, 14]  
#173 177 177 177 177 177
# June 22nd and June 26th

datW[datW$air.tempQ1 > 33,1]  
datW[datW$air.tempQ1 > 33,14]  
#182 182 182 182 182 182 182 183
# July 1 amd July 2

