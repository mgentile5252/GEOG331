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


#####################################################################################################################################
# QUESTION 4
datW[datW$air.tempQ1 < 8, 1]  

datW[datW$air.tempQ1 < 8, 14]  
#173 177 177 177 177 177
# June 22nd and June 26th

datW[datW$air.tempQ1 > 33,1]  
datW[datW$air.tempQ1 > 33,14]  
#182 182 182 182 182 182 182 183
# July 1 amd July 2
#####################################################################################################################################



### Measurements outside of sensor capabilities ###



#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#####################################################################################################################################

### CODE FOR TEST FOR QUESTION 5 ###


# old asset call for syntax
assert(1 == 2, "error: unequal values")

assert(length(datW$DD) == length(lightscale), "error: inputted statement not true")
assert(length(datW$DD) != length(lightscale), "error: inputted statement not true") #error received

assert(length(datW$DD[lightscale > 0]) == length(which(lightscale > 0)), "error: inputted statement not true")
assert(length(datW$DD[lightscale > 0]) != length(which(lightscale > 0)), "error: inputted statement not true")#error received

#check specific values line up
assert(lightscale[974] > 0, "error: inputted statement not true")
assert(datW$DD[974] %in% datW$DD[lightscale > 0], "error: inputted statement not true" )

assert(lightscale[967] > 0, "error: inputted statement not true")#error received
assert(datW$DD[967] %in% datW$DD[lightscale > 0], "error: inputted statement not true" )#error received

#####################################################################################################################################


#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))




#####################################################################################################################################

### QUESTION 6 ###


datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))

# check to see if NAs were added (ie unreliable data during storms was removed)
assert(length(which(is.na(datW$wind.speedQ2))) == length(which(is.na(datW$wind.speed))), "error: inputted statement not true")#error received


#####################################################################################################################################


# Researchers share that in mid-July the weather station was tampered with
# Unreliable data marked by dramatic changes in soil temperature and moisture 
# also check that sensor was pulled from ground and unplgged cable at same time



### QUESTION 7 ###

datW$timestamp[1]
length(datW$timestamp)
datW$timestamp[2118]

plot( datW$doy, datW$soil.moisture)


# july DOY 182 - 212
# test period 175 - 215
dat_july <- datW[datW$doy < 215 & datW$doy > 175,]


#look at just july data to get closer idea of when outage occured
plot(dat_july$doy, dat_july$soil.moisture) #Looks like sensor went out around DOY = 193
plot(dat_july$doy, dat_july$soil.temp) #Looks like sensor went out around DOY = 193



#Look at data of 10 days before outage 
dat_before <- dat_july <- datW[datW$doy < 193 & datW$doy > 185,]
plot(dat_before$doy, dat_before$soil.moisture, ylab = "Soil Moisture",
     xlab = "Day of Year", main = "Soil Mosture Recordings Before Outage near July 12") 
plot(dat_before$doy, dat_before$soil.temp, ylab = "Soil Moisture",
     xlab = "Day of Year", main = "Soil Mosture Recordings Before Outage near July 12") 

# figure out of mean soil moisture of each day jumped signficantly 


mean_soil_moistures <- c()
mean_soil_temperatures <- c()

for (i in 180:191){
  day_avg_m <- mean(datW[datW$doy == i,]$soil.moisture,na.rm = TRUE)
  day_avg_t <- mean(datW[datW$doy == i,]$soil.temp, na.rm = TRUE)
  
  mean_soil_moistures[i-179] <- day_avg_m
  mean_soil_temperatures[i-179] <- day_avg_t
  
}
mean_soil_moistures
mean_soil_temperatures


## explore average temperature for these days and precipitation



plot(dat_before$doy, dat_before$air.tempQ2, xlab = "Day Of Year", ylab = "Air Temp", main = "Air Temperature")
plot(dat_before$doy, dat_before$precipitation, xlab = "Day Of Year", ylab = "Precipitation", main = "Preicpiation")


#####################################################################################################################################


### QUESTION 8 ###

# create table with avg. air temp, wind speed, soil moisture, soil temp
# total preciptation as well 
# indicate how many observations in each calc and time period


# first must find values to fill table with
# check to see how many NA values were not used in calculation 

total_precip <- sum(datW$precipitation, na.rm = TRUE)
#177.828
length(datW$precipitation)
#2118
length(which(is.na(datW$precipitation)))
#0

avg_temp <- mean(datW$air.tempQ2, na.rm = TRUE)
#20.00936
length(datW$air.tempQ2)
#2118
length(which(is.na(datW$air.tempQ2)))
#13



avg_wind <- mean(datW$wind.speedQ2, na.rm = TRUE)
# 0.4452637
length(datW$wind.speedQ2)
#2118
length(which(is.na(datW$wind.speedQ2)))
#13


avg_s_temp <- mean(datW$soil.temp, na.rm = TRUE)
#17.41777
length(datW$soil.temp)
#2118
length(which(is.na(datW$soil.temp)))
#707

avg_s_mois <- mean(datW$soil.moisture, na.rm = TRUE)
#0.1445495
length(datW$soil.moisture)
#2118
length(which(is.na(datW$soil.moisture)))
#707


# create vector of the names for table rows
table_names <- c("Average Air Temp", 
                 "Average Wind Speed", 
                 "Average Soil Temp",
                 "Average Soil Moisture",
                 "Total Precipitation")

#values <- c(20.00936, 0.4452637, 17.41777, 0.1445495, 177.828)

#calcs <- c(2015, 2015, 1411,1411, 2118)


#create matrix -> table
mat <- matrix(c(20.00936, 0.4452637, 17.41777, 0.1445495, 177.828,2015, 2015, 1411,1411, 2118), ncol = 2)
rownames(mat) <- table_names
colnames(mat) <- c("Value", "# of Obs")
info_table <- as.table(mat)


#####################################################################################################################################


### QUESTION 9 ###

par(mfrow=c(2,2))
soil_m_plot <- plot(datW$doy, datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture")
  
  
soil_t_plot <- plot(datW$doy, datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature")
  
  
precip_plot <-  plot(datW$doy, datW$precipitation, xlab = "Day of Year", ylab = "Precipitation")
  

air_temp_plot <- plot(datW$doy, datW$air.tempQ2, xlab = "Day of Year", ylab = "Air Temperature")







