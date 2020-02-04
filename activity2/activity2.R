#create file in class

####### WORKING WITH DATA ####### 

#make a vector of tree heights in meters
heights <- c(30,41,20,22)

#convert to cm through multiplication on each element
heights_cm <- heights*100
heights_cm


#explore subsetting vector
heights[3] #20
heights_cm[2] #4100


#get more info on the matrix function
help(matrix)


#create matrix with 2 columns and fill by row
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#create matrix with same input vector of numbers but fill by column
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol


#explore subsetting matrix by [row, column]

#value in first row, second column
Mat.bycol[1,2] 

#all values in row 1
Mat.bycol[1,]

#all values in column 2
Mat.bycol[,2]



####### DATAFRAMES ####### 

#Follow instructions in activity 2 to copy data folder to student folder


#read in weather station file from the data folder
datW <- read.csv("y:\\Students\\mgentile\\a02\\2011124.csv")



#get more information about the dataframe
?str #Compactly Display the Structure of an Arbitrary R Object
str(datW)

nrow(datW) #157849 rows
ncol(datW) #9 columns


#create a column with a proper date format
#note the format here dataframe$column (datW$newcolumn)
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

ncol(datW) #now has 10 columns

#create antother column that holds the year as a numeric value
datW$year <- as.numeric(format(datW$dateF,"%Y")) 


#QUESTION 2 ExAMPLE DATA TYPES
char_example <- c("hello", "my", "name", "is", "Matt Gentile", "1")
char_example
class(char_example)

num_example <- c(3.2,3,4.2,6,8.921)
num_example
class(num_example)

int_example <- as.integer(c(1,2,3,4,5,6))
int_example
class(int_example)

fact_example <- factor(c("nice","mean","angry","nice","mean","angry"))
fact_example
class(fact_example)


##### Descriptive Statistics and Histograms ######



#find out all unique site names
levels(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#use na.rm = True argument to remove NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm= TRUE)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)




#more efficient method
#get mean across all sites with aggregate function
#specify mean as the fun argument so takes average

averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

# change the automatic output of column names from group.1, x to be more meaningful
# MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert site name level to numeric value so it can be referenced later
datW$NAME
datW$siteN <- as.numeric(datW$NAME) 
datW$siteN



#make a histogram for the site number 1
#main= is the title name argument --- Site Name

hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")



# create same histogram with veritcal lines for mean and vertical dashed lines for 1 sd away 



hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

# want veritcal lines so use v = argument

# line for mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

# line for lower sd
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# line for higher sd
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


# HISTOGRAMS FOR OTHER SITES FOR QUESTION 4


# site 2 - LIVERMORE, CA

hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

# want veritcal lines so use v = argument

# line for mean
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

# line for lower sd
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# line for higher sd
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)





# site 4 - MORMON FLAT, AZ

hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

# want veritcal lines so use v = argument

# line for mean
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

# line for lower sd
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# line for higher sd
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


# site 5 - MORRISVILLE 6 SW, NY

hist(datW$TAVE[datW$siteN == 5],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

# want veritcal lines so use v = argument

# line for mean
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

# line for lower sd
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# line for higher sd
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)




















