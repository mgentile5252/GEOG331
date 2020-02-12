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

























