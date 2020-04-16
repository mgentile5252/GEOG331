### activity 7 ###

#install.packages(c("raster", "sp", "rgeos", "mapview", "caret","randomForest","nnet"))


library(raster)
library(sp)
library(rgdal)
library(rgeos)
#library(mapview)
library(caret)
library(randomForest)
library(nnet)



# read in data

# file path on personal computer
#/Users/matthewgentile/Documents/GitHub/GEOG331/oneida/sentinel/T18TVN_20190814T154911_B02_20m.tif 

dirR <- "/Users/matthewgentile/Documents/GitHub/GEOG331/oneida/"
#read in Sentinel data

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB5 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B05_20m.tif"))
rdatB6 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B06_20m.tif"))
rdatB7 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B07_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))
rdatB11 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B11_20m.tif"))
rdatB12 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B12_20m.tif"))
clouds <- raster(paste0(dirR,"/sentinel/MSK_CLDPRB_20m.tif"))

#read in validation data
#here verbose=FALSE hiddes
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
built <- readOGR(paste0(dirR,"/Oneida/built.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)



#stack red green blue
rgbS <- stack(rdatB4,rdatB3,rdatB2)
#stack all raster data
allbands <- stack(rdatB2,rdatB3,rdatB4,rdatB5,rdatB6,rdatB7, rdatB8,rdatB11, rdatB12,clouds)


#view raster, maximum digigtal is around 20000 so set scale to that
plotRGB(rgbS, scale=20000)


#view raster, maximum digigtal is around 20000 so set scale to that
# add linear stretch contrast
plotRGB(rgbS, scale=20000, stretch="lin")



#use mapview package
#view rgb and set up a contrast stretch, exclude clouds with high values

# MAPVIEW PACKAGE NOT LOADING
#viewRGB(rgbS,r=1,g=2,b=3,maxpixels =  2297430, #view all pixels don' lower resolution
       # quantiles = c(0.00,0.995), #quantilesfor stretch. Cuts off high reflectance from clouds
       # homebutton=FALSE,
        #viewer.suppress=FALSE)#view in Rstudio 


# work with clouds

plot(allbands[[10]])


#set clouds to NA
allbandsCloud <- list()
for(i in 1:9){
      allbandsCloud[[i]] <- setValues(allbands[[i]],
                                      ifelse(getValues(allbands[[10]])>60,NA,getValues(allbands[[i]])))
      
}
allbandsCloudf <- stack(allbandsCloud[[1]],allbandsCloud[[2]],allbandsCloud[[3]],allbandsCloud[[4]],allbandsCloud[[5]],allbandsCloud[[6]],allbandsCloud[[7]],allbandsCloud[[8]],allbandsCloud[[9]])




plot(allbandsCloudf)


plotRGB(allbandsCloudf,r=4, g=3, b=2,  
        scale=10000, 
        stretch="lin", 
        margins=TRUE,
        colNA="grey50")




### Classify landcover ###

#set up training and validation data


#set seed so samples always the same
set.seed(12153)

#randomly select 
algSamp <- sort(sample(seq(1,120),60))
#set up vector for data type
algData <- rep("train",120)
#randomly replace half of the data to be validating data
algData[algSamp] <- "valid"


waterSamp <- sort(sample(seq(1,120),60))
#set up vector for data type
waterData <- rep("train",120)
#randomly replace half of the data to be validating data
waterData[waterSamp] <- "valid"


agriSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
agriData <- rep("train",120)
#randomly replace half of the data to be validating data
agriData[agriSamp] <- "valid"


builtSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
builtData <- rep("train",120)
#randomly replace half of the data to be validating data
builtData[builtSamp] <- "valid"

forestSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
forestData <- rep("train",120)
#randomly replace half of the data to be validating data
forestData[forestSamp] <- "valid"


wetlandsSamp  <- sort(sample(seq(1,120),60))
#set up vector for data type
wetlandsData <- rep("train",120)
#randomly replace half of the data to be validating data
wetlandsData[wetlandsSamp] <- "valid"




#create id table that gives each landcover an ID
landclass <- data.frame(landcID= seq(1,6),
                        landcover = c("algal bloom", "open water","agriculture","built","forest","wetlands"))

#set up table with coordinates and data type (validate or train) for each point
landExtract <-  data.frame(landcID = rep(seq(1,6),each=120),
                           sampleType=c(algData,waterData,agriData,builtData,forestData, wetlandsData),
                           x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],built@coords[,1],forest@coords[,1],wetlands@coords[,1] ),
                           y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],built@coords[,2],forest@coords[,2],wetlands@coords[,2] ))





#extract raster data at each point
#using point coordinates
rasterEx <- data.frame(extract(allbandsCloudf,landExtract[,3:4]))
#give names of bands
colnames(rasterEx) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")

#combine point information with raster information
dataAll <- cbind(landExtract,rasterEx)
#preview
head(dataAll)


trainD <- dataAll[dataAll$sampleType == "train",]
validD <- dataAll[dataAll$sampleType == "valid",]





### RANDOM FOREST CLASSIFIER ###













