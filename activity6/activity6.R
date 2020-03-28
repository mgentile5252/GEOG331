#### ACTIVITY 6 ####

install.packages("raster")


install.packages(c("sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)


#⁨Users⁩ ▸ ⁨matthewgentile⁩ ▸ ⁨Documents⁩ ▸ ⁨GitHub⁩ ▸ ⁨GEOG331⁩
# Read in data

g1966 <- readOGR("/Users/matthewgentile/Documents/GitHub/GEOG331/a06/GNPglaciers/GNPglaciers_1966.shp")
#g1966 <- readOGR("Y:\\Students\\mgentile\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
plot(g1966)

g1998 <- readOGR("/Users/matthewgentile/Documents/GitHub/GEOG331/a06/GNPglaciers/GNPglaciers_1998.shp")
#g1998 <- readOGR("Y:\\Students\\mgentile\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
plot(g1998, col = "black")


g2005 <- readOGR("/Users/matthewgentile/Documents/GitHub/GEOG331/a06/GNPglaciers/GNPglaciers_2005.shp")
#g2005 <- readOGR("Y:\\Students\\mgentile\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
plot(g2005, col = "black", axes = TRUE) #axes value are vector coordinates in space


g2015 <- readOGR("/Users/matthewgentile/Documents/GitHub/GEOG331/a06/GNPglaciers/GNPglaciers_2015.shp")
#g2015 <- readOGR("Y:\\Students\\mgentile\\a06\\GNPglaciers\\GNPglaciers_2015.shp")
plot(g2015, col = "black", axes = TRUE)


str(g2015)
#reference specific polygon
g2015@polygons[[1]]

#for future reference. reference projection info
g2015@proj4string

head(g2015@data)

# practice spplot
spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))




# satellite imagery data

#read in rgb imagery from landsat


redL <- raster("/Users/matthewgentile/Documents/GitHub/GEOG331/a06/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/matthewgentile/Documents/GitHub/GEOG331/a06/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/matthewgentile/Documents/GitHub/GEOG331/a06/glacier_09_05_14/l08_blue.tif")
#redL <- raster("Y:\\Students\\mgentile\\a06\\glacier_09_05_14\\l08_red.tif")
#greenL <- raster("Y:\\Students\\mgentile\\a06\\glacier_09_05_14\\l08_green.tif")
#blueL <- raster("Y:\\Students\\mgentile\\a06\\glacier_09_05_14\\l08_blue.tif")


#check coordinate system
redL@crs


#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)


#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#Subset the plot below to zoom in on closer on a few glaciers
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)




#Working with raster data
#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  #NDVIraster[[i]] <- raster(paste0("Y:\\Students\\mgentile\\a06\\NDVI\\NDVI_",ndviYear[i],".tif"))
   NDVIraster[[i]] <- raster(paste0("/Users/matthewgentile/Documents/GitHub/GEOG331/a06/NDVI/NDVI_",ndviYear[i],".tif"))
}

str(NDVIraster[[1]])

#get Lambert Azimuthal Equal Area projection
NDVIraster[[1]]@crs

plot(NDVIraster[[1]])
# NOTE: Values below zero represent water and other non-vegetative features




#######################################################################################################



# Question 3 code
par(mai=c(1,1,1,1))
par(mfrow = c(1,2))
plot(NDVIraster[[1]], axes = TRUE)
plot(g1966, axes = TRUE)





#######################################################################################################






#Vector data analysis: glacier retreat



#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)

g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)




#######################################################################################################



# Question 4 code


par(mai=c(1,1,1,1))
par(mfrow=c(1,1))
plot(NDVIraster[[1]], axes = FALSE)
plot(g2015p, add = TRUE)








#######################################################################################################

# mock change for practice commit/push from personal computer

# start of code on perosonal computer

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)


# join data together in table not associated with shape file
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
nrow(gAllp1)
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
nrow(gAllp2)
nrow(g2015p@data)

#names in 2015 data frame of 2 glaciers are different. Need to be same for later join() call.
g2015p@data$GLACNAME <- levels(g1966p@data$GLACNAME)
      
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")
nrow(gAll)





# make plot of area of each glacier
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
      points(c(1966,1998,2005,2015), 
             c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
             type="b", 
             pch=19, col=rgb(0.5,0.5,0.5,0.5))
      
}   

# more helpful to look at the percent change



#######################################################################################################



# Question 5 code
#gAll_new <- head(gAll,-2)
#gAll_new <- gAll[!((gAll$GLACNAME == "Miche Wabun") | (gAll$GLACNAME == "North Swiftcurrent Glacier")),  ]

gAll$area_change <- ((gAll$a1966m.sq - gAll$a2015m.sq)/gAll$a1966m.sq)*100
area_change_df <- as.data.frame(gAll$GLACNAME)
area_change_df$percent_change <- gAll$area_change
colnames(area_change_df) <- c("GLACNAME", "percent_change")

g2015p_new <- g2015p
g2015p_new@data <- join(g2015p@data,area_change_df, by = "GLACNAME", type = "full")

nrow(area_change_df)
nrow(g2015p@data)
nrow(g2015p_new@data)
#g2015_new@data <- g2015_new@data[:-2]






spplot(g2015p_new, "percent_change")





#######################################################################################################

#commented out because of gDifference function --- diffPoly not defined

#diffPoly <- gDifference(g1966p, g2015p)
#plot(diffPoly)


#plot with NDVI

#plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
#plot(diffPoly,col="black", border=NA,add=TRUE)





#######################################################################################################



# Question 6 code

area_change_df$GLACNAME[area_change_df$percent_change == max(area_change_df$percent_change)]
#Boulder Glacier --- glacier with max percent change in area from 1966 - 2015


boulder_1966 <- subset(g1966p, GLACNAME == "Boulder Glacier")
boulder_1998 <- subset(g1998p, GLACNAME == "Boulder Glacier")
boulder_2005 <- subset(g2005p, GLACNAME == "Boulder Glacier")
boulder_2015 <- subset(g2015p, GLACNAME == "Boulder Glacier")

plot(boulder_1966)
plot(boulder_1998,border = "red", add = TRUE)
plot(boulder_2005,border = "blue", add = TRUE)
plot(boulder_2015,border = "green", add = TRUE)

title("Extent of Boulder Glacier 1966 - 2015")
legend("bottomleft", legend= c("1966","1998", "2005", "2015"),
       col=c("black","blue","red","green"), lty=1)





#######################################################################################################

## COMMENTED OUT BECAUSE diffPoly not defined

#extract NDVI values
#NDVIdiff <- list()
#meanDiff <- numeric(0)
#loop through all NDVI years and use extract function
#for(i in 1:length(ndviYear)){
      #get raster values in the difference polygon
 #     NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
      #calculate the mean of the NDVI values
  #    meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
#}

#plot(ndviYear, meanDiff, type="b",
 #    xlab= "Year",
 #    ylab="Average NDVI (unitless)",
 #    pch=19)


# notes from plot:
# no trend, NDVI high in 2015, 
# may take many years for vegetation to move in and fill in a newly bare glacial area
# may also be prolonged snow cover in many of these areas



#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
      if(is.na(x[1])){
            NA}else{
                  #fit a regression and extract a slope
                  lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE) ### USE TO ANSWER QUESTION 7




#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units



#zonal statistics
#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)


#remove the actual glacier from our statistics
# Since the IDs will be the same, we can subtract the two rasters 
# to end up with an id for only in areas with the buffer

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)






#get the statistics for the rate of vegetation change in the area around the rasterized buffer


meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)



