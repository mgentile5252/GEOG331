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
gAll_new <- gAll[!((gAll$GLACNAME == "Miche Wabun Glacier") | (gAll$GLACNAME == "N. Swiftcurrent Glacier")),  ]

gAll_new$area_change <- ((gAll_new$a1966m.sq - gAll_new$a2015m.sq)/gAll_new$a1966m.sq)*100
area_change_df <- as.data.frame(gAll_new$GLACNAME)
area_change_df$percent_change <- gAll_new$area_change
colnames(area_change_df) <- c("GLACNAME", "percent_change")

g2015_new <- g2015
g2015_new@data <- join(g2015@data,area_change_df, by = "GLACNAME", type = "full")

nrow(area_change_df)
nrow(g2015@data)
nrow(g2015_new@data)
#g2015_new@data <- g2015_new@data[:-2]


spplot(g2015_new, "percent_change")





#######################################################################################################

#commented out because of gDifference function

#diffPoly <- gDifference(g1966p, g2015p)
#plot(diffPoly)









