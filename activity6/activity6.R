#### ACTIVITY 6 ####


#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)



# Read in data

g1966 <- readOGR("Y:\\Students\\mgentile\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
plot(g1966)

g1998 <- readOGR("Y:\\Students\\mgentile\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
plot(g1998, col = "black")


g2005 <- readOGR("Y:\\Students\\mgentile\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
plot(g2005, col = "black", axes = TRUE) #axes value are vector coordinates in space


g2015 <- readOGR("Y:\\Students\\mgentile\\a06\\GNPglaciers\\GNPglaciers_2015.shp")
plot(g2015, col = "black", axes = TRUE)


str(g2015)
#reference specific polygon
g2015@polygons[[1]]

#for future reference. reference projection
g2015@proj4string

head(g2015@data)










































































