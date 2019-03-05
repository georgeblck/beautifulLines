rm(list=ls())
dev.off()
#Load packages
library(sp)
library(rgdal)
library(reshape)
library(ggplot2)
library(maptools)
library(rgeos) #create a range standardisation function
library(raster)
library(spdep)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}


#load in a population grid - in this case it's population density from NASA's SEDAC (see link above). It's spatial data if you haven't seen this before in R.
input<-readGDAL("glp10ag30.asc") # the latest data come as a Tiff so you will need to tweak.
input <- readGDAL("GeospatialLineGraphs/gl_gpwv3_pcount_00_ascii_half/glp00ag30.asc")
#input <- readGDAL("GeospatialLineGraphs/gl_gpwv3_pcount_00_ascii_25/glp00ag.asc")
str(input)
small <- FALSE
if(small){
  gerExtent <- extent(raster(xmn = 5, xmx = 16, ymn = 47, ymx = 56))
  brickedInput <- brick(input)
  gerPoly <- as(gerExtent, "SpatialPolygons")
  proj4string(gerPoly) <- CRS(proj4string(brickedInput))
  
  ## Clip the map
  out <- crop(brickedInput, gerPoly)
  input <- as(out, 'SpatialGridDataFrame') 
  rm(out)
  rm(brickedInput)
}
str(input)
## glp10ag30.asc has GDAL driver AAIGrid 
## and has 286 rows and 720 columns

#proj4string(input) = CRS("+init=epsg:4326")

## Warning in `proj4string<-`(`*tmp*`, value = ): A new CRS was assigned to an object with an existing CRS:
## +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
## without reprojecting.
## For reprojection, use function spTransform

#Get the data out of the spatial grid format using "melt" and rename the columns.
values<-melt(input)
names(values)<- c("pop", "x", "y")

#Rescale the values. This is to ensure that you can see the variation in the data
values$pop_st<-range01(values$pop)*0.2
values$x_st<-range01(values$x)
values$y_st<-range01(values$y)

#Switch off various ggplot things

xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

#Add 180 to the latitude to remove negatives in southern hemisphere, then order them.
values$ord<-values$y#+180
values_s<- values[order(-values$ord),]

#Create an empty plot called p
p<-ggplot()

#This loops through each line of latitude and produced a filled polygon that will mask out the lines beneath and then plots the paths on top.The p object becomes a big ggplot2 plot.
for (i in unique(values_s$ord))
{
  p<-p+geom_polygon(data=values_s[values_s$ord==i,],aes(x_st, pop_st+y_st,group=y_st), size=0.2, fill="white", col="white")+ 
    geom_path(data=values_s[values_s$ord==i,],aes(x_st, pop_st+y_st,group=y_st),size=0.2, lineend="round")
}

#show plot!
p +theme(panel.background = element_rect(fill='white',colour='white'))+quiet
