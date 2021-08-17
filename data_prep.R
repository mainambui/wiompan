_________________
##download Gurobi Optimizer fom the website https://www.gurobi.com/downloads/gurobi-software/
##obtain the academic license
##install the license on the terminal using: grbgetkey ad0445fe-fcf2-11eb-ba3c-0242ac120002
##install Gurobi on R using the cide below 
#https://www.gurobi.com/documentation/9.1/refman/ins_the_r_package.html
#Resources
#https://mathmarecol.github.io/SpatialPlanning_Workshop2021/index.html
#install.packages('/Library/gurobi912/mac64/R/gurobi_9.1-2_R_4.0.2.tgz', repos=NULL)
#install.packages('slam')
#source('/Library/gurobi912/mac64/examples/R/mip.R', chdir = TRUE)
#if (!require(remotes)) install.packages("remotes")
#remotes::install_github("prioritizr/prioritizr")
#if (!require(devtools))
 # install.packages("devtools")
#devtools::install_github("prioritizr/prioritizrdata")
______________________________________________
# load packages
library(prioritizr)
library(prioritizrdata)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(mapview)
library(units)
library(scales)
library(assertthat)
library(gridExtra)
library(dplyr)

##create planning units'
rm(list=ls())
#wio extent
#read eez file
wio.eez<-readOGR(dsn='~/OneDrive - Macquarie University/Projects/WIOMSA/Connectivity/connect.share/shapefiles/','wioeez')

##select countries of interest from eez and delete columns
eez<-unique(wio.eez@data$Territory1)
non.wio<-c("Djibouti","Maldives","Chagos Archipelago")
wio<-eez[!eez %in% non.wio]
wio.roi <- subset(wio.eez, Territory1 %in% wio)

cols<-names(wio.roi)
filtercols<-cols[!cols %in% c("OBJECTID","Territory1")]
roi.wio <- wio.roi[,!(names(wio.roi) %in% filtercols)] 

#Project to Afica_Albers
afr.alb<-CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

roi.wio.alb<- spTransform(roi.wio, afr.alb)

#create planning units of 1km2
source("makegrid.R")
# hex - without clipping
#hex_grid <- make_grid(roi.wio.alb, type = "hexagonal", cell_area = 1000000, #clip = FALSE)
#plot(roi.wio.alb, col = "grey50", bg = "light blue", axes = FALSE)
#plot(hex_grid, border = "orange", add = TRUE)
#box()
# hex - with clipping
hex_grid_c <- make_grid(roi.wio.alb, type = "hexagonal", cell_area = 1000000, clip = TRUE)
plot(roi.wio.alb, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid_c, border = "orange", add = TRUE)
box()

# Extract polygon/p ID's
pid <- sapply(slot(hex_grid_c, "polygons"), function(x) slot(x, "ID")) 

# Create dataframe with correct rownames
p.df <- data.frame( ID=1:length(hex_grid), row.names = pid)    

# Try coersion again and check class
pu <- SpatialPolygonsDataFrame(hex_grid, p.df)
class(pu) 
setwd('~/OneDrive - Macquarie University/Projects/WIOMSA/Connectivity/connect.share/Marxan/planningUnits')
#writeOGR(layer=pu,dsn='.', "planningUnits.25", driver="ESRI Shapefile")
writeOGR(obj=pu,dsn='.', layer="wioplanningUnits", driver="ESRI Shapefile",overwrite_layer=TRUE)
plot(pu)







##Burn MPAs onto the planning units


##extract biodiversity features
#Polygon coverage
# cell areas
hex_grid_c <- gArea(hex_grid_c, byid = T) %>% 
  data.frame(id = names(.), area = ., stringsAsFactors = FALSE) %>% 
  SpatialPolygonsDataFrame(hex_grid_c, .)
hex_cover <- gIntersection(hex_grid_c, MYPOLYGONS, byid = TRUE) %>% 
  gArea(byid = TRUE) %>% 
  data.frame(id_both = names(.), cover_area = ., stringsAsFactors = FALSE) %>% 
  separate(id_both, "id", extra = "drop") %>% 
  merge(hex_grid_c, ., by = "id")
hex_cover$cover_area[is.na(hex_cover$cover_area)] <- 0
hex_cover$pct_cover <- 100 * hex_cover$cover_area /hex_cover$area














