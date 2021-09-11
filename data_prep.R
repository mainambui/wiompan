_________________
##download Gurobi Optimizer fom the website https://www.gurobi.com/downloads/gurobi-software/
##obtain the academic license
##install the license on the terminal using: grbgetkey ad0445fe-fcf2-11eb-ba3c-0242ac120002
<<<<<<< HEAD
##grbgetkey fa525344-ff4c-11eb-b77b-0242ac120002# license server
#info  : GRB_LICENSE_FILE=/Users/maina/Gurobi/gurobi.lic
=======
##grbgetkey fa525344-ff4c-11eb-b77b-0242ac120002##server license
>>>>>>> a624e5026a63791acf40778072bb5435c701261d
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
#("devtools")
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
wio.eez<-readOGR(dsn='~/Documents/tbca/Cleaned/WIO_EEZ/','WIO_EEZ')
wio.shelf<-readOGR(dsn='~/Documents/tbca/Cleaned/WIO_Geomorphic/','Shelf')
wio.slope<-readOGR(dsn='~/Documents/tbca/Cleaned/WIO_Geomorphic/','Slope')


##select countries of interest from eez and delete columns
#eez<-unique(wio.eez@data$Territory1)
#non.wio<-c("Djibouti","Maldives","Chagos Archipelago")
#wio<-eez[!eez %in% non.wio]
#wio.roi <- subset(wio.eez, Territory1 %in% wio)
#cols<-names(wio.roi)
#filtercols<-cols[!cols %in% c("OBJECTID","Territory1")]
#roi.wio <- wio.roi[,!(names(wio.roi) %in% filtercols)] 

#Project to Afica_Albers
afr.alb<-CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

roi.wio.alb<- spTransform(wio.eez, afr.alb)

#create planning units of 1km2
source("makegrid.R")
# hex - without clipping
hex_grid_shelf <- make_grid(wio.shelf, type = "hexagonal", cell_area = 1000000, clip = FALSE)
hex_grid_slope <- make_grid(wio.slope, type = "hexagonal", cell_area = 1000000, clip = FALSE)
hex_grid_eez <- make_grid(roi.wio.alb, type = "hexagonal", cell_area = 25000000, clip = FALSE)

#plot(roi.wio.alb, col = "grey50", bg = "light blue", axes = FALSE)
#plot(hex_grid, border = "orange", add = TRUE)
#box()
# hex - with clipping
#hex_grid_c <- make_grid(roi.wio.alb, type = "hexagonal", cell_area = 1000000, clip = TRUE)
#plot(roi.wio.alb, col = "grey50", bg = "light blue", axes = FALSE)
#plot(hex_grid_c, border = "orange", add = TRUE)
#box()

# Extract polygon/p ID's
pid.shelf <- sapply(slot(hex_grid_shelf, "polygons"), function(x) slot(x, "ID")) 
pid.slope <- sapply(slot(hex_grid_slope, "polygons"), function(x) slot(x, "ID")) 
pid.eez <- sapply(slot(hex_grid_eez, "polygons"), function(x) slot(x, "ID"))

# Create dataframe with correct rownames
p.df.shelf <- data.frame( ID=1:length(hex_grid_shelf), row.names = pid.shelf)    
p.df.slope <- data.frame( ID=1:length(hex_grid_slope), row.names = pid.slope)  
p.df.eez <- data.frame( ID=1:length(hex_grid_eez), row.names = pid.eez)

# Try coersion again and check class
pu.shelf <- SpatialPolygonsDataFrame(hex_grid_shelf, p.df.shelf)
class(pu.shelf) 
pu.slope <- SpatialPolygonsDataFrame(hex_grid_slope, p.df.slope)
class(pu.slope) 
pu.eez <- SpatialPolygonsDataFrame(hex_grid_eez, p.df.eez)
class(pu.eez) 

writeOGR(obj=pu.shelf,dsn='~/Documents/tbca/PlanningUnits', layer="wioShelf_1km", driver="ESRI Shapefile",overwrite_layer=TRUE)
writeOGR(obj=pu.slope,dsn='~/Documents/tbca/PlanningUnits', layer="wioSlope_1km", driver="ESRI Shapefile",overwrite_layer=TRUE)
writeOGR(obj=pu.eez,dsn='~/Documents/tbca/PlanningUnits', layer="wioEEZ_1km", driver="ESRI Shapefile",overwrite_layer=TRUE)

rm(list=ls())



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


##############
#Priortize R
################










