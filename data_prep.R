_________________
##download Gurobi Optimizer fom the website https://www.gurobi.com/downloads/gurobi-software/
##obtain the academic license
##install the license on the terminal using: grbgetkey ad0445fe-fcf2-11eb-ba3c-0242ac120002
##grbgetkey fa525344-ff4c-11eb-b77b-0242ac120002# license server
#info  : GRB_LICENSE_FILE=/Users/maina/Gurobi/gurobi.lic
=======
##grbgetkey fa525344-ff4c-11eb-b77b-0242ac120002##server license
>>>>>>> a624e5026a63791acf40778072bb5435c701261d
##install Gurobi on R using the cide below 
#https://www.gurobi.com/documentation/9.1/refman/ins_the_r_package.html
#Resources
#https://mathmarecol.github.io/SpatialPlanning_Workshop2021/index.html
#https://marxansolutions.org/
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
rm(list=ls())
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
#read wio land to eez shapefile file

source("makegrid.R")
roi.wio.alb<-readOGR(dsn='~/Documents/tbca/PlanningUnits/WIO_PUfile_base_Sept2021/','wio_country_land to eez')

wio_landtoshelf <- subset(roi.wio.alb, FID_wio_la == 0)
wio_slope <- subset(roi.wio.alb, FID_wio_la == 1)
wio_eez <- subset(roi.wio.alb, FID_wio_la == 2)

#select the column of the attribute table that will determine the split of the shp
uniques <- unique(wio_landtoshelf$COUNTRY2)

#create country pus based on the determined column
output<- list()
for (i in 1:length(uniques)) {
  cntry <- wio_landtoshelf[wio_landtoshelf$COUNTRY2 == uniques[i], ]
  output[i]<- make_grid(cntry, type = "hexagonal", cell_area = 5000000, clip = TRUE)
}

wiopu_landtoshelf = do.call(bind, output)

# Extract polygon/p ID's
#pid.shelf <- sapply(slot(hex_grid_shelf, "polygons"), function(x) slot(x, "ID")) 
pid.eez <- sapply(slot(allwiopu, "polygons"), function(x) slot(x, "ID")) 

# Create dataframe with correct rownames
#p.df.shelf <- data.frame( ID=1:length(hex_grid_shelf), row.names = pid.shelf)    
p.df.eez <- data.frame( ID=1:length(allwiopu), row.names = pid.shelf ))

# Try coersion again and check class
#pu.shelf <- SpatialPolygonsDataFrame(hex_grid_shelf, p.df.shelf)
pu.eez <- SpatialPolygonsDataFrame(allwiopu, p.df.eez)
class(pu.eez) 

##please write Large data to (~/volumes/Data)
#writeOGR(obj=pu.shelf,dsn='~/Documents/tbca/PlanningUnits', layer="wioShelf_1km", driver="ESRI Shapefile",overwrite_layer=TRUE)
writeOGR(obj=map2,dsn='~/Documents/tbca/PlanningUnits', layer="slope.pu_2.5km", driver="ESRI Shapefile",overwrite_layer=TRUE)


map1<-readOGR(dsn='~/Documents/tbca/PlanningUnits', "eez.pu_5km")
map2<-readOGR(dsn='~/Documents/tbca/PlanningUnits', "slope.pu_2.5km")
map3<-readOGR(dsn='~/Documents/tbca/PlanningUnits', "land.shelf.pu_1km")

allwiopu<-rbind(map1,map2, map3)

allwiopu <- sp::spChFIDs(allwiopu, as.character(1:length(allwiopu)))

pid.allwiopu <- sapply(slot(allwiopu, "polygons"), function(x) slot(x, "ID")) 
p.df.allwiopu <- data.frame( ID=1:length(allwiopu), row.names = pid.allwiopu )
allwiopu<- SpatialPolygonsDataFrame(allwiopu, p.df.allwiopu)

writeOGR(obj=allwiopu,dsn='~/Documents/tbca/PlanningUnits', layer="wioMultilevelPu", driver="ESRI Shapefile",overwrite_layer=TRUE)

#wio.shelf<-readOGR(dsn='~/Documents/tbca/Cleaned/WIO_Geomorphic/','Shelf')
#wio.slope<-readOGR(dsn='~/Documents/tbca/Cleaned/WIO_Geomorphic/','Slope')
#WIOinland<-readOGR(dsn='~/Documents/tbca/PlanningUnits/WIO_PUfile_base/','WIO_EEZ_Inland_fin')
##select countries of interest from eez and delete columns
#eez<-unique(wio.eez@data$Territory1)
#non.wio<-c("Djibouti","Maldives","Chagos Archipelago")
#wio<-eez[!eez %in% non.wio]
#wio.roi <- subset(wio.eez, Territory1 %in% wio)
#cols<-names(wio.roi)
#filtercols<-cols[!cols %in% c("OBJECTID","Territory1")]
#roi.wio <- wio.roi[,!(names(wio.roi) %in% filtercols)] 

#Project to Afica_Albers
#afr.alb<-CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

#roi.wio.alb<- spTransform(wio.eez, afr.alb)


#create planning units of 1km2
# hex - without clipping
#hex_grid_shelf <- make_grid(wio.shelf, type = "hexagonal", cell_area = 1000000, clip = FALSE)
#hex_grid_shelf <- make_grid(wio.shelf, type = "hexagonal", cell_area = 1000000, clip = TRUE)
#hex_grid_eez <- make_grid(roi.wio.alb, type = "hexagonal", cell_area = 25000000, clip = TRUE)

#plot(roi.wio.alb, col = "grey50", bg = "light blue", axes = FALSE)
#plot(hex_grid, border = "orange", add = TRUE)
#box()
# hex - with clipping
#hex_grid_c <- make_grid(roi.wio.alb, type = "hexagonal", cell_area = 1000000, clip = TRUE)
#plot(roi.wio.alb, col = "grey50", bg = "light blue", axes = FALSE)
#plot(hex_grid_c, border = "orange", add = TRUE)
#box()
#writeOGR(obj=pu.eez,dsn='~/Documents/tbca/PlanningUnits', layer="wioEEZ_5km", driver="ESRI Shapefile",overwrite_layer=TRUE)
rm(list=ls())



####tbca starts here
##tbca dataprep
#Sept 14
##readin the shapefile
setwd("~/Documents/tbca/wiompan_mpa/")

#read in plannimng units
tbca.pu<-readOGR(dsn='~/Documents/tbca/PlanningUnits/TBCA_PU_file_8Jun2022/','tbca_pu_file_country_empty')
tbca.pu@data$PU<-rownames(tbca.pu@data)#add PU ID

#plot(tbca.pu)
files<-list.files(path='~/Documents/tbca/Data/Cleaned/WIO_Geomorphic/', pattern='shp$')
#delete shelf classification
#files<-files[-14]
#files<-files[-3]
#files<-files[-4]
hc<-list()
pi<-list()
hc<-list()
sh<-list()
outpout<- list()
for (i in seq_along(files)){
  tryCatch({
  sh[[i]]<-readOGR(dsn='~/Documents/tbca/Data/Cleaned/WIO_Geomorphic/',tools::file_path_sans_ext(files[i]))
  hc[[i]]<-raster::crop(sh[[i]], extent(tbca.pu))
  pi[[i]] <- raster::intersect( hc[[i]], tbca.pu)
  pi[[i]]$area <- area(pi[[i]]) / 1000000
  outpout[[i]]<-aggregate(area~PU+ Geomorphic, data=pi[[i]]@data, FUN=sum)
  #write.csv(outpout[[i]],paste('/Users/josephmaina/OneDrive - Macquarie University/Projects/,files[i],'.csv'))		
  }, error=function(e){})
} 

puvspr<-do.call(rbind,outpout)
colnames(puvspr)<-c('pu','species','amount')
r.seafloor = rle(puvspr$species)
puvspr$species.id <- rep(seq_along(r.seafloor$lengths), r.seafloor$lengths)
puvspr<-puvspr[order(puvspr$pu),]
puvspr.seasfloor.id<-puvspr[,c('species.id','pu','amount','species')]
#colnames(puvspr.seasfloor.id)[1]<-'species'
write.csv(puvspr.seasfloor.id,'~/Documents/tbca/wiompan_mpa/tbca.planning/puvspr.seafloor.csv')

rm(list=ls())
###ALLEN ATLAS##
#read in plannimng units
tbca.pu<-readOGR(dsn='~/Documents/tbca/PlanningUnits/TBCA_PU_file_8Jun2022/','tbca_pu_file_country_empty')
tbca.pu@data$PU<-rownames(tbca.pu@data)#add PU ID

##extract coral seagass and reef geomorphology totbca
#split allen atlas geomorphic into different shapefiles
geomorphic.allen<-readOGR(dsn='~/Documents/tbca/Data/Cleaned/WIO_Geomorphic_Allen/','geomorphic')
names(geomorphic.allen)
unique.ga <- unique(geomorphic.allen@data$class)
#unique.ga
#[1] "Inner Reef Flat"       "Sheltered Reef Slope"  "Outer Reef Flat"       "Back Reef Slope"       "Reef Slope"            "Shallow Lagoon"       
#[7] "Plateau"               "Deep Lagoon"           "Terrestrial Reef Flat" "Reef Crest"          
hc<-list()
pi<-list()
hc<-list()
sh<-list()
outpout<- list()
for (i in 1:length(unique.ga)) {
  sh[[i]] <- geomorphic.allen[geomorphic.allen$class == unique.ga[i], ] 
  hc[[i]]<-raster::crop(sh[[i]], extent(tbca.pu))
  pi[[i]] <- raster::intersect( hc[[i]], tbca.pu)
  pi[[i]]$area <- area(pi[[i]]) / 1000000
  outpout[[i]]<-aggregate(area~PU+ class, data=pi[[i]]@data, FUN=sum)
}

puvspr<-do.call(rbind,outpout)
colnames(puvspr)<-c('pu','species','amount')
r.ga = rle(puvspr$species)
puvspr$species.id <- rep(seq_along(r.ga$lengths), r.ga$lengths)
puvspr<-puvspr[order(puvspr$pu),]
puvspr.ga.id<-puvspr[,c('species.id','pu','amount','species')]
write.csv(puvspr.ga.id,'~/Documents/tbca/wiompan_mpa/tbca.planning/puvspr.reefgeomo.allen.csv')
rm(list=ls())


#######
##Coral and Seagrass
#read in plannimng units
tbca.pu<-readOGR(dsn='~/Documents/tbca/PlanningUnits/TBCA_PU_file_8Jun2022/','tbca_pu_file_country_empty')
tbca.pu@data$PU<-rownames(tbca.pu@data)#add PU ID

#plot(tbca.pu)
files<-list.files(path='~/Documents/tbca/Data/Cleaned/WIO_Seagrass_Allen_1Jun2022', pattern='shp$')
dat<-readOGR(dsn="/Users/maina/Documents/tbca/Data/Cleaned/WIO_Seagrass_Allen_1Jun2022",tools::file_path_sans_ext(files[1]))#change to 1 or 2 for coral and seagrass rspectively
dat1<-raster::crop(dat, extent(tbca.pu))
#wio_eez_seagrass

#convert to sf due to large memory##https://stackoverflow.com/questions/45128670/combining-spatialpointsdataframe-with-spatialpolygonsdataframe-error-maximum-re
dat.sf<-st_as_sf(dat1)
tbca.sf<-st_as_sf(tbca.pu)

#check th lengths of levels omn category2 field in coral data. N per levcel seems sufficient. now use to breakup the data
#dat1@data %>% 
#  group_by(CATEGORY2) %>%
#  summarise(no_rows = length(CATEGORY2))

##run intersection of the cropped extent
#dat2 <- dat.sf%>%
#  group_by(CATEGORY2) %>%
#  do(sf::st_intersection(., tbca.sf))

dat2 <- st_intersection(dat.sf, tbca.sf)

dat3<-st_as_sf(dat2)
dat3$area <- sf::st_area(dat3)/1000000
puvspr<-aggregate(area~PU+ class, data=dat3, FUN=sum)

colnames(puvspr)<-c('pu','species','amount')
dat.ga = rle(puvspr$species)
puvspr$species.id <- rep(seq_along(dat.ga$lengths), dat.ga$lengths)
puvspr<-puvspr[order(puvspr$pu),]
puvspr.id<-puvspr[,c('species.id','pu','amount','species')]
write.csv(puvspr.id,'~/Documents/tbca/wiompan_mpa/tbca.planning/puvspr.seagrass.allen.csv')
rm(list=ls())


#pending layers
#gravity of markkets
#climate change
#socioeconomic data
##################
#Priortize R: tbca
##################

library(prioritizr)
# set default options for printing tabular data
options(tibble.width = Inf)

##Create puvspr_dat table
##createspec_dat table
##load the species/area dataframes
library(data.table)
setwd("/Users/maina/Documents/tbca/wiompan_mpa/tbca.planning/")
temp = list.files(path="/Users/maina/Documents/tbca/wiompan_mpa/tbca.planning/",pattern="*.csv")
sppList = lapply(temp[2:4], read.csv)
puvspr_dat<-do.call(rbind,sppList)
puvspr_dat$species<-as.factor(puvspr_dat$species)
spec_dat<-data.frame(as.vector(levels(puvspr_dat$species)), seq(1:15),0.3,1)
spec_dat<-spec_dat[,c(2,3,4,1)]
colnames(spec_dat)<-c("id","prop","spf","name")
puvspr_dat <-puvspr_dat %>% mutate(species = species %>% as.factor() %>% as.numeric())
id<-unique(puvspr_dat$species)
puvspr_dat<-puvspr_dat[,c("species","pu","amount")]
head(puvspr_dat)
write.csv(puvspr_dat,"puvspr_dat.csv")
write.csv(spec_dat,"spec_dat.csv")





tbca.pu<-readOGR(dsn='~/Documents/tbca/PlanningUnits/TBCA_PU_file_shared_8Oct2021/','tbca_pu_empty_250m_country')

# load polygon planning unit data
data(sim_pu_polygons)




head(sim_pu_polygons@data)























