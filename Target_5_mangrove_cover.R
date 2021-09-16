#Target 5 
#By 2020, the rate of loss of all natural habitats, including forests, 
#is at least halved and where feasible brought close to zero, and degradation 
#and fragmentation is significantly reduced.

#Here, we used temporal comparison of magrove cover between 2010 and 2016 to
#estimate whether the loss of a key habitat within MPAs was lower compared outside
#MPAs 

#Dataset - note
#In QGIS, we intersected manrgove cover by year and MPA and
#EEZ/Coastal and calculated the area.


library(here)
library(ggplot2)
library(cowplot)
library(dplyr)

dir()
mangrove2010MPA <- read.csv(here("_data_Target5","Intersection_Mangrove_2010_MPA.csv"))
mangrove2016MPA <- read.csv(here("_data_Target5","Intersection_mangrove_MPA_2016.csv"))

head(mangrove2010MPA)
head(mangrove2016MPA)

pos<-aggregate(int_aream2_2016 ~ COUNTRY, mangrove2016MPA[mangrove2016MPA$STATUS == "Working",], sum)
pre <- aggregate(int_2010_aream2 ~ COUNTRY, mangrove2010MPA[mangrove2010MPA$STATUS == "Working",], sum) 

tempmangrove<-left_join(pos,pre)
tempmangrove$delta <- 
