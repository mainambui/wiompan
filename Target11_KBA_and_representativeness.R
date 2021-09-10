#KBA areas - Key Biodiversity areas  (IUCN)
#First, to determine KBA area within EEZ by country
library(here)
library(dplyr)
library(tidyverse)
# KBA and EEZ layers were merged in QGis - Polygons intercepting were considered within EEZ boundaries
#note all KBAs identify either within or intercepting EEZ were considered.
KBA_EEZ <- read.csv(here("_data_Target11","KBA_EEZ.csv"))
summary(KBA_EEZ)
#load dataset with the area pf KBAs protected (intersection layer)
KBA_MPA_int<-read.csv(here("_data_Target11","KBA_Area_protected.csv"))
summary(KBA_MPA_int)
#load data KBA area within EEZ by Country
KBA_EEZ_int <- read.csv(here("_data_Target11","EEZ_KBA_intersection.csv"))
summary(KBA_EEZ_int)
#meters to Km2
KBA_EEZ$area_KM2_KBA <- KBA_EEZ$area_meters_KBA/1000000
KBA_MPA_int$area_KM2_KBA_Protected <- KBA_MPA_int$area_m2_int/1000000
KBA_EEZ_int$area_KM2_int <- KBA_EEZ_int$area_int_EEZ_KBA/1000000
#GENERATE % SUMMARY by Territory
unique(KBA_EEZ$Sovereign) #10 territories/ 1 dispute
length(unique(KBA_EEZ$SitRecID)) #300 KBAs within EEZ - use SitRecID
length(unique(KBA_MPA_int$SitRecID)) #109 KBAs within EEZ
#KBA area
areaKBAs<-as.data.frame(aggregate(area_KM2_KBA ~ SitRecID + Country + Sovereign, KBA_EEZ, max))
areaKBAs_Protected<-as.data.frame(aggregate(area_KM2_KBA_Protected ~ SitRecID + Country, KBA_MPA_int,sum))
areaKBA_Summary<-left_join(areaKBAs, areaKBAs_Protected, by=c("SitRecID","Country"),all=T)
areaKBA_Summary[is.na(areaKBA_Summary)] <- 0
head(areaKBA_Summary)
head(KBA_EEZ_int[Sovereign,])

int_KBAEEZarea <- as.data.frame(KBA_EEZ_int[,c(19,21)] %>%
                                  group_by(Sovereign) %>%
                                  summarise_all(list(sum)))
#Summary - total KBA area protected by Country
final_summary_by_Country <- as.data.frame(areaKBA_Summary[,3:5] %>%
  group_by(Sovereign) %>%
  summarise_all(list(sum)))

final_summary_by_Country <- left_join(final_summary_by_Country,int_KBAEEZarea,by='Sovereign')
final_summary_by_Country$area_KM2_int <- round(final_summary_by_Country$area_KM2_int, 3)

final_summary_by_Country$percent_protected_within <- round((final_summary_by_Country$area_KM2_KBA_Protected/final_summary_by_Country$area_KM2_int)*100, 2)
final_summary_by_Country$percent_protected <- round((final_summary_by_Country$area_KM2_KBA_Protected/final_summary_by_Country$area_KM2_KBA)*100, 2)

#turtle nest

