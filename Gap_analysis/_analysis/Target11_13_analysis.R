#Luisa Fontoura - Aug 2021 - Last update 30 Oct 2021

##Target 11 and 13

#Target 13 - Proxy of strategies implemented to minimize genetic erosion and 
#safeguard genetic diversity: Protection of areas key for species reproduction 
#and offspring survivor to support population persistence. 
#Target 11 - We applied the Key Biodiversity Areas (IUCN) database as a proxy for areas 
#of particular importance for biodiversity. Mangroves, coral reefs, seagrass and seamounts 
#were considered areas of particular importance for ecosystem services and habitat 
#representativeness. Furthermore, coral reefs acting as sinks and sources of larval subsidies 
#were also considered key areas for sustaining ecosystem services (e.g. fishery yields, REF). 
#To assess whether the current system of marine protected areas (MPAs) is safeguarding marine 
#connectivity, we a) evaluated the conservation status of potential reef fish nurseries and 
#fish larval sources of currently protected coral reef areas

#libraries
library(here)
library(dplyr)
library(tidyverse)
library(dplyr)
library(sna)
library(igraph)
library(here)
library(ggpubr)
library(reshape)
library(stats)

#Target 11 - KBA areas - Key Biodiversity areas  (IUCN)----

#First, to determine KBA area within EEZ by country
# KBA and EEZ layers were merged in QGis - Polygons intercepting were considered within EEZ boundaries
#note all KBAs identify either within or intercepting EEZ were considered.
KBA_EEZ <- read.csv(here("_dataTargets11_13","KBA_EEZ.csv"))
summary(KBA_EEZ)
#load dataset with the area pf KBAs protected (intersection layer)
KBA_MPA_int<-read.csv(here("_dataTargets11_13","KBA_Area_protected.csv"))
summary(KBA_MPA_int)
#load data KBA area within EEZ by Country
KBA_EEZ_int <- read.csv(here("_dataTargets11_13","EEZ_KBA_intersection.csv"))
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
prem_KBA_summary<-final_summary_by_Country[!final_summary_by_Country$Sovereign == "South Africa",] #NOTE - South africa KBA data is missing // 

#WIO -  Regional % of coastal KBAs protected
sum(prem_KBA_summary$area_KM2_KBA_Protected)/sum(prem_KBA_summary$area_KM2_KBA)*100



#Target 11 - Protection of Habitats and EEZ Area - Mangroves, Seagrass and Coral reefs ----

#read files
seagrass <- read.csv(here("_dataTargets11_13","seagrass_EEZ_ID.csv"))
coralreefs <- read.csv(here("_dataTargets11_13","WIO_coralreefs_area_F.csv"))
mangrove <- read.csv(here("_dataTargets11_13","WIO_mangrove_area.csv"))
seamount <- read.csv(here("_dataTargets11_13","WIO_seamounts_points.csv"))

mangrove$COUNTRY <- ifelse(mangrove$CTRY == "COM", "Comoros",
                           ifelse(mangrove$CTRY == "MYT", "France",
                                  ifelse(mangrove$CTRY == "MDG", "Madagascar",
                                         ifelse(mangrove$CTRY == "MOZ", "Mozambique",
                                                ifelse(mangrove$CTRY == "KEN", "Kenya",
                                                       ifelse(mangrove$CTRY == "TZA", "Tanzania",
                                                              ifelse(mangrove$CTRY == "ZAF", "South Africa",
                                                                     ifelse(mangrove$CTRY == "SYC", "Seychelles",
                                                                            ifelse(mangrove$CTRY == "SOM", "Somalia",NA)))))))))

#Mangrove #filter by unique mangrove ID to build total area by country (summary)
filtermangrove<- mangrove[!duplicated(mangrove$ID), ]
#dim(filtermangrove) #ok
mangrove_summary <- as.data.frame(filtermangrove[,c("mangrove_aream2","COUNTRY")] %>%
                                    group_by(COUNTRY) %>%
                                    summarise_all(list(sum)))


#filter mangrove protected area
protected_mangroves_MPA <- mangrove[mangrove$CATEGORY2 == "National",]
protected_mangroves_MPA <- protected_mangroves_MPA[protected_mangroves_MPA$DESIG == "" & protected_mangroves_MPA$CATEGORY2 == "National",]
dim(protected_mangroves_MPA) #2369 mangrove (polygons) that are exclusively protected by marine protected areas
protected_mangroves_MPA <- protected_mangroves_MPA[!duplicated(protected_mangroves_MPA$ID), ]
#ok no duplicated 
#remove duplicate ID 

mangrove_summary
mangrove_summary_MPA <- as.data.frame(protected_mangroves_MPA[,c("mangrove_aream2","COUNTRY")] %>%
                                        group_by(COUNTRY) %>%
                                        summarise_all(list(sum)))


#filtering to avoid double count of area
protected_mangroves <- mangrove[!mangrove$ID %in% protected_mangroves_MPA$ID,] #remove mangroves protected only by MPAs
protected_mangroves<- protected_mangroves[protected_mangroves$IUCN %in% c("II","VI","V","VI"),] #filter by IUCN categories 
protected_mangroves <- protected_mangroves[!duplicated(protected_mangroves$ID),] #remove duplicates 

mangrove_summary_PA <- as.data.frame(protected_mangroves[,c("mangrove_aream2","COUNTRY")] %>%
                                       group_by(COUNTRY) %>%
                                       summarise_all(list(sum)))

colnames(mangrove_summary) <- c("Country","TOT_area_m2")
colnames(mangrove_summary_MPA) <- c("Country", "MPA_area_m2")
colnames(mangrove_summary_PA) <- c("Country", " PA_area_m2")

mangrove_final <- merge(mangrove_summary, mangrove_summary_MPA, all=T)
mangrove_final <- merge(mangrove_final,mangrove_summary_PA,all=T)
mangrove_final[is.na(mangrove_final)] <- 0
mangrove_final$TOT_proc_area_m2 <- mangrove_final$MPA_area_m2 + mangrove_final$` PA_area_m2`
mangrove_final$percent_prot <- (mangrove_final$TOT_proc_area_m2/mangrove_final$TOT_area_m2)*100
#WIO 
sum(mangrove_final$TOT_proc_area_m2)/sum(mangrove_final$TOT_area_m2)*100

#Seagrass
head(seagrass)
seagrass$COUNTRY <- ifelse(seagrass$Sovereign == "", seagrass$Territory1,seagrass$Sovereign)

seagrass_summary <- as.data.frame(seagrass[,c("area_sqkm","COUNTRY")] %>%
                                    group_by(COUNTRY) %>%
                                    summarise_all(list(sum)))

protected_seagrass <- seagrass[seagrass$CATEGORY2 == "National",]

seagrasss_protected_summary <- as.data.frame(protected_seagrass[,c("area_sqkm","COUNTRY")] %>%
                                               group_by(COUNTRY) %>%
                                               summarise_all(list(sum)))

seagrass_summary_final <- merge(seagrass_summary,seagrasss_protected_summary, by="COUNTRY",all=T)

colnames(seagrass_summary_final) <- c("COUNTRY","TOT_area_km2","TOT_prot_area_km2")
seagrass_summary_final <- seagrass_summary_final[!seagrass_summary_final$COUNTRY %in% c("","Disputed"),]

seagrass_summary_final$COUNTRY <- ifelse(seagrass_summary_final$COUNTRY %in% "Comoro Islands","Comoros",seagrass_summary_final$COUNTRY)
seagrass_summary_final$percent_prot <- (seagrass_summary_final$TOT_prot_area_km2/seagrass_summary_final$TOT_area_km2)*100
seagrass_summary_final[is.na(seagrass_summary_final)] <-0

#WIO
sum(seagrass_summary_final$TOT_prot_area_km2)/sum(seagrass_summary_final$TOT_area_km2)*100

#Coral reefs
coralreefs$COUNTRY <- ifelse(coralreefs$Sovereign == "", coralreefs$COUNTRY_2,coralreefs$Sovereign)

coralreefs_summary <- as.data.frame(coralreefs[,c("reef_aream2","COUNTRY")] %>%
                                      group_by(COUNTRY) %>%
                                      summarise_all(list(sum)))

protected_coralreefs <- coralreefs[coralreefs$CATEGORY2 == "National",]

coralreefss_protected_summary <- as.data.frame(protected_coralreefs[,c("reef_aream2","COUNTRY")] %>%
                                                 group_by(COUNTRY) %>%
                                                 summarise_all(list(sum)))

coralreefs_summary_final <- merge(coralreefs_summary,coralreefss_protected_summary, by="COUNTRY",all=T)

colnames(coralreefs_summary_final) <- c("COUNTRY","TOT_area_m2","TOT_prot_area_m2")
coralreefs_summary_final <- coralreefs_summary_final[!coralreefs_summary_final$COUNTRY %in% c("","Disputed"),]

coralreefs_summary_final$COUNTRY <- ifelse(coralreefs_summary_final$COUNTRY %in% "Comoro Islands","Comoros",coralreefs_summary_final$COUNTRY)
coralreefs_summary_final$percent_prot <- (coralreefs_summary_final$TOT_prot_area_m2/coralreefs_summary_final$TOT_area_m2)*100
coralreefs_summary_final[is.na(coralreefs_summary_final)] <-0

#WIO
sum(coralreefs_summary_final$TOT_prot_area_m2)/sum(coralreefs_summary_final$TOT_area_m2)*100


#Seamounts
head(seamount)
dim(seamount) #1516 seamounts identified in the region
length(unique(seamount$PEAKID)) #ok

seamount_summary <- as.data.frame(seamount[,c("Sovereign","PEAKID")] %>%
                                    group_by (Sovereign) %>%
                                    summarise(n=n()))

seamount_protected <- seamount[seamount$CATEGORY2 == "National",]

seamount_protected_summary <- as.data.frame(seamount_protected [,c("Sovereign","PEAKID")] %>%
                                              group_by (Sovereign) %>%
                                              summarise(n=n()))


seamount_summary_final <- merge(seamount_summary,seamount_protected_summary,all=T, by="Sovereign")
colnames(seamount_summary_final) <- c("Country","n","n_protected")
seamount_summary_final[is.na(seamount_summary_final)] <-0
seamount_summary_final$perc_protected<- (seamount_summary_final$n_protected/ seamount_summary_final$n)*100
seamount_summary_final<-seamount_summary_final[!seamount_summary_final$Country == "",]
sum(seamount_summary_final$n_protected)/sum(seamount_summary_final$n)*100

#EEZ protected (MPA area)
mpa <- read.csv(here("_dataTargets11_13","WIO_MPA_area.csv"))
head(mpa)

eez_summary <- as.data.frame(mpa[,c("area_EEZ","Sovereign")] %>%
                               group_by(Sovereign) %>%
                               summarise_all(list(min)))

eez_summary$Sovereign <- ifelse(eez_summary$Sovereign == "Comoro Islands", "Comoros",eez_summary$Sovereign)
eez_summary <- eez_summary[!eez_summary$Sovereign == "",]
colnames(eez_summary) <- c("COUNTRY","EEZ_area_m2")

protected_eez <- mpa[mpa$CATEGORY2 %in% c("National","Transboundary") & mpa$STATUS == "Working",]

eez_protected_summary <- as.data.frame(protected_eez[,c("area_mpa","COUNTRY")] %>%
                                         group_by(COUNTRY) %>%
                                         summarise_all(list(sum)))

eez_summary_final <- merge(eez_summary,eez_protected_summary,by="COUNTRY",all=T)
eez_summary_final$perct_protected <- (eez_summary_final$area_mpa/eez_summary_final$EEZ_area_m2)*100
sum(eez_summary_final$area_mpa)/sum(eez_summary_final$EEZ_area_m2)*100 #21.9 


#Target 11 and 13 - Larval connectivity # Sources, Sinks (11) and Dispersal corridors (13) ----

#1.1 Identifying the percentage of top larval sources and sinks protected across Countries 

#EEZ Connectivity reef cells (dataset built in QGIS - overlaying Connectivity.shp and WIO_EEZ.shp)
EEZ_CON<-read.csv(here("_dataTargets11_13","EEZ_reef_cells_con.csv"))
EEZ_CON <- EEZ_CON %>% filter(!is.na(ID_2))
dim(EEZ_CON) #750
#unique(EEZ_CON$Sovereign) #Territories
#total number of reef cells by Country
Summary_CON <- as.data.frame(EEZ_CON %>% 
                               group_by(Sovereign) %>%
                               summarise(n = n()))
#sum(Summary_CON$n) #750 reef cells

#MPAs Connectivity reef cells within MPAs (buffered radius 4km) (dataset build in QGIS - overlaying connectivity.shp using buffer 4km and MPa.shp)
MPA_Con_B<- read.csv(here("_dataTargets11_13","MPA_Connectivity_Overlay.csv")) #8km buffer
EEZ_CON$protection <- ifelse(EEZ_CON$ID_2 %in% MPA_Con_B$ID, "protected", "fished")
dim(EEZ_CON[EEZ_CON$protection == "protected",]) #295
#
Summary_CON_MPA <- as.data.frame(EEZ_CON[EEZ_CON$protection == "protected",] %>% 
                                   group_by(Sovereign) %>%
                                   summarise(n = n()))

Summary_CON <- left_join(Summary_CON,Summary_CON_MPA,by="Sovereign",all=T)
colnames(Summary_CON) <- c("Sovereign", "Total", "Protected")

#Top sinks ans sources protected
EEZ_CON_SoSi<-  EEZ_CON %>% filter(!is.na(NetflowC5))
droplevels(EEZ_CON_SoSi)
dim(EEZ_CON_SoSi) #706 reef cells
#
#total cells and % protected
length(EEZ_CON_SoSi[EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi$ID_2)*100 # 39.9%


#protection Sinks WIO
length(EEZ_CON_SoSi[EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi$NetflowC5,0.25,na.rm = T) &
                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi$NetflowC5,0.25,na.rm = T),]$ID_2)*100


SinksPA <- c( length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Comoro Islands" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Comoro Islands",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Comoro Islands" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Comoro Islands",]$NetflowC5,0.25,na.rm = T),]$ID_2)*100,
              
              length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "France" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "France",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "France" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "France",]$NetflowC5,0.25),]$ID_2)*100,
              
              length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Kenya" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Kenya",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Kenya" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Kenya",]$NetflowC5,0.25,na.rm = T),]$ID_2)*100,
              
              length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Madagascar" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Madagascar",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Madagascar" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Madagascar",]$NetflowC5,0.25,na.rm = T),]$ID_2)*100,
              
              length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mauritius" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mauritius",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mauritius" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mauritius",]$NetflowC5,0.25,na.rm = T),]$ID_2)*100,
              
              length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mozambique" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mozambique",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mozambique" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mozambique",]$NetflowC5,0.25,na.rm = T),]$ID_2)*100,
              
              length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Seychelles" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Seychelles",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Seychelles" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Seychelles",]$NetflowC5,0.25,na.rm = T),]$ID_2)*100,
              
              length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Somalia" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Somalia",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Somalia" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Somalia",]$NetflowC5,0.25,na.rm = T),]$ID_2)*100,
              
              length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "South Africa" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "South Africa",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "South Africa" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "South Africa",]$NetflowC5,0.25,na.rm = T),]$ID_2)*100,
              
              length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Tanzania" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Tanzania",]$NetflowC5,0.25,na.rm = T) &
                                    EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Tanzania" & EEZ_CON_SoSi$NetflowC5 < quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Tanzania",]$NetflowC5,0.25,na.rm = T),]$ID_2)*100 )

#protection SourcesWIO 
length(EEZ_CON_SoSi[EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi$NetflowC5,0.75,na.rm = T) &
                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi$NetflowC5,0.75,na.rm = T),]$ID_2)*100


SourcesPA <- c( length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Comoro Islands" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Comoro Islands",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Comoro Islands" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Comoro Islands",]$NetflowC5,0.75,na.rm = T),]$ID_2)*100,
                
                length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "France" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "France",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "France" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "France",]$NetflowC5,0.75),]$ID_2)*100,
                
                length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Kenya" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Kenya",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Kenya" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Kenya",]$NetflowC5,0.75,na.rm = T),]$ID_2)*100,
                
                length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Madagascar" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Madagascar",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Madagascar" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Madagascar",]$NetflowC5,0.75,na.rm = T),]$ID_2)*100,
                
                length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mauritius" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mauritius",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mauritius" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mauritius",]$NetflowC5,0.75,na.rm = T),]$ID_2)*100,
                
                length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mozambique" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mozambique",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mozambique" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Mozambique",]$NetflowC5,0.75,na.rm = T),]$ID_2)*100,
                
                length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Seychelles" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Seychelles",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Seychelles" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Seychelles",]$NetflowC5,0.75,na.rm = T),]$ID_2)*100,
                
                length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Somalia" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Somalia",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Somalia" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Somalia",]$NetflowC5,0.75,na.rm = T),]$ID_2)*100,
                
                length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "South Africa" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "South Africa",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "South Africa" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "South Africa",]$NetflowC5,0.75,na.rm = T),]$ID_2)*100,
                
                length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Tanzania" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Tanzania",]$NetflowC5,0.75,na.rm = T) &
                                      EEZ_CON_SoSi$protection == "protected",]$ID_2)/ length(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Tanzania" & EEZ_CON_SoSi$NetflowC5 > quantile(EEZ_CON_SoSi[EEZ_CON_SoSi$Sovereign == "Tanzania",]$NetflowC5,0.75,na.rm = T),]$ID_2)*100 )

summary(EEZ_CON)
EEZ_CON_Cor<-  EEZ_CON %>% filter(OutflowP5 > 0 & InflowP5  > 0)
dim(EEZ_CON_Cor) #701 corridors

#protection CorridorsWIO 
length(EEZ_CON_Cor[EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor$InflowP5,0.75,na.rm = T) & EEZ_CON_Cor$protection == "protected",]$ID_2)/length(EEZ_CON_Cor[EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor$InflowP5,0.75,na.rm = T),]$ID_2)*100


CorridorsPA <- c( length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Comoro Islands" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Comoro Islands",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Comoro Islands" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Comoro Islands",]$InflowP5,0.75,na.rm = T),]$ID_2)*100,
                  
                  length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "France" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "France",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "France" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "France",]$InflowP5,0.75),]$ID_2)*100,
                  
                  length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Kenya" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Kenya",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Kenya" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Kenya",]$InflowP5,0.75,na.rm = T),]$ID_2)*100,
                  
                  length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Madagascar" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Madagascar",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Madagascar" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Madagascar",]$InflowP5,0.75,na.rm = T),]$ID_2)*100,
                  
                  length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Mauritius" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Mauritius",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Mauritius" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Mauritius",]$InflowP5,0.75,na.rm = T),]$ID_2)*100,
                  
                  length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Mozambique" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Mozambique",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Mozambique" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Mozambique",]$InflowP5,0.75,na.rm = T),]$ID_2)*100,
                  
                  length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Seychelles" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Seychelles",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Seychelles" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Seychelles",]$InflowP5,0.75,na.rm = T),]$ID_2)*100,
                  
                  length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Somalia" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Somalia",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Somalia" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Somalia",]$InflowP5,0.75,na.rm = T),]$ID_2)*100,
                  
                  length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "South Africa" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "South Africa",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "South Africa" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "South Africa",]$InflowP5,0.75,na.rm = T),]$ID_2)*100,
                  
                  length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Tanzania" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Tanzania",]$InflowP5,0.75,na.rm = T) &
                                       EEZ_CON_Cor$protection == "protected",]$ID_2)/ length(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Tanzania" & EEZ_CON_Cor$InflowP5 > quantile(EEZ_CON_Cor[EEZ_CON_Cor$Sovereign == "Tanzania",]$InflowP5,0.75,na.rm = T),]$ID_2)*100 )


Summary_CON$SinkPA <- SinksPA
Summary_CON$SourcePA <- SourcesPA
Summary_CON$CorridorPA <- CorridorsPA


#1.2. Identifying whether main larval sources from sinks located inside MPAs are protected (%)

WIO_Graph<-read.csv(here("_dataTargets11_13", "crypto5matrixWIO.csv"))
dim(WIO_Graph)
WIO_Graph<-as.matrix(WIO_Graph[,2:1908])
rownames(WIO_Graph) <- colnames(WIO_Graph)
WIOT<-melt(WIO_Graph)
head(WIOT)
colnames(WIOT) <- c("From","To","Prob")
WIOFi<-WIOT[WIOT$Prob>0,]
head(WIOFi)
WIO_EEZ_edgelist <- WIOFi %>% filter(To %in% EEZ_CON$NA_) #filtering by WIO sinks (sinks here defined by all reef cells importing larvae - inflow >0)

#extract the status of protection for each cell (From and To)
WIO_EEZ_edgelist_full <-WIO_EEZ_edgelist  %>%
  inner_join(EEZ_CON %>% dplyr::select(NA_, Sovereign), by = c('From' = 'NA_'), all=F) %>%
  dplyr::rename(Source_Country = Sovereign) %>%
  inner_join(EEZ_CON %>% dplyr::select(NA_, protection), by = c('From' = 'NA_'), all=F) %>%
  dplyr::rename(Source_protection = protection) %>%
  inner_join(EEZ_CON %>% dplyr::select(NA_, Sovereign), by = c('To' = 'NA_'), all=F) %>%
  dplyr::rename(Sink_Country = Sovereign) %>%
  inner_join(EEZ_CON %>% dplyr::select(NA_, protection), by = c('To' = 'NA_'), all=F) %>%
  dplyr::rename(Sink_protection = protection) 

#Filter by MPA sinks
WIO_EEZ_MPAsinks <- WIO_EEZ_edgelist_full %>% filter(Sink_protection == "protected")
head(WIO_EEZ_MPAsinks)

testando <- as.data.frame(EEZ_CON[EEZ_CON$protection == "protected",] %>% 
                            group_by(Sovereign) %>%
                            summarise(n = n()))
dim(WIO_EEZ_MPAsinks) #13994 ocurrences 
dim(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_Country == WIO_EEZ_MPAsinks$Sink_Country,])
4499/13994*100 #32% of sources from distinct countries

length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected",]$From))/
  length(unique(WIO_EEZ_MPAsinks$From))*100 #34% of sources of MPA sinks are protected

PAsource_ofsinkMPA <- c( 
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "Comoro Islands",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "Comoro Islands",]$From))*100,
  
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "France",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "France",]$From))*100,
  
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "Kenya",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "Kenya",]$From))*100,
  
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "Madagascar",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "Madagascar",]$From))*100,
  
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "Mauritius",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "Mauritius",]$From))*100,
  
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "Mozambique",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "Mozambique",]$From))*100,
  
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "Seychelles",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "Seychelles",]$From))*100,
  
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "Somalia",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "Somalia",]$From))*100,
  
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "South Africa",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "South Africa",]$From))*100,
  
  length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Source_protection == "protected" & WIO_EEZ_MPAsinks$Sink_Country == "Tanzania",]$From))/
    length(unique(WIO_EEZ_MPAsinks[WIO_EEZ_MPAsinks$Sink_Country == "Tanzania",]$From))*100)

###
Summary_CON$PA_sources_sinkMPAs <- PAsource_ofsinkMPA
#### Final table
Summary_CON
##END

##Figure - Larval connectivity 
dim(EEZ_CON) #dataset with all reefs and respective connectivity value
FIG_DATA<- left_join(EEZ_CON,MPA_Con_B[,c("ID","CATEGORY2","CATEGORY")],by=c("ID_2"="ID"), all=T)

percentile <- ecdf(FIG_DATA$NetflowC5)
FIG_DATA$NetflowC5_percentile<-percentile(FIG_DATA$NetflowT15)
percentile <- ecdf(FIG_DATA$InDegP5)
FIG_DATA$InDegP5_percentile<-percentile(FIG_DATA$InDegP5)

allRe<- ggplot(FIG_DATA, aes(y=NetflowC5_percentile, x=InDegP5_percentile)) +
  geom_point(alpha=0) + 
  geom_point(data=FIG_DATA, 
             aes(y=NetflowC5_percentile, x=InDegP5_percentile, color=CATEGORY),alpha=0.5,size=2) +theme_bw() +
  geom_hline(yintercept=0.25,linetype=2) +
  geom_hline(yintercept=0.75,linetype=2) +
  geom_vline(xintercept=0.75,linetype=2) +
  geom_vline(xintercept=0.75,linetype=2)

#Create object for graphical edition in inkscape for A - conpectual and B - results 
relatinnetfin <- ggarrange(allRe,allRe, nrow=1,common.legend = T,legend="bottom",labels = c("A","B"))





##Target 13 - Turtle nest, IBAs and Potential fish spawning locations  ----
turtle<-read.csv(here("_dataTargets11_13", "Turtle_nest_MPA.csv"))
dim(turtle) #178 turtle nests identified in the region
turtle$nest_status_protection <- ifelse(turtle$COUNTRY_2 == "", "threatened", "protected")
turtle$Country_sum<- ifelse(turtle$Country == "Mayotte", "France",
                            ifelse(turtle$Country == "Réunion", "France",
                                   ifelse(turtle$Country == "French Southern Territories","France",turtle$Country)))
#unique(turtle$Country_sum)
#No data recorded for Mauritius and Somalia
nest_protection_summary <- as.data.frame(turtle %>%
                                           group_by (Country_sum) %>%
                                           summarise(n=n()))

nest_protected_sum <- as.data.frame(turtle[turtle$nest_status_protection == "protected",] %>%
                                      group_by (Country_sum) %>%
                                      summarise(n=n()))

nest_protection_summary<-left_join(nest_protection_summary,nest_protected_sum,by="Country_sum",all=T)
colnames(nest_protection_summary) <- c("Country","Total Number Nests", "Total number Nest protected")
nest_protection_summary[is.na(nest_protection_summary)] <- 0

nest_protection_summary$percent <- nest_protection_summary$`Total number Nest protected`/nest_protection_summary$`Total Number Nests`*100
#WIO regional 
sum(nest_protection_summary$`Total number Nest protected`)/(sum(nest_protection_summary$`Total Number Nests`))*100

###IBAs protected (same data as KBa but filtered by confirmed IBAs)
IBA_EEZ <-  KBA_EEZ[KBA_EEZ$IbaStatus == "confirmed",]
IBA_MPA_int <- KBA_MPA_int[KBA_MPA_int$IbaStatus == "confirmed",]
IBA_EEZ_int <- KBA_EEZ_int[KBA_EEZ_int$IbaStatus == "confirmed",]

#GENERATE % SUMMARY by Territory
unique(IBA_EEZ$Sovereign) #9 territories/ 1 dispute
length(unique(IBA_EEZ$SitRecID)) #111 IBAs within EEZ - use SitRecID
length(unique(IBA_MPA_int$SitRecID)) #45 IBAs within EEZ
#KBA area
areaIBAs<-as.data.frame(aggregate(area_KM2_KBA ~ SitRecID + Country + Sovereign, IBA_EEZ, max))
areaIBAs_Protected<-as.data.frame(aggregate(area_KM2_KBA_Protected ~ SitRecID + Country, IBA_MPA_int,sum))
areaIBA_Summary<-left_join(areaIBAs, areaIBAs_Protected, by=c("SitRecID","Country"),all=T)
areaIBA_Summary[is.na(areaIBA_Summary)] <- 0
head(areaIBA_Summary)

int_IBAEEZarea <- as.data.frame(IBA_EEZ_int[,c(19,21)] %>%
                                  group_by(Sovereign) %>%
                                  summarise_all(list(sum)))
#Summary - total IBA area protected by Country
final_summaryIBA_by_Country <- as.data.frame(areaIBA_Summary[,3:5] %>%
                                               group_by(Sovereign) %>%
                                               summarise_all(list(sum)))

final_summaryIBA_by_Country <- left_join(final_summaryIBA_by_Country,int_IBAEEZarea,by='Sovereign')
final_summaryIBA_by_Country$area_KM2_int <- round(final_summaryIBA_by_Country$area_KM2_int, 3)

final_summaryIBA_by_Country$percent_protected_within <- round((final_summaryIBA_by_Country$area_KM2_KBA_Protected/final_summaryIBA_by_Country$area_KM2_int)*100, 2)
final_summaryIBA_by_Country$percent_protected <- round((final_summaryIBA_by_Country$area_KM2_KBA_Protected/final_summaryIBA_by_Country$area_KM2_KBA)*100, 2)
prem_IBA_summary<-final_summaryIBA_by_Country[!final_summaryIBA_by_Country$Sovereign == "South Africa",] #NOTE - South africa KBA data is missing // 

#WIO -  Regional % of coastal IBAs protected
sum(prem_IBA_summary$area_KM2_KBA_Protected)/sum(prem_IBA_summary$area_KM2_KBA)*100

#Potential spawning areas
#We used the SCRFA dataset to identify habitats where fish aggregations have been observed in the region
#We defined Reef slope and reef crest as potential spawning locations for transient fish species (e.g., large groupers)

spawningMPA<-read.csv(here("_dataTargets11_13", "MPA_geomorphic_joined.csv"))
spawningMPA <- spawningMPA[spawningMPA$class %in% c("Reef Slope","Reef Crest") & spawningMPA$STATUS %in% "Working",]
spawning_protection_summary <- as.data.frame(spawningMPA[,c("COUNTRY","area_m2")] %>%
                                               group_by (COUNTRY) %>%
                                               summarise_all(list(sum)))

spawningEEZ<-read.csv(here("_dataTargets11_13", "EEZ_geomorphic_area_joined.csv"))
spawningEEZ <- spawningEEZ[spawningEEZ$class %in% c("Reef Slope","Reef Crest"),]
spawningEEZ$Sovereign <- ifelse(spawningEEZ$Sovereign == "Comoro Islands", "Comoros", spawningEEZ$Sovereign)
spawningEEZ <- spawningEEZ[spawningEEZ$Sovereign %in% c("Comoros","France","Seychelles","Mauritius","Mozambique",
                                                        "Madagascar","Kenya","Somalia","Tanzania"),]

spawning_area_summary <- as.data.frame(spawningEEZ[,c("Sovereign","area_m2")] %>%
                                         group_by (Sovereign) %>%
                                         summarise_all(list(sum)))

colnames(spawning_area_summary) <- c("COUNTRY","total_aream2")

spawning_protection_summary <- left_join(spawning_protection_summary,spawning_area_summary,by='COUNTRY')
spawning_protection_summary$percent <- (spawning_protection_summary$area_m2/spawning_protection_summary$total_aream2)*100

#WIO regional 
sum(spawning_protection_summary$area_m2)/(sum(spawning_protection_summary$total_aream2))*100





#Target 11 and 13 - Seagrass nurseries and coral-seagrass connectivity

#Building seagrass-coral distace dataset using shapefiles
#coralreef <- st_read("/Users/louisa/Downloads/Cleaned-2/WIO_Coral_Allen/WIO_coral.shp")
#seagrass <- st_read("/Users/louisa/Downloads/Cleaned-2/WIO_Seagrass_Allen/WIO_seagrass.shp")

#a<-st_nearest_feature(coralreef,seagrass)
#Calc_distance <- st_distance(coralreef, seagrass[a,], by_element=TRUE)
#combinedCoralSeagrass = cbind(coralreef, st_drop_geometry(seagrass)[a,])
#combinedCoralSeagrass$DISTANCE <- Calc_distance
#combinedCoralSeagrass$seagrassID <- round(as.numeric(rownames(combinedCoralSeagrass)))

#seagrass_EEZ <- read.csv("/Users/louisa/Downloads/seagrass_EEZ_ID.csv")
#seagrass_EEZ$COUNTRY <- ifelse(seagrass_EEZ$Sovereign == "", seagrass_EEZ$Territory1,seagrass_EEZ$Sovereign)

###Number of seagrass beds that are likely to act as coral reef fish nurseries (8 km threshold)
#seagrasNursery <- combinedCoralSeagrass[as.numeric(combinedCoralSeagrass$DISTANCE) < 8000,]
#seagrasNursery_EEZ <- left_join(seagrasNursery, seagrass_EEZ[,c("seagrassID","COUNTRY")], by="seagrassID")

#Target 13 - Potential Seagrass nurseries of coral reef fish ----
#Seagrass nurseries  % of protection (this indicator is within Target 13 )
seagrass_nursery <- read.csv(here("_dataTargets11_13","seagrass_nurseries_rowid.csv"))
seagrassNursery<- seagrass[seagrass_nursery$seagrass_id,]
seagrassNursery$COUNTRY <- ifelse(seagrassNursery$Sovereign == "", seagrassNursery$Territory1,seagrassNursery$Sovereign)
seagrassNursery$COUNTRY <- as.factor(seagrassNursery$COUNTRY)

nursery_protection_summary <- as.data.frame(seagrassNursery %>%
                                              group_by (COUNTRY) %>%
                                              summarise(n=n()))

nursery_protected_sum <- as.data.frame(seagrassNursery[seagrassNursery$CATEGORY2 %in% "National",] %>%
                                         group_by (COUNTRY) %>%
                                         summarise(n=n()))

nursery_protection_summary<-left_join(nursery_protection_summary,nursery_protected_sum,by="COUNTRY",all=T)
colnames(nursery_protection_summary) <- c("Country","Total Number Nurseries", "Total number Nurseries protected")
nursery_protection_summary[is.na(nursery_protection_summary)] <- 0
nursery_protection_summary$percent <- nursery_protection_summary$`Total number Nurseries protected`/nursery_protection_summary$`Total Number Nurseries`*100
nursery_protection_summary <- nursery_protection_summary[!nursery_protection_summary$Country %in% c("","Disputed"),]

#WIO regional 
sum(nursery_protection_summary$`Total number Nurseries protected`)/(sum(nursery_protection_summary$`Total Number Nurseries`))*100


##Target 11 - Potential Seagrass fish nurseries of coral reefs within MPAs ----
#protected corals only and its respective seagrass bed 
#protectedCorals<- as.data.frame(seagrasNursery_EEZ[seagrasNursery_EEZ$CATEGORY2 %in% "National",]) #390557 protected reefs 

protectedCorals <- read.csv(here("_dataTargets11_13","protectedCorals_Seagrass.csv"))
protectedCorals$COUNTRY <- as.factor(protectedCorals$COUNTRY)
nursery_protection_summary <- as.data.frame(protectedCorals[,c("COUNTRY","seagrassID")] %>%
                                              group_by (COUNTRY) %>%
                                              summarise(n=n()))

nursery_protected_sum <- as.data.frame(protectedCorals[protectedCorals$CATEGORY2.1 %in% "National",] %>%
                                         group_by (COUNTRY) %>%
                                         summarise(n=n()))

nursery_protection_summary<-left_join(nursery_protection_summary,nursery_protected_sum,by="COUNTRY",all=T)
colnames(nursery_protection_summary) <- c("Country","Total Number Nurseries", "Total number Nurseries protected")
nursery_protection_summary[is.na(nursery_protection_summary)] <- 0
nursery_protection_summary$percent <- nursery_protection_summary$`Total number Nurseries protected`/nursery_protection_summary$`Total Number Nurseries`*100
nursery_protection_summary <- nursery_protection_summary[!nursery_protection_summary$Country %in% c("","Disputed"),]
#WIO regional 
sum(nursery_protection_summary$`Total number Nurseries protected`)/(sum(nursery_protection_summary$`Total Number Nurseries`))*100




