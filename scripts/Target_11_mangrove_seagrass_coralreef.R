#Target 11 
#Protection of Mangroves, Seagrass and Coral reefs

#load library
library(here)
library(dplyr)
library(tidyverse)


#read files
seagrass <- read.csv("~/Desktop/Data Target 11/seagrass_EEZ_ID.csv")
coralreefs <- read.csv("~/Desktop/Data Target 11/WIO_coralreefs_area_F.csv")
mangrove <- read.csv("~/Desktop/Data Target 11/WIO_mangrove_area.csv")
seamount <- read.csv("~/Desktop/Data Target 11/WIO_seamounts_points.csv")

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


#filtering to acoid double count of area
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

###
#Marine protected area (MPAs)

mpa <- read.csv("~/Desktop/Data Target 11/WIO_MPA_area.csv")
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

#Seagrass nurseries  % of protection (this indicator is within Target 13 )
seagrass_nursery <- read.csv("~/Desktop/Data Target 11/seagrass_nurseries_rowid.csv")
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
