#Target 5 - Ongoing analysis
#Luisa Fontoura - Aug2021 - Last update: 11 Oct

#Definition target 5 (CBD)
#By 2020, the rate of loss of all natural habitats, including forests, 
#is at least halved and where feasible brought close to zero, and degradation 
#and fragmentation is significantly reduced.

#Brief description
#Here, we used a temporal comparison of mangrove cover between 2010 and 2016 to
#estimate whether the loss of a key habitat was lower inside compared to outside MPA/PAs

#Databases used
#Intersection_mangrove201X_MPAs_no_overlayPAs -> Intersection of mangrove area with MPA areas (WIO layer) that do NOT overlay with PA (WDPA layer).
#Intersection_LandPA_mangrove201X -> Intersection of mangrove area with protected areas (WDPA layer)
#Mangrove2010_area_OUT_PAs -> Mangrove area outside both PA and MPA layers (in QGIS, WIO and WDPA layers were merged and intersection area excluded prior calculating total mangrove area by country)

#Libraries
library(here)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(viridis)


#Mangrove area with MPA areas (WIO layer) that do NOT overlay with PA (WDPA layer)
mangrove2010MPA <- read.csv(here("_data_Target5","Intersection_mangrove2010_MPAs_no_overlayPAs.csv"))
mangrove2016MPA <- read.csv(here("_data_Target5","Intersection_mangrove2016_MPA_no_overlayPAs.csv"))

mangrove2010MPA$YEAR <- as.numeric(mangrove2010MPA$YEAR)
mangrove2016MPA$YEAR <- as.numeric(mangrove2016MPA$YEAR)
#remove areas without year of establishment
mangrove2010MPA <- mangrove2010MPA %>% filter(!is.na(mangrove2010MPA$YEAR))
mangrove2016MPA <- mangrove2016MPA %>% filter(!is.na(mangrove2016MPA$YEAR))
#for comparisons, filter by MPAs created before or at 2010 and National MPAs working 
pre <- aggregate(int_area_m2_2010 ~ COUNTRY, mangrove2010MPA[mangrove2010MPA$STATUS == "Working" & mangrove2010MPA$YEAR < 2011 & mangrove2010MPA$CATEGORY == "National",], sum) 
pos<-aggregate(int_area_m2_2016 ~ COUNTRY, mangrove2016MPA[mangrove2016MPA$STATUS == "Working" & mangrove2016MPA$YEAR < 2011 & mangrove2016MPA$CATEGORY == "National",], sum)

tempmangrove<-left_join(pos,pre, by="COUNTRY")

#on land 
mangrove2010PA <- read.csv(here("_data_Target5", "Intersection_LandPA_mangrove2010.csv"))
mangrove2016PA <- read.csv(here("_data_Target5", "Intersection_LandPA_mangrove2016.csv"))
unique(mangrove2010PA$ISO3)
mangrove2010PA$COUNTRY <- ifelse(mangrove2010PA$ISO3 == "COM", "Comoros",
                                 ifelse(mangrove2010PA$ISO3 == "MYT", "France",
                                 ifelse(mangrove2010PA$ISO3 == "MDG", "Madagascar",
                                        ifelse(mangrove2010PA$ISO3 == "MOZ", "Mozambique",
                                               ifelse(mangrove2010PA$ISO3 == "KEN", "Kenya",
                                                      ifelse(mangrove2010PA$ISO3 == "TZA", "Tanzania",
                                                             ifelse(mangrove2010PA$ISO3 == "ZAF", "South Africa",
                                                                    ifelse(mangrove2010PA$ISO3 == "SYC", "Seychelles",
                                                                           ifelse(mangrove2010PA$ISO3 == "MUS", "Mauritius",NA)))))))))
mangrove2016PA$COUNTRY <- ifelse(mangrove2016PA$ISO3 == "COM", "Comoros",
                                 ifelse(mangrove2016PA$ISO3 == "MYT", "France",
                                        ifelse(mangrove2016PA$ISO3 == "MDG", "Madagascar",
                                               ifelse(mangrove2016PA$ISO3 == "MOZ", "Mozambique",
                                                      ifelse(mangrove2016PA$ISO3 == "KEN", "Kenya",
                                                             ifelse(mangrove2016PA$ISO3 == "TZA", "Tanzania",
                                                                    ifelse(mangrove2016PA$ISO3 == "ZAF", "South Africa",
                                                                           ifelse(mangrove2016PA$ISO3 == "SYC", "Seychelles",
                                                                                  ifelse(mangrove2016PA$ISO3 == "MUS", "Mauritius",NA)))))))))

#for comparisons, filter by PAs created before or at 2010 and national mpas working 
mangrove2010PA$STATUS_YR <- as.numeric(mangrove2010PA$STATUS_YR)
mangrove2016PA$STATUS_YR <- as.numeric(mangrove2016PA$STATUS_YR)

#remove PAs without IUCN categories and without year of establishment
mangrove2010PA <- mangrove2010PA %>% filter(!mangrove2010PA$STATUS_YR == 0 & !mangrove2010PA$IUCN_CAT %in% c("Not Reported","Not Assigned","Not Applicable"))
mangrove2016PA <- mangrove2016PA %>% filter(!mangrove2016PA$STATUS_YR == 0 & !mangrove2016PA$IUCN_CAT %in% c("Not Reported","Not Assigned","Not Applicable"))

#Checking same PAs (WDPAID) in the two time points
identical(unique(mangrove2010PA$WDPAID), unique(mangrove2016PA$WDPAID))

prePA <- aggregate(int_area_m2 ~ COUNTRY, mangrove2010PA[mangrove2010PA$STATUS_YR < 2011 & mangrove2010PA$DESIG_TYPE == "National",], sum) 
posPA <-aggregate(int_area_m2 ~ COUNTRY, mangrove2016PA[mangrove2016PA$STATUS_YR < 2011 & mangrove2016PA$DESIG_TYPE == "National",], sum)

tempmangrovePA<-left_join(posPA,prePA, by="COUNTRY")

colnames(tempmangrovePA) <- c("COUNTRY", "PA_aream2_2016","PA_2010_aream2")
colnames(tempmangrove) <- c("COUNTRY", "MPA_aream2_2016","MPA_2010_aream2")

tempmangrove_landseaprotected <- merge(tempmangrove,tempmangrovePA, by="COUNTRY",all=T)
tempmangrove_landseaprotected[is.na(tempmangrove_landseaprotected)] <- 0

tempmangrove_landseaprotected$TotalPA2016 <- tempmangrove_landseaprotected$PA_aream2_2016 + tempmangrove_landseaprotected$MPA_aream2_2016
tempmangrove_landseaprotected$TotalPA2010 <- tempmangrove_landseaprotected$PA_2010_aream2 + tempmangrove_landseaprotected$MPA_2010_aream2


#MANGROVE OUTISDE PAs (both MPA and PA) 
mangrove2010EEZ <-  read.csv(here("_data_Target5", "Mangrove2010_area_OUT_PAs.csv"))
mangrove2016EEZ <-  read.csv(here("_data_Target5", "Mangrove2016_area_OUT_PAs.csv"))

posOUT<-aggregate(int_area_m2_2016 ~ COUNTRYAFF, mangrove2016EEZ, sum)
preOUT <- aggregate(int_area_m2_2010 ~ COUNTRYAFF, mangrove2010EEZ, sum) 

tempmangroveEEZ<-left_join(posOUT,preOUT, by="COUNTRYAFF")

colnames(tempmangroveEEZ) <- c("COUNTRY", "tot_area2016","tot_area2010")
tempmangroveEEZ$deltaEEZ <- log(tempmangroveEEZ$tot_area2016/tempmangroveEEZ$tot_area2010)
tempmangroveEEZ$deltaEEZ_RC <- (tempmangroveEEZ$tot_area2016 - tempmangroveEEZ$tot_area2010)/tempmangroveEEZ$tot_area2010*100
tempmangroveEEZ$deltaEEZ_AC <- (tempmangroveEEZ$tot_area2016 - tempmangroveEEZ$tot_area2010)

tempmangroveEEZ <- tempmangroveEEZ[-c(2,3,13),]

tempmangrove_final<-left_join(tempmangroveEEZ,tempmangrove_landseaprotected,by="COUNTRY", all=F)
tempmangrove_final[is.na(tempmangrove_final)] <-0

tempmangrove_final$deltaOUTmpa<- log(tempmangrove_final$tot_area2016/tempmangrove_final$tot_area2010)
tempmangrove_final$deltaOUTmpa_RC<- (tempmangrove_final$tot_area2016 - tempmangrove_final$tot_area2010)/tempmangrove_final$tot_area2010*100
tempmangrove_final$deltaOUTmpa_AC<- (tempmangrove_final$tot_area2016 - tempmangrove_final$tot_area2010)

tempmangrove_final$deltaINpa<- log(tempmangrove_final$TotalPA2016/tempmangrove_final$TotalPA2010)
tempmangrove_final$deltaINpa_RC<- (tempmangrove_final$TotalPA2016 - tempmangrove_final$TotalPA2010)/tempmangrove_final$TotalPA2010*100
tempmangrove_final$deltaINpa_AC<- (tempmangrove_final$TotalPA2016 - tempmangrove_final$TotalPA2010)


mangrove_changes <- tempmangrove_final[,c("COUNTRY","deltaINpa_RC","deltaOUTmpa_RC")]
mangrove_Achanges <- tempmangrove_final[,c("COUNTRY","deltaINpa_AC","deltaOUTmpa_AC")]

plotchanges<- 
  as.data.frame(mangrove_changes %>%
                  pivot_longer(!COUNTRY, names_to = "deltaCAT", values_to = "changes"))

plotchangesA<- 
  as.data.frame(mangrove_Achanges %>%
                  pivot_longer(!COUNTRY, names_to = "deltaCAT", values_to = "changes"))

#build obj for plot

RChanges <- ggplot(plotchanges, aes(COUNTRY, changes, fill = deltaCAT)) + geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() + scale_fill_viridis(discrete=TRUE, option="cividis") + ylim(c(-10,10)) + xlab("") + ylab("Relative change (%) of Mangrove cover")

###
AChanges <- ggplot(plotchangesA, aes(COUNTRY, changes/10000, fill = deltaCAT)) + geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() + scale_fill_viridis(discrete=TRUE, option="cividis")  + xlab("") + ylab("Absolute change (hectare) of Mangrove cover")

##
mangrove_Target5 <- ggarrange(AChanges,RChanges, nrow=2,common.legend = T,legend="bottom",labels = c("A","B"))
mangrove_Target5

####Total MANGROVE AREA

total_mangrove_area2010<-read.csv(here("_data_Target5",'Mangrove_area2010_Intersection_Country_layer.csv'))
total_mangrove_area2016<-read.csv(here("_data_Target5","mangrove_area2016_intersection_country_layer.csv"))

tot_bycountry2010<-aggregate(mangrove_area_m2_2010 ~ COUNTRYAFF, total_mangrove_area2010, sum)
tot_bycountry2016<-aggregate(total_mangrove_area_m2 ~ COUNTRYAFF, total_mangrove_area2016, sum)
tot_bycountry<-left_join(tot_bycountry2010,tot_bycountry2016,by="COUNTRYAFF")
tot_bycountry$delta_TOT <- log(tot_bycountry$total_mangrove_area_m2/tot_bycountry$mangrove_area_m2_2010)
tot_bycountry <- tot_bycountry %>% filter(!tot_bycountry$COUNTRYAFF %in% c("Djibouti","Eritrea","Yemen"))
colnames(tot_bycountry) <- c("COUNTRY","Mangrove_totalarea_m2_2010","Mangrove_totalarea_m2_2016","delta_TOT")
tempman <- tempmangrove_final[,c("COUNTRY","TotalPA2016","TotalPA2010","tot_area2016","tot_area2010","deltaINpa","deltaOUTmpa")]
final_total_mang<-left_join(tot_bycountry,tempman,by="COUNTRY")
mangrove_tot <- final_total_mang[,c("COUNTRY","Mangrove_totalarea_m2_2010","Mangrove_totalarea_m2_2016")]

plotarea<- 
  as.data.frame(mangrove_tot  %>%
                  pivot_longer(!COUNTRY, names_to = "Year", values_to = "Area_m2"))
#########
##Plots##

total_area_plot<-ggplot(plotarea, aes(COUNTRY, Area_m2/10000, fill = Year)) + geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() + scale_fill_viridis(discrete=TRUE, option = "plasma")

changes_plot<-ggplot(plotchanges, aes(COUNTRY, changes, fill = deltaCAT)) + geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() + scale_fill_viridis(discrete=TRUE) + ylim(c(-0.25,0.25))

library(ggpubr)
#Create object for graphical edition in inkscape 
mangrove_Target5 <- ggarrange(AChanges,RChanges, nrow=2,common.legend = T,legend="bottom",labels = c("A","B"))

