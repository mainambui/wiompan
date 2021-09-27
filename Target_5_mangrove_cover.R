#Target 5 - Ongoing analysis

#Note : update total mangrove area per country in QGIS - current data being used 
#are likely deprecated (joine function); recalculate deltaEEZ using land sea mangrove/country 
#and recalculate deltaOUTmpa

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
library(tidyverse)
library(viridis)


mangrove2010MPA <- read.csv(here("_data_Target5","Intersection_Mangrove_2010_MPA.csv"))
mangrove2016MPA <- read.csv(here("_data_Target5","Intersection_mangrove_MPA_2016.csv"))

#for comparisons, filter by MPAs created before or at 2010 and national mpas working 
mangrove2010MPA$YEAR <- as.numeric(mangrove2010MPA$YEAR)
mangrove2016MPA$YEAR <- as.numeric(mangrove2016MPA$YEAR)
#remove areas without year of establishment
mangrove2010MPA <- mangrove2010MPA %>% filter(!is.na(mangrove2010MPA$YEAR))
mangrove2016MPA <- mangrove2016MPA %>% filter(!is.na(mangrove2016MPA$YEAR))

pre <- aggregate(int_2010_aream2 ~ COUNTRY, mangrove2010MPA[mangrove2010MPA$STATUS == "Working" & mangrove2010MPA$YEAR < 2011 & mangrove2010MPA$CATEGORY == "National",], sum) 
pos<-aggregate(int_aream2_2016 ~ COUNTRY, mangrove2016MPA[mangrove2016MPA$STATUS == "Working" & mangrove2016MPA$YEAR < 2011 & mangrove2016MPA$CATEGORY == "National",], sum)

tempmangrove<-left_join(pos,pre)

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

#load clipped mangroves within both MPA and PA - 
# these data will be used to discount the mangrove area within MPAs to estimate the mangrove area only protected  by land/coastal mpas
clipped_mangrove2010_MPAinPA<-read.csv(here("_data_Target5", "Clipped_2010_PA_MPA.csv"))
clipped_mangrove2016_MPAinPA<-read.csv(here("_data_Target5", "Clipped_2016_PA_MPA.csv"))


#create Country column based on ISO3
clipped_mangrove2016_MPAinPA$COUNTRY <- ifelse(clipped_mangrove2016_MPAinPA$ISO3 == "COM", "Comoros",
                                 ifelse(clipped_mangrove2016_MPAinPA$ISO3 == "MYT", "France",
                                        ifelse(clipped_mangrove2016_MPAinPA$ISO3 == "MDG", "Madagascar",
                                               ifelse(clipped_mangrove2016_MPAinPA$ISO3 == "MOZ", "Mozambique",
                                                      ifelse(clipped_mangrove2016_MPAinPA$ISO3 == "KEN", "Kenya",
                                                             ifelse(clipped_mangrove2016_MPAinPA$ISO3 == "TZA", "Tanzania",
                                                                    ifelse(clipped_mangrove2016_MPAinPA$ISO3 == "ZAF", "South Africa",
                                                                           ifelse(clipped_mangrove2016_MPAinPA$ISO3 == "SYC", "Seychelles",
                                                                                  ifelse(clipped_mangrove2016_MPAinPA$ISO3 == "MUS", "Mauritius",NA)))))))))

clipped_mangrove2010_MPAinPA$COUNTRY <- ifelse(clipped_mangrove2010_MPAinPA$ISO3 == "COM", "Comoros",
                                               ifelse(clipped_mangrove2010_MPAinPA$ISO3 == "MYT", "France",
                                                      ifelse(clipped_mangrove2010_MPAinPA$ISO3 == "MDG", "Madagascar",
                                                             ifelse(clipped_mangrove2010_MPAinPA$ISO3 == "MOZ", "Mozambique",
                                                                    ifelse(clipped_mangrove2010_MPAinPA$ISO3 == "KEN", "Kenya",
                                                                           ifelse(clipped_mangrove2010_MPAinPA$ISO3 == "TZA", "Tanzania",
                                                                                  ifelse(clipped_mangrove2010_MPAinPA$ISO3 == "ZAF", "South Africa",
                                                                                         ifelse(clipped_mangrove2010_MPAinPA$ISO3 == "SYC", "Seychelles",
                                                                                                ifelse(clipped_mangrove2010_MPAinPA$ISO3 == "MUS", "Mauritius",NA)))))))))


#for comparisons, filter by PAs created before or at 2010 and national mpas working 
mangrove2010PA$STATUS_YR <- as.numeric(mangrove2010PA$STATUS_YR)
mangrove2016PA$STATUS_YR <- as.numeric(mangrove2016PA$STATUS_YR)
#remove areas without year of establishment
prePA <- aggregate(int_area_m2 ~ COUNTRY, mangrove2010PA[mangrove2010PA$STATUS_YR < 2011,], sum) 
posPA <-aggregate(int_area_m2 ~ COUNTRY, mangrove2016PA[mangrove2016PA$STATUS_YR < 2011,], sum)

tempmangrovePA<-left_join(posPA,prePA, by="COUNTRY")
colnames(tempmangrovePA) <- c("COUNTRY", "int_aream2_2016","int_2010_aream2")

#remove overlapping area PA vs MPA_WIO dataset
#for comparisons, filter by PAs created before or at 2010 and national mpas working 
clipped_mangrove2010_MPAinPA$STATUS_YR <- as.numeric(clipped_mangrove2010_MPAinPA$STATUS_YR)
clipped_mangrove2016_MPAinPA$STATUS_YR <- as.numeric(clipped_mangrove2016_MPAinPA$STATUS_YR)
#remove areas without year of establishment
Clipped_prePA <- aggregate(clipped_area_m2 ~ COUNTRY, clipped_mangrove2010_MPAinPA[clipped_mangrove2010_MPAinPA$STATUS_YR < 2011,], sum) 
Clipped_posPA <-aggregate(clipped_area_m2 ~ COUNTRY, clipped_mangrove2016_MPAinPA[clipped_mangrove2016_MPAinPA$STATUS_YR < 2011,], sum)

Clipped_tempmangrovePA<-left_join(Clipped_prePA,Clipped_posPA, by="COUNTRY")
colnames(Clipped_tempmangrovePA) <- c("COUNTRY", "cli_int_aream2_2016","cli_int_2010_aream2")

tempmangrovePA <- left_join(tempmangrovePA,Clipped_tempmangrovePA,by="COUNTRY")
tempmangrovePA[is.na(tempmangrovePA)] <-0

tempmangrovePA$int_area_2016_Land <- tempmangrovePA$int_aream2_2016 - tempmangrovePA$cli_int_aream2_2016
tempmangrovePA$int_area_2010_Land <- tempmangrovePA$int_2010_aream2 - tempmangrovePA$cli_int_2010_aream2


tempmangrove_landseaprotected<-left_join(tempmangrovePA[,c("COUNTRY","int_area_2016_Land","int_area_2010_Land")], tempmangrove, by="COUNTRY")
tempmangrove_landseaprotected[is.na(tempmangrove_landseaprotected)] <-0
tempmangrove_landseaprotected$tot_protected_aream2_2016 <- tempmangrove_landseaprotected$int_area_2016_Land + tempmangrove_landseaprotected$int_aream2_2016
tempmangrove_landseaprotected$tot_protected_aream2_2010 <- tempmangrove_landseaprotected$int_area_2010_Land + tempmangrove_landseaprotected$int_2010_aream2
tempmangrove_landseaprotected



#mangrove cover outside mpas 
mangrove2010EEZ <-  read.csv(here("_data_Target5", "mangrove2010_eez_joined.csv"))
mangrove2016EEZ <-  read.csv(here("_data_Target5", "mangrove2016_eez_joined.csv"))
head(mangrove2016EEZ)
head(mangrove2010EEZ)

posOUT<-aggregate(join_aream2 ~ Sovereign, mangrove2016EEZ, sum)
preOUT <- aggregate(join_aream2 ~ Sovereign, mangrove2010EEZ, sum) 

tempmangroveEEZ<-left_join(posOUT,preOUT, by="Sovereign")
colnames(tempmangroveEEZ) <- c("COUNTRY", "tot_area2016","tot_area2010")
tempmangroveEEZ$deltaEEZ <- log(tempmangroveEEZ$tot_area2016/tempmangroveEEZ$tot_area2010)
tempmangroveEEZ$COUNTRY <- ifelse(tempmangroveEEZ$COUNTRY == "Comoro Islands", "Comoros", tempmangroveEEZ$COUNTRY)

tempmangrove_final<-left_join(tempmangroveEEZ,tempmangrove_landseaprotected,by="COUNTRY")
tempmangrove_final[is.na(tempmangrove_final)] <-0

tempmangrove_final$area_OUTmpa2010 <- tempmangrove_final$tot_area2010 - tempmangrove_final$tot_protected_aream2_2010
tempmangrove_final$area_OUTmpa2016 <- tempmangrove_final$tot_area2016 - tempmangrove_final$tot_protected_aream2_2016
tempmangrove_final$deltaOUTmpa<- log(tempmangrove_final$area_OUTmpa2016/tempmangrove_final$area_OUTmpa2010)
tempmangrove_final$deltaINpa<- log(tempmangrove_final$tot_protected_aream2_2016/tempmangrove_final$tot_protected_aream2_2010)

head(tempmangrove_final)

mangrove_changes <- tempmangrove_final[,c("COUNTRY","deltaEEZ","deltaINpa","deltaOUTmpa")]

plotchanges<- 
  as.data.frame(mangrove_changes %>%
                  pivot_longer(!COUNTRY, names_to = "deltaCAT", values_to = "changes"))

#build obj for plot

ggplot(plotchanges, aes(COUNTRY, changes, fill = deltaCAT)) + geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() + scale_fill_viridis(discrete=TRUE)
