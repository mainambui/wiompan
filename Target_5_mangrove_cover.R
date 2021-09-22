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
tempmangrove_final

pre <- aggregate(int_2010_aream2 ~ COUNTRY, mangrove2010MPA[mangrove2010MPA$STATUS == "Working" & mangrove2010MPA$YEAR < 2011 & mangrove2010MPA$CATEGORY == "National",], sum) 
pos<-aggregate(int_aream2_2016 ~ COUNTRY, mangrove2016MPA[mangrove2016MPA$STATUS == "Working" & mangrove2016MPA$YEAR < 2011 & mangrove2016MPA$CATEGORY == "National",], sum)

tempmangrove<-left_join(pos,pre)

tempmangrove$deltaMPA <- log(as.integer(tempmangrove$int_aream2_2016)/as.integer(tempmangrove$int_2010_aream2))


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
tempmangrove_final<-left_join(tempmangroveEEZ,tempmangrove,by="COUNTRY")

tempmangrove_final$area_OUTmpa2010 <- tempmangrove_final$tot_area2010 - tempmangrove_final$int_2010_aream2
tempmangrove_final$area_OUTmpa2016 <- tempmangrove_final$tot_area2016 - tempmangrove_final$int_aream2_2016
tempmangrove_final$deltaOUTmpa<- log(tempmangrove_final$area_OUTmpa2016/tempmangrove_final$area_OUTmpa2010)
head(tempmangrove_final)
mangrove_changes <- tempmangrove_final[,c("COUNTRY","deltaEEZ","deltaMPA","deltaOUTmpa")]

plotchanges<- 
  as.data.frame(mangrove_changes %>%
  pivot_longer(!COUNTRY, names_to = "deltaCAT", values_to = "changes"))

#build obj for plot

ggplot(plotchanges, aes(COUNTRY, changes, fill = deltaCAT)) + geom_bar(stat="identity", position=position_dodge()) +
  theme_classic() + scale_fill_viridis(discrete=TRUE)


