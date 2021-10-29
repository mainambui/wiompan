#Target 11 - Larval connectivity
library(dplyr)
library(sna)
library(igraph)
library(here)
library(ggpubr)
library(reshape)
library(stats)

#1.1 Identifying the percentage of top larval sources and sinks protected across Countries 

#EEZ Connectivity reef cells (dataset built in QGIS - overlaying Connectivity.shp and WIO_EEZ.shp)
EEZ_CON<-read.csv(here("_data_Target11","EEZ_reef_cells_con.csv"))
EEZ_CON <- EEZ_CON %>% filter(!is.na(ID_2))
dim(EEZ_CON) #750
#unique(EEZ_CON$Sovereign) #Territories
#total number of reef cells by Country
Summary_CON <- as.data.frame(EEZ_CON %>% 
  group_by(Sovereign) %>%
  summarise(n = n()))
#sum(Summary_CON$n) #750 reef cells

#MPAs Connectivity reef cells within MPAs (buffered radius 4km) (dataset build in QGIS - overlaying connectivity.shp using buffer 4km and MPa.shp)
MPA_Con_B<- read.csv(here("_data_Target11","MPA_Connectivity_Overlay.csv")) #8km buffer
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

WIO_Graph<-read.csv(here("_data_Target11", "crypto5matrixWIO.csv"))
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


library(ggpubr)
#Create object for graphical edition in inkscape for A - conpectual and B - results 
relatinnetfin <- ggarrange(allRe,allRe, nrow=1,common.legend = T,legend="bottom",labels = c("A","B"))





