library(ggplot2)
library(tidyverse)
library(viridis)

##
gap_sum<-read.csv("~/Desktop/gap-analysis_summary.csv")
#Target as factor----
gap_sum$target <- as.factor(gap_sum$target)
#gap_sum$indicator <- as.factor(gap_sum$indicator)
gap_sum$indicator <- factor(gap_sum$indicator, levels = c("total_protected_area","KBA","mangrove","seagrass","seamount","coral reef","larval sinks","larval sources",
                                                          "turtle_nest","IBA","TA spawning","Dispersal corridor","larval sources of PA","seagrass_nursery","seagrass nursery of PA") )
#Create ISO column
unique(gap_sum$location)
gap_sum$ISO3 <- ifelse(gap_sum$location == "Comoros","COM",
                       ifelse(gap_sum$location == "France", "FRA",
                       ifelse(gap_sum$location == "Kenya", "KEN",
                       ifelse(gap_sum$location == "Madagascar", "MDG",
                              ifelse(gap_sum$location == "Mauritius", "MUS",
                                     ifelse(gap_sum$location == "Mozambique", "MOZ",
                                            ifelse(gap_sum$location == "Seychelles", "SYC",
                                                   ifelse(gap_sum$location == "Somalia", "SOM",
                                                          ifelse(gap_sum$location == "South Africa", "ZAF",
                                                                 ifelse(gap_sum$location == "Tanzania", "TZA", gap_sum$location))))))))))
#Create levels 
#Create unique ID for each row
data_all <- gap_sum
colnames(data_all) <- c("location","target","individual","type","value","group")
data_all$id=seq(1, nrow(data_all))
head(data_all)

###Circular plot for Target 11 ----
target11 <- data_all[data_all$target == "11",]
unique(target11$group) #9 indicators (fish nurseries of PAs missing*)
rm(data)
data <- target11
#remove WIO (only estimates by country)
data <- data[!data$location %in% c("WIO","Somalia","South Africa"),]
data<-droplevels(data)
data$group <- as.factor(data$group)

# Set a number of 'empty bar' to add at the end of each group
empty_bar=3
to_add = data.frame(matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(group,location)
data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I subtract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

####Plot----
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=individual)) +
  geom_bar(aes(x=as.factor(id), y=value, fill=individual), stat="identity", alpha=0.5) +
  scale_fill_viridis_d(option="inferno", name="Indicators",
                       breaks=levels(data$individual), labels=c("EEZ protected","Key Biodibersity Areas (KBAs)","Mangrove",
                                                           "Seagrass","Seamount","Coral Reefs","Fish larval sinks","Fish larval sources","Fish larval sources of MPAs","Seagrass nursery of MPAs")) +
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:5) * 20),
    color = "lightgrey",alpha=0.3, size=0.2) +
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),5), y = c(20, 40, 60, 80,100), label = c("20%", "40%", "60%", "80%","100%") , color="black", size=3.5 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=individual), stat="identity", alpha=0.8) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar()

target11_plot <- p + 
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  #geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0,0,0,0,0,1), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
   annotate(
    x = 0, 
    y = -100, 
    label = "Target 11", 
    geom = "text", 
    color = "black",
    size=10
  ) 

target11_plot +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

#Target 13
###Circular plot for Target 13 ----
target13 <- data_all[data_all$target == "13",]
unique(target13$group) 
rm(data)
data <- target13
#remove WIO (only estimates by country)
data <- data[!data$location %in% c("WIO","Somalia","South Africa"),]
data<-droplevels(data)
data$group <- as.factor(data$group)

# Set a number of 'empty bar' to add at the end of each group
empty_bar=3
to_add = data.frame(matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
data=data %>% arrange(group,location)
data$id=seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

####Plot----
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=individual)) +
  geom_bar(aes(x=as.factor(id), y=value, fill=individual), stat="identity", alpha=0.5) +
  scale_fill_viridis_d(option="mako", name="Indicators",
                       breaks=levels(data$individual), labels=c("Turtle nests","Important Bird Areas (IBAs)","Fish spawning aggregation sites (Trasient fish",
                                                                "Reef fish dispersal corridors", "Seagrass fish nurseries")) +
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:5) * 20),
    color = "lightgrey",alpha=0.3, size=0.2) +
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),5), y = c(20, 40, 60, 80,100), label = c("20%", "40%", "60%", "80%","100%") , color="black", size=3.5 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=individual), stat="identity", alpha=0.8) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar()

target13_plot <- p + 
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  #geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0,0,0,0,0,1), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
  annotate(
    x = 0, 
    y = -100, 
    label = "Target 13", 
    geom = "text", 
    color = "black",
    size=10
  ) 

target13_plot +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)


##Graph for WIO ----
#
rm(data)
target11WIO <- data_all[data_all$target == "11",]
data <- target11WIO
#remove WIO (only estimates by country)
data <- data[data$location %in% c("WIO"),]
data<-droplevels(data)

target11WIO_plot <- ggplot(data, aes(x=as.factor(individual), y=value, fill=individual)) +
  geom_bar(aes(x=as.factor(individual), y=value, fill=individual), stat="identity", alpha=0.8) +
  scale_fill_viridis_d(option="inferno", name="Indicators",
                         breaks=levels(data$individual), labels=c("EEZ protected","Key Biodibersity Areas (KBAs)","Mangrove",
                                                             "Seagrass","Seamount","Coral Reefs","Fish larval sinks","Fish larval sources","Fish larval sources of MPAs", "Seagrass nnursery of MPAs")) + ylim(0,100) +
  theme_classic()  +
  theme(
    legend.position = "none")

#13
rm(data)
target13WIO <- data_all[data_all$target == "13",]
data <- target13WIO
#remove WIO (only estimates by country)
data <- data[data$location %in% c("WIO"),]
data<-droplevels(data)

target13WIO_plot <- ggplot(data, aes(x=as.factor(individual), y=value, fill=individual)) +
  geom_bar(aes(x=as.factor(individual), y=value, fill=individual), stat="identity", alpha=0.8) +
  scale_fill_viridis_d(option="mako", name="Indicators",
                       breaks=levels(data$individual), labels=c("Turtle nests","Important Bird Areas (IBAs)","Fish spawning aggregation sites (Trasient fish",
                                                                "Reef fish dispersal corridors", "Seagrass fish nurseries")) + ylim(0,100) +
  theme_classic()  +
  theme(
    legend.position = "none")

library(ggpubr)

ggarrange(target11WIO_plot,target13WIO_plot,ncol=2)

