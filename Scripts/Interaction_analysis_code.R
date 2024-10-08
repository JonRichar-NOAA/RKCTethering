#Add libraries
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(patchwork)
library(ggridges)
library(stats)
library(nlme)
library(lmtest)
library(mgcv)
library(nlme)
library(nlstools)
library(MuMIn)
library(tidyverse)
library(corrplot)
library(voxel)
library(data.table)


##################################################################################
################################## Add data #####################################

inter_j<-read_csv("./DATA/Interactions_J_proc.csv")
inter_a<-read_csv("./DATA/Interactions_A_proc.csv")

behave_j<-read_csv("./DATA/Behavior_J.csv")
behave_a<-read_csv("./DATA/Behavior_A.csv")

files_j<-read_csv("./DATA/Files_J.csv")
files_a<-read_csv("./DATA/Files_A.csv")

files_j %>%
  select(DATJOIN, Location)->files_j_sub
files_a %>%
  select(DATJOIN, Location)->files_a_sub

left_join(inter_j, files_j_sub, by = "DATJOIN") ->inter_j_join
left_join(inter_a, files_a_sub, by = "DATJOIN") ->inter_a_join
##################### Combine interactions #####################################
### NOTE: Need to join by investigator before combining files

inter_dat0<-as_tibble(rbind(inter_j_join,inter_a_join))

names(inter_dat0)
View(inter_dat0)

inter_dat0 %>%
  mutate(Total_time_sec=Total_time_elapsed/60) ->inter_dat
##############################################################################
names(inter_dat)

inter_dat %>% 
  select(Effector_species,Interaction,Location,Total_time_elapsed) %>%
  group_by(Effector_species,Interaction,Location) %>%
  mutate(Interaction_time = sum(Total_time_elapsed/60))%>%
  distinct(Effector_species,Interaction,Location,Interaction_time) -> interdat_analysis
  
View(interdat_analysis)

interdat_analysis%>%
  filter(Location == "Trident")->interdat_trident

ggplot(interdat_trident, aes(x=Interaction, y=Interaction_time))+
  geom_bar() +
  labs(y="Time in sec",x="Interaction", title="Trident Basin")+
  facet_wrap(~Effector_species, scales = "free_y", ncol = 1) +
  labs(y = "Time in sec", main = "Trident Basin") 

facet_plot<- ggplot(interdat_trident, aes(x=Interaction, y=Interaction_time))+
  geom_bar() +
  labs(y="Time in sec",x="Interaction", title="Trident Basin")+
  facet_wrap(~Effector_species, scales = "free_y", ncol = 1) +
  labs(y = "Time in sec", main = "Trident Basin") 

ggsave(plot = facet_plot, "./Figures/FacetPlots.jpeg", height=10, width=7, units="in")
