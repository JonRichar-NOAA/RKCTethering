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

inter_j<-read_csv("./DATA/Interactions_J.csv")
inter_a<-read_csv("./DATA/Interactions_A.csv")

behave_j<-read_csv("./DATA/Behavior_J.csv")
behave_a<-read_csv("./DATA/Behavior_A.csv")

files_j<-read_csv("./DATA/Files_J.csv")
files_a<-read_csv("./DATA/Files_A.csv")


##################### Combine interactions #####################################

inter_dat<-as_tibble(rbind(inter_j,inter_a))

names(inter_dat)
##################### Combine behaviors #########################################
behav_dat<-as_tibble(rbind(behave_j,behave_a))

names(behav_dat)

############################################################################################
##################### Explore interaction data ##############################################

inter_dat%>%
  dplyr::distinct(Effector_species)->spec_dat

View(spec_dat)

## all interactions

all_int<-as.matrix(as.numeric(inter_dat$Interaction))
edit_int<-na.omit(all_int)
edit_int
nrow(edit_int)                              #540 total interactions
hist(edit_int)
######################################################################\

inter_dat%>% dplyr::filter(Effector_species %like% "hermit") ->hc

hc_int<-as.numeric(hc$Interaction)
count(hc_int)
hist(hc_int)

inter_dat%>% dplyr::filter(Effector_species %like% "cod") ->cod
View(cod)

cod_int<-as.numeric(cod$Interaction)
hist(cod_int)

inter_dat%>% dplyr::filter(Effector_species %like% "scul") ->sculpin
View(sculpin)

sculpin_int<-as.numeric(sculpin$Interaction)
hist(sculpin_int)


inter_dat%>% dplyr::filter(Effector_species %like% "gree") -> greenling
View(greenling)

greenling_int<-as.numeric(greenling$Interaction)
hist(greenling_int)

inter_dat%>% dplyr::filter(Effector_species %like% "gun") -> gunnel
View(gunnel)

gunnel_int<-as.numeric(gunnel$Interaction)
hist(gunnel_int)

inter_dat%>% dplyr::filter(Effector_species %like% "rock") -> rockfish
View(rockfish)

rockfish_int<-as.numeric(rockfish$Interaction)
hist(rockfish_int)
###################### Explore behavior data 
View(behav_dat)

hist(as.numeric(behav_dat$Crab_behavior))
hist(as.numeric(behav_dat$Cover))
hist(as.numeric(behav_dat$Habitat))

behav_dat%>%
  dplyr::distinct(Crab_behavior)->behaviors

View(behaviors)
