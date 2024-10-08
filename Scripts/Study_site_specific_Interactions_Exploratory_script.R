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

############################################################################################
##################### Explore interaction data ##############################################

inter_dat%>%
  dplyr::distinct(Effector_species)->spec_dat

#View(spec_dat)

## all interactions

all_int<-as.matrix(as.numeric(inter_dat$Interaction))
edit_int<-na.omit(all_int)
edit_int
nrow(edit_int)                              #540 total interactions
hist(edit_int)
######################################################################\

inter_dat%>% dplyr::filter(Effector_species %like% "hermit") ->hc
View(hc)

hc_int<-as.numeric(hc$Interaction)
hist(hc_int)
Time_hc<-sum(hc$Total_time_elapsed/60)


inter_dat%>% dplyr::filter(Effector_species %like% "cod") ->cod
View(cod)

cod_int<-as.numeric(cod$Interaction)
hist(cod_int)
Time_cod<-sum(cod$Total_time_elapsed/60)

inter_dat%>% dplyr::filter(Effector_species %like% "scul") ->sculpin
View(sculpin)

sculpin_int<-as.numeric(sculpin$Interaction)
hist(sculpin_int)
Time_sculpin<-sum(sculpin$Total_time_elapsed/60)

inter_dat%>% dplyr::filter(Effector_species %like% "gree") -> greenling
View(greenling)

greenling_int<-as.numeric(greenling$Interaction)
hist(greenling_int)
Time_greenling<-sum(greenling$Total_time_elapsed/60)

inter_dat%>% dplyr::filter(Effector_species %like% "gun") -> gunnel
View(gunnel)

gunnel_int<-as.numeric(gunnel$Interaction)
hist(gunnel_int)
Time_gunnel<-sum(gunnel$Total_time_elapsed/60)

inter_dat%>% dplyr::filter(Effector_species %like% "rock") -> rockfish
View(rockfish)

rockfish_int<-as.numeric(rockfish$Interaction)
hist(rockfish_int)
Time_rockfish<-sum(rockfish$Total_time_elapsed/60)


##################################################################################################################################
############################### Trident Basin (Site A) only #####################################################################################

inter_dat %>% 
  filter(Location =="Trident") ->trident_dat


nrow(trident_dat)                              #373 total interactions

############################# Hermit crabs #########################################\

trident_dat%>% dplyr::filter(Effector_species %like% "hermit") ->hc_trident

hc_trident_int<-as.numeric(hc_trident$Interaction)
nrow(as.matrix(hc_trident_int))
hist(hc_trident_int)
Time_trident_hc<-sum(hc_trident$Total_time_elapsed/60)


hc_trident %>% dplyr::filter(Interaction>0)->hc_trident_nz
hc_trident_int_nz<-as.numeric(hc_trident_nz$Interaction)
nrow(as.matrix(hc_trident_int_nz))
hist(hc_trident_int_nz)

Time_trident_hc_nz<-sum(hc_trident_nz$Total_time_elapsed/60)

############################ Cod ##############################################
trident_dat%>% dplyr::filter(Effector_species %like% "cod") ->cod_trident
#View(cod_trident)

cod_trident_int<-as.numeric(cod_trident$Interaction)
nrow(as.matrix(cod_trident_int))
hist(cod_trident_int)
Time_trident_cod<-sum(cod_trident$Total_time_elapsed/60)


cod_trident %>% dplyr::filter(Interaction>0)->cod_trident_nz

cod_trident_int_nz<-as.numeric(cod_trident_nz$Interaction)
nrow(as.matrix(cod_trident_int_nz))
hist(cod_trident_int_nz)
Time_trident_cod_nz<-sum(cod_trident_nz$Total_time_elapsed/60)

############################# Sculpin ##########################################
trident_dat%>% dplyr::filter(Effector_species %like% "scul") ->sculpin_trident
#View(sculpin_trident)

sculpin_trident_int<-as.numeric(sculpin_trident$Interaction)
nrow(as.matrix(sculpin_trident_int))
hist(sculpin_trident_int)
Time_trident_sculpin<-sum(sculpin_trident$Total_time_elapsed/60)


sculpin_trident %>% dplyr::filter(Interaction>0)->sculpin_trident_nz

sculpin_trident_int_nz<-as.numeric(sculpin_trident_nz$Interaction)
nrow(as.matrix(sculpin_trident_int_nz))
hist(sculpin_trident_int_nz)
Time_trident_sculpin_nz<-sum(sculpin_trident_nz$Total_time_elapsed/60)

############################# Greenling ########################################
trident_dat%>% dplyr::filter(Effector_species %like% "gree") -> greenling_trident
#View(greenling_trident)

greenling_trident_int<-as.numeric(greenling_trident$Interaction)
nrow(as.matrix(greenling_trident_int))
hist(greenling_trident_int)
Time_trident_greenling<-sum(greenling_trident$Total_time_elapsed/60)


greenling_trident %>% dplyr::filter(Interaction>0)->greenling_trident_nz

greenling_trident_int_nz<-as.numeric(greenling_trident_nz$Interaction)
nrow(as.matrix(greenling_trident_int_nz))
hist(greenling_trident_int_nz)

Time_trident_greenling_nz<-sum(greenling_trident_nz$Total_time_elapsed/60)

############################# Gunnel ###########################################
trident_dat%>% dplyr::filter(Effector_species %like% "gun") -> gunnel_trident
#View(gunnel_trident)

gunnel_trident_int<-as.numeric(gunnel_trident$Interaction)
nrow(as.matrix(gunnel_trident_int))
hist(gunnel_trident_int)
Time_trident_gunnel<-sum(gunnel_trident$Total_time_elapsed/60)


gunnel_trident %>% dplyr::filter(Interaction>0)->gunnel_trident_nz

gunnel_trident_int_nz<-as.numeric(gunnel_trident_nz$Interaction)
nrow(as.matrix(gunnel_trident_int_nz))
hist(gunnel_trident_int_nz)

Time_trident_gunnel_nz<-sum(gunnel_trident_nz$Total_time_elapsed/60)

############################ Rockfish ##########################################
trident_dat%>% dplyr::filter(Effector_species %like% "rock") -> rockfish_trident
#View(rockfish_trident)

rockfish_trident_int<-as.numeric(rockfish_trident$Interaction)
nrow(as.matrix(rockfish_trident_int))
hist(rockfish_trident_int)
Time_trident_rockfish<-sum(rockfish_trident$Total_time_elapsed/60)


rockfish_trident %>% dplyr::filter(Interaction>0)->rockfish_trident_nz
rockfish_trident_int_nz<-as.numeric(rockfish_trident_nz$Interaction)
nrow(as.matrix(rockfish_trident_int_nz))
hist(rockfish_trident_int_nz)
Time_trident_rockfish_nz<-sum(rockfish_trident_nz$Total_time_elapsed/60)

##################################################################################################################################
############################### Long Island (Site B) only #####################################################################################

inter_dat %>% 
  filter(Location =="Long") ->long_dat


nrow(long_dat)                              #93 total interactions

############################### Hermit crab ####################################

long_dat%>% dplyr::filter(Effector_species %like% "hermit") ->hc_long

hc_long_int<-as.numeric(hc_long$Interaction)
nrow(as.matrix(hc_long_int))
hist(hc_long_int)
Time_long_hc<-sum(hc_long$Total_time_elapsed/60)

hc_long %>% dplyr::filter(Interaction>0)->hc_long_nz

hc_long_int_nz<-as.numeric(hc_long_nz$Interaction)
nrow(as.matrix(hc_long_int_nz))
hist(hc_long_int_nz)
Time_long_hc_nz<-sum(hc_long_nz$Total_time_elapsed/60)
################################ Cod ###########################################
long_dat%>% dplyr::filter(Effector_species %like% "cod") ->cod_long
#View(cod_long)

cod_long_int<-as.numeric(cod_long$Interaction)
nrow(as.matrix(cod_long_int))
hist(cod_long_int)
Time_long_cod<-sum(cod_long$Total_time_elapsed/60)

cod_long %>% dplyr::filter(Interaction>0)->cod_long_nz

cod_long_int_nz<-as.numeric(cod_long_nz$Interaction)
nrow(as.matrix(cod_long_int_nz))
hist(cod_long_int_nz)
Time_long_cod_nz<-sum(cod_long_nz$Total_time_elapsed/60)

################################ Sculpin #######################################
long_dat%>% dplyr::filter(Effector_species %like% "scul") ->sculpin_long
#View(sculpin_long)

sculpin_long_int<-as.numeric(sculpin_long$Interaction)
nrow(as.matrix(sculpin_long_int))
hist(sculpin_long_int)
Time_long_sculpin<-sum(sculpin_long$Total_time_elapsed/60)

sculpin_long %>% dplyr::filter(Interaction>0)->sculpin_long_nz

sculpin_long_int_nz<-as.numeric(sculpin_long_nz$Interaction)
nrow(as.matrix(sculpin_long_int_nz))
hist(sculpin_long_int_nz)
Time_long_sculpin_nz<-sum(sculpin_long_nz$Total_time_elapsed/60)

################################ Greenling #####################################
long_dat%>% dplyr::filter(Effector_species %like% "gree") -> greenling_long
#View(greenling_long)

greenling_long_int<-as.numeric(greenling_long$Interaction)
nrow(as.matrix(greenling_long_int))
hist(greenling_long_int)
Time_long_greenling<-sum(greenling_long$Total_time_elapsed/60)

greenling_long %>% dplyr::filter(Interaction>0)->greenling_long_nz

greenling_long_int_nz<-as.numeric(greenling_long_nz$Interaction)
nrow(as.matrix(greenling_long_int_nz))
hist(greenling_long_int_nz)
Time_long_greenling_nz<-sum(greenling_long_nz$Total_time_elapsed/60)

################################ Gunnel ########################################
long_dat%>% dplyr::filter(Effector_species %like% "gun") -> gunnel_long
#View(gunnel_long)

gunnel_long_int<-as.numeric(gunnel_long$Interaction)
nrow(as.matrix(gunnel_long_int))
hist(gunnel_long_int)
Time_long_gunnel<-sum(gunnel_long$Total_time_elapsed/60)


gunnel_long %>% dplyr::filter(Interaction>0)->gunnel_long_nz
gunnel_long_int_nz<-as.numeric(gunnel_long_nz$Interaction)
nrow(as.matrix(gunnel_long_int_nz))
hist(gunnel_long_int_nz)
Time_long_gunnel_nz<-sum(gunnel_long_nz$Total_time_elapsed/60)

################################ Rockfish ######################################
long_dat%>% dplyr::filter(Effector_species %like% "rock") -> rockfish_long
#View(rockfish_long)

rockfish_long_int<-as.numeric(rockfish_long$Interaction)
nrow(as.matrix(rockfish_long_int))
hist(rockfish_long_int)

Time_long_rockfish<-sum(rockfish_long$Total_time_elapsed/60)

rockfish_long %>% dplyr::filter(Interaction>0)->rockfish_long_nz

rockfish_long_int_nz<-as.numeric(rockfish_long_nz$Interaction)
nrow(as.matrix(rockfish_long_int_nz))
hist(rockfish_long_int_nz)
Time_long_rockfish_nz<-sum(rockfish_long_nz$Total_time_elapsed/60)
##################################################################################################################################
############################### Holiday Island (Site C) only #####################################################################################

inter_dat %>% 
  filter(Location =="Holiday") ->holiday_dat


nrow(holiday_dat)                              #540 total interactions
hist(holiday_dat)
################################ Hermit crab ###################################

holiday_dat%>% dplyr::filter(Effector_species %like% "hermit") ->hc_holiday

hc_holiday_int<-as.numeric(hc_holiday$Interaction)
nrow(as.matrix(hc_holiday_int))
hist(hc_holiday_int)
Time_holiday_hc<-sum(hc_holiday$Total_time_elapsed/60)

hc_holiday %>% dplyr::filter(Interaction>0)->hc_holiday_nz

hc_holiday_int_nz<-as.numeric(hc_holiday_nz$Interaction)
nrow(as.matrix(hc_holiday_int_nz))
hist(hc_holiday_int_nz)

Time_holiday_hc_nz<-sum(hc_holiday_nz$Total_time_elapsed/60)
################################ Cod ###########################################
holiday_dat%>% dplyr::filter(Effector_species %like% "cod") ->cod_holiday
#View(cod_holiday)

cod_holiday_int<-as.numeric(cod_holiday$Interaction)
nrow(as.matrix(cod_holiday_int))
hist(cod_holiday_int)
Time_holiday_cod<-sum(cod_holiday$Total_time_elapsed/60)

cod_holiday %>% dplyr::filter(Interaction>0)->cod_holiday_nz

cod_holiday_int_nz<-as.numeric(cod_holiday_nz$Interaction)
nrow(as.matrix(cod_holiday_int_nz))
hist(cod_holiday_int_nz)
Time_holiday_cod_nz<-sum(cod_holiday_nz$Total_time_elapsed/60)

################################ Sculpin #######################################
holiday_dat%>% dplyr::filter(Effector_species %like% "scul") ->sculpin_holiday
#View(sculpin_holiday)

sculpin_holiday_int<-as.numeric(sculpin_holiday$Interaction)
nrow(as.matrix(sculpin_holiday_int))
hist(sculpin_holiday_int)
Time_holiday_sculpin<-sum(sculpin_holiday$Total_time_elapsed/60)

sculpin_holiday %>% dplyr::filter(Interaction>0)->sculpin_holiday_nz

sculpin_holiday_int_nz<-as.numeric(sculpin_holiday_nz$Interaction)
nrow(as.matrix(sculpin_holiday_int_nz))
hist(sculpin_holiday_int_nz)
Time_holiday_sculpin_nz<-sum(sculpin_holiday_nz$Total_time_elapsed/60)

################################ Greenling #####################################
holiday_dat%>% dplyr::filter(Effector_species %like% "gree") -> greenling_holiday
#View(greenling_holiday)

greenling_holiday_int<-as.numeric(greenling_holiday$Interaction)
nrow(as.matrix(greenling_holiday_int))
hist(greenling_holiday_int)
Time_holiday_greenling<-sum(greenling_holiday$Total_time_elapsed/60)

greenling_holiday %>% dplyr::filter(Interaction>0)->greenling_holiday_nz

greenling_holiday_int_nz<-as.numeric(greenling_holiday_nz$Interaction)
nrow(as.matrix(greenling_holiday_int_nz))
hist(greenling_holiday_int_nz)

Time_holiday_greenling_nz<-sum(greenling_holiday_nz$Total_time_elapsed/60)

################################ Gunnel ########################################
holiday_dat%>% dplyr::filter(Effector_species %like% "gun") -> gunnel_holiday
#View(gunnel_holiday)

gunnel_holiday_int<-as.numeric(gunnel_holiday$Interaction)
nrow(as.matrix(gunnel_holiday_int))
hist(gunnel_holiday_int)
Time_holiday_gunnel<-sum(gunnel_holiday$Total_time_elapsed/60)

gunnel_holiday %>% dplyr::filter(Interaction>0)->gunnel_holiday_nz

gunnel_holiday_int_nz<-as.numeric(gunnel_holiday_nz$Interaction)
nrow(as.matrix(gunnel_holiday_int_nz))
hist(gunnel_holiday_int_nz)

Time_holiday_gunnel_nz<-sum(gunnel_holiday_nz$Total_time_elapsed/60)

################################# Rockfish #####################################
holiday_dat%>% dplyr::filter(Effector_species %like% "rock") -> rockfish_holiday
#View(rockfish_holiday)

rockfish_holiday_int<-as.numeric(rockfish_holiday$Interaction)
nrow(as.matrix(rockfish_holiday_int))
hist(rockfish_holiday_int)
Time_holiday_rockfish<-sum(rockfish_holiday$Total_time_elapsed/60)

rockfish_holiday %>% dplyr::filter(Interaction>0)->rockfish_holiday_nz

rockfish_holiday_int_nz<-as.numeric(rockfish_holiday_nz$Interaction)
nrow(as.matrix(rockfish_holiday_int_nz))
hist(rockfish_holiday_int_nz)
Time_holiday_rockfish_nz<-sum(rockfish_holiday_nz$Total_time_elapsed/60)

###############################################################################################
dev.new()
par(mfrow=c(3,1))

hist(hc_trident_int)
hist(hc_long_int)
hist(hc_holiday_int)

dev.new()
par(mfrow=c(3,1))

hist(cod_trident_int)
hist(cod_long_int)
hist(cod_holiday_int)

dev.new()
par(mfrow=c(3,1))

hist(sculpin_trident_int)
hist(sculpin_long_int)
hist(sculpin_holiday_int)

dev.new()
par(mfrow=c(3,1))

hist(greenling_trident_int)
hist(greenling_long_int)
hist(greenling_holiday_int)

dev.new()
par(mfrow=c(3,1))

hist(gunnel_trident_int)
hist(gunnel_long_int)
hist(gunnel_holiday_int)

dev.new()
par(mfrow=c(3,1))

hist(rockfish_trident_int)
hist(rockfish_long_int)
hist(rockfish_holiday_int)

########################


times_trident<-as.matrix(cbind(Time_trident_hc,Time_trident_cod,Time_trident_sculpin,Time_trident_greenling,Time_trident_gunnel,Time_trident_rockfish))
colnames(times_trident)<-c("Hermit crabs","Cod","Sculpins","Greenling","Gunnel","Rockfish")

times_long<-as.matrix(cbind(Time_long_hc,Time_long_cod,Time_long_sculpin,Time_long_greenling,Time_long_gunnel,Time_long_rockfish))
colnames(times_long)<-c("Hermit crabs","Cod","Sculpins","Greenling","Gunnel","Rockfish")

times_holiday<-as.matrix(cbind(Time_holiday_hc,Time_holiday_cod,Time_holiday_sculpin,Time_holiday_greenling,Time_holiday_gunnel,Time_holiday_rockfish))
colnames(times_holiday)<-c("Hermit crabs","Cod","Sculpins","Greenling","Gunnel","Rockfish")

times_trident
times_long
times_holiday

############################################################################################
barplot(times_trident,ylab="Total time in seconds", main= "Trident Basin")
barplot(times_long,ylab="Total time in seconds", main= "Long Island")
barplot(times_holiday,ylab="Total time in seconds", main= "Holiday Island")
 
#############################################################################################
############################### Non-zero interactions ######################################
times_trident_nz<-as.matrix(cbind(Time_trident_hc_nz,Time_trident_cod_nz,Time_trident_sculpin_nz,Time_trident_greenling_nz,Time_trident_gunnel_nz,Time_trident_rockfish_nz))
colnames(times_trident_nz)<-c("Hermit crabs","Cod","Sculpins","Greenling","Gunnel","Rockfish")

times_long_nz<-as.matrix(cbind(Time_long_hc_nz,Time_long_cod_nz,Time_long_sculpin_nz,Time_long_greenling_nz,Time_long_gunnel_nz,Time_long_rockfish_nz))
colnames(times_long_nz)<-c("Hermit crabs","Cod","Sculpins","Greenling","Gunnel","Rockfish")

times_holiday_nz<-as.matrix(cbind(Time_holiday_hc_nz,Time_holiday_cod_nz,Time_holiday_sculpin_nz,Time_holiday_greenling_nz,Time_holiday_gunnel_nz,Time_holiday_rockfish_nz))
colnames(times_holiday_nz)<-c("Hermit crabs","Cod","Sculpins","Greenling","Gunnel","Rockfish")

##############################################################################################
barplot(times_trident_nz,ylab="Total time in seconds", main= "Trident Basin")
barplot(times_long_nz,ylab="Total time in seconds", main= "Long Island")
barplot(times_holiday_nz,ylab="Total time in seconds", main= "Holiday Island")

###############################################################################################
######################### Histograms ##########################################################
dev.new()
par(mfrow=c(3,4))

hist(hc_trident_int)
hist(hc_trident_int_nz)
hist(cod_trident_int)
hist(cod_trident_int_nz)
hist(sculpin_trident_int)
hist(sculpin_trident_int_nz)
hist(greenling_trident_int)
hist(greenling_trident_int_nz)
hist(gunnel_trident_int)
hist(gunnel_trident_int_nz)
hist(rockfish_trident_int)
hist(rockfish_trident_int_nz)

###############################
dev.new()
par(mfrow=c(3,4))

hist(hc_long_int)
hist(hc_long_int_nz)
hist(cod_long_int)
hist(cod_long_int_nz)
hist(sculpin_long_int)
hist(sculpin_long_int_nz)
hist(greenling_long_int)
hist(greenling_long_int_nz)
hist(gunnel_long_int)
hist(gunnel_long_int_nz)
hist(rockfish_long_int)
hist(rockfish_long_int_nz)

###############################
dev.new()
par(mfrow=c(3,4))

hist(hc_holiday_int)
hist(hc_holiday_int_nz)
hist(cod_holiday_int)
hist(cod_holiday_int_nz)
hist(sculpin_holiday_int)
hist(sculpin_holiday_int_nz)
hist(greenling_holiday_int)
hist(greenling_holiday_int_nz)
hist(gunnel_holiday_int)
hist(gunnel_holiday_int_nz)
hist(rockfish_holiday_int)
hist(rockfish_holiday_int_nz)
