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


files_j<-read_csv("./DATA/Files_J.csv")
files_a<-read_csv("./DATA/Files_A.csv")

files_dat0<-as_tibble(rbind(files_j,files_a))
View(files_dat0)
hist(as.numeric(files_dat0$Substrate))

hist(as.numeric(files_dat0$Biogenics))

date_cam<-as.matrix(paste0(files_dat0$Date_set_out,"_",files_dat0$Camera_and_Station))
date_cam

files_dat1<-cbind(files_dat0,date_cam)
View(files_dat1)

files_dat1 %>%
  distinct(date_cam, Substrate, Biogenics) -> files_dat

files_dat %>%
  distinct(date_cam, Substrate) -> Sub_dat
hist(Sub_dat$Substrate)

files_dat %>%
  distinct(date_cam,Biogenics) ->Biogenic_dat

View(Biogenic_dat)
hist(as.numeric(Biogenic_dat$Biogenics))
