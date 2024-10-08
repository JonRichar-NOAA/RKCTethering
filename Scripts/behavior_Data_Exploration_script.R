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
library(lubridate)
##################################################################################
################################## To seconds function ##########################
### Source : https://stackoverflow.com/questions/10835908/is-there-a-way-to-convert-mmss-00-to-seconds-00

toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2) 
               i[1]*60 + i[2]
             else if (length(i) == 1) 
               i[1]
           }  
    )  
  )  
} 
##################################################################################
################################## Add data #####################################

behave_j<-read_csv("./DATA/Behavior_J_proc.csv")
behave_a<-read_csv("./DATA/Behavior_A_proc.csv")

files_j<-read_csv("./DATA/Files_J.csv")
files_a<-read_csv("./DATA/Files_A.csv")

##################### Combine behavior data sets #################################
behav_dat00<-as_tibble(rbind(behave_j,behave_a))
behav_dat00 %>%
  mutate(Total_time_sec=Total_time_elapsed/60) ->behav_dat0

View(behav_dat0)

time<-(as.matrix(behav_dat0$Total_time_elapsed))
total_sec<-time/60

behav_dat<-as_tibble(cbind(behav_dat0,total_sec))
     

behav_dat%>%
  dplyr::distinct(Crab_behavior)->spec_behav_dat
spec_behav_dat

#####################################################################################################
########################### Behavior 0 ##############################################################
behav_code <-0

##################################################################################
names(behav_dat)
behav_dat %>%
  filter(Crab_behavior==behav_code) ->code0

View(code0)

############################ Total time for behavior code 0 ######################
Total_code_0<-sum(code0$total_sec)
Total_code_0

Total_code_0/60
########################## Behavior 0, Habitat 0 #################################

code0 %>%
  filter(Habitat==0) ->hab

Time_hab0_0<-sum(hab$total_sec)
Time_hab0_0

Time_hab0_0/60
########################## Behavior 0, Habitat 1 #################################

code0 %>%
  filter(Habitat==1) ->hab

Time_hab0_1<-sum(hab$total_sec)
Time_hab0_1

Time_hab0_1/60
########################## Behavior 0, Habitat 2 #################################

code0 %>%
  filter(Habitat==2) ->hab

Time_hab0_2<-sum(hab$total_sec)
Time_hab0_2

Time_hab0_2/60
########################## Behavior 0, Habitat 3 #################################

code0 %>%
  filter(Habitat==3) ->hab

Time_hab0_3<-sum(hab$total_sec)
Time_hab0_3

Time_hab0_3/60

########################## Behavior 0, Habitat 4 #################################

code0 %>%
  filter(Habitat==4) ->hab

Time_hab0_4<-sum(hab$total_sec)
Time_hab0_4

Time_hab0_4/60
#####################################################################################################
########################### Behavior 1 ##############################################################
behav_code <-1

##################################################################################
names(behav_dat)
behav_dat %>%
  filter(Crab_behavior==behav_code) ->code1

View(code1)

############################ Total time for behavior code 1 ######################
Total_code_1<-sum(code1$total_sec)
Total_code_1

########################## Behavior 1, Habitat 0 #################################

code1 %>%
  filter(Habitat==0) ->hab

Time_hab1_0<-sum(hab$total_sec)
Time_hab1_0

Time_hab1_0/60
########################## Behavior 1, Habitat 1 #################################

code1 %>%
  filter(Habitat==1) ->hab

Time_hab1_1<-sum(hab$total_sec)
Time_hab1_1

Time_hab1_1/60
########################## Behavior 1, Habitat 2 #################################

code1 %>%
  filter(Habitat==2) ->hab

Time_hab1_2<-sum(hab$total_sec)
Time_hab1_2

Time_hab1_2/60
########################## Behavior 1, Habitat 3 #################################

code1 %>%
  filter(Habitat==3) ->hab

Time_hab1_3<-sum(hab$total_sec)
Time_hab1_3

Time_hab1_3/60
########################## Behavior 1, Habitat 4 #################################

code1 %>%
  filter(Habitat==4) ->hab

Time_hab1_4<-sum(hab$total_sec)
Time_hab1_4

Time_hab1_4/60
#####################################################################################################
########################### Behavior 2 ##############################################################
behav_code <-2

##################################################################################
names(behav_dat)
behav_dat %>%
  filter(Crab_behavior==behav_code) ->code2

View(code2)

############################ Total time for behavior code 1 ######################
Total_code_2<-sum(code2$total_sec)
Total_code_2

########################## Behavior 2, Habitat 0 #################################

code2 %>%
  filter(Habitat==0) ->hab

Time_hab2_0<-sum(hab$total_sec)
Time_hab2_0

Time_hab2_0/60
########################## Behavior 2, Habitat 1 #################################

code2 %>%
  filter(Habitat==1) ->hab

Time_hab2_1<-sum(hab$total_sec)
Time_hab2_1

Time_hab2_1/60
########################## Behavior 2, Habitat 2 #################################

code2 %>%
  filter(Habitat==2) ->hab

Time_hab2_2<-sum(hab$total_sec)
Time_hab2_2

Time_hab2_2/60
########################## Behavior 2, Habitat 3 #################################

code2 %>%
  filter(Habitat==3) ->hab

Time_hab2_3<-sum(hab$total_sec)
Time_hab2_3

Time_hab2_3/60
########################## Behavior 2, Habitat 4 #################################

code2 %>%
  filter(Habitat==4) ->hab

Time_hab2_4<-sum(hab$total_sec)
Time_hab2_4

Time_hab2_4/60

#####################################################################################################
########################### Behavior 3 ##############################################################
behav_code <-3

##################################################################################
names(behav_dat)
behav_dat %>%
  filter(Crab_behavior==behav_code) ->code3

View(code3)

############################ Total time for behavior code 3 ######################
Total_code_3<-sum(code3$total_sec)
Total_code_3

########################## Behavior 3, Habitat 0 #################################

code3 %>%
  filter(Habitat==0) ->hab

Time_hab3_0<-sum(hab$total_sec)
Time_hab3_0

Time_hab3_0/60
########################## Behavior 3, Habitat 1 #################################

code3 %>%
  filter(Habitat==1) ->hab

Time_hab3_1<-sum(hab$total_sec)
Time_hab3_1

Time_hab3_1/60
########################## Behavior 3, Habitat 2 #################################

code3 %>%
  filter(Habitat==2) ->hab

Time_hab3_2<-sum(hab$total_sec)
Time_hab3_2

Time_hab3_2/60
########################## Behavior 3, Habitat 3 #################################

code3 %>%
  filter(Habitat==3) ->hab

Time_hab3_3<-sum(hab$total_sec)
Time_hab3_3

Time_hab3_3/60
########################## Behavior 3, Habitat 4 #################################

code3 %>%
  filter(Habitat==4) ->hab

Time_hab3_4<-sum(hab$total_sec)
Time_hab3_4

Time_hab3_4/60

#####################################################################################################
########################### Behavior 4 ##############################################################
behav_code <-4

##########################################################################################
names(behav_dat)
behav_dat %>%
  filter(Crab_behavior==behav_code) ->code4

View(code4)


############################ Total time for behavior code 4 #################################################
Total_code_4<-sum(code4$total_sec)
Total_code_4

########################## Behavior 4, Habitat 0 #################################

code4 %>%
  filter(Habitat==0) ->hab

Time_hab4_0<-sum(hab$total_sec)
Time_hab4_0

Time_hab4_4/60
########################## Behavior 4, Habitat 1 #################################

code4 %>%
  filter(Habitat==1) ->hab

Time_hab4_1<-sum(hab$total_sec)
Time_hab4_1

Time_hab4_1/60
########################## Behavior 4, Habitat 2 #################################

code4 %>%
  filter(Habitat==2) ->hab

Time_hab4_2<-sum(hab$total_sec)
Time_hab4_2

Time_hab4_2/60
########################## Behavior 4, Habitat 3 #################################

code4 %>%
  filter(Habitat==3) ->hab

Time_hab4_3<-sum(hab$total_sec)
Time_hab4_3

Time_hab4_3/60
########################## Behavior 4, Habitat 4 #################################

code4 %>%
  filter(Habitat==4) ->hab

Time_hab4_4<-sum(hab$total_sec)
Time_hab4_4

Time_hab4_4/60

#####################################################################################################
########################### Behavior 5 ##############################################################
behav_code <-5

##################################################################################
names(behav_dat)
behav_dat %>%
  filter(Crab_behavior==behav_code) ->code5

View(code5)

############################ Total time for behavior code 5 ######################
Total_code_5<-sum(code5$total_sec)
Total_code_5

########################## Behavior 5, Habitat 0 #################################

code5 %>%
  filter(Habitat==0) ->hab

Time_hab5_0<-sum(hab$total_sec)
Time_hab5_0

Time_hab5_0/60
########################## Behavior 5, Habitat 1 #################################

code5 %>%
  filter(Habitat==1) ->hab

Time_hab5_1<-sum(hab$total_sec)
Time_hab5_1

Time_hab5_1/60
########################## Behavior 5, Habitat 2 #################################

code5 %>%
  filter(Habitat==2) ->hab

Time_hab5_2<-sum(hab$total_sec)
Time_hab5_2

Time_hab5_2/60
########################## Behavior 5, Habitat 3 #################################

code5 %>%
  filter(Habitat==3) ->hab

Time_hab5_3<-sum(hab$total_sec)
Time_hab5_3

Time_hab5_3/60
########################## Behavior 5, Habitat 4 #################################

code5 %>%
  filter(Habitat==4) ->hab

Time_hab5_4<-sum(hab$total_sec)
Time_hab5_4

Time_hab5_4/60

#####################################################################################################
########################### Behavior 6 ##############################################################
behav_code <-6

##################################################################################
names(behav_dat)
behav_dat %>%
  filter(Crab_behavior==behav_code) ->code6

View(code6)

############################ Total time for behavior code 6 ######################
Total_code_6<-sum(code6$total_sec)
Total_code_6

########################## Behavior 6, Habitat 0 #################################

code6 %>%
  filter(Habitat==0) ->hab

Time_hab6_0<-sum(hab$total_sec)
Time_hab6_0

Time_hab6_0/60
########################## Behavior 6, Habitat 1 #################################

code6 %>%
  filter(Habitat==1) ->hab

Time_hab6_1<-sum(hab$total_sec)
Time_hab6_1

Time_hab6_1/60
########################## Behavior 6, Habitat 2 #################################

code6 %>%
  filter(Habitat==2) ->hab

Time_hab6_2<-sum(hab$total_sec)
Time_hab6_2

Time_hab6_2/60
########################## Behavior 6, Habitat 3 #################################

code6 %>%
  filter(Habitat==3) ->hab

Time_hab6_3<-sum(hab$total_sec)
Time_hab6_3

Time_hab6_3/60
########################## Behavior 6, Habitat 4 #################################

code6 %>%
  filter(Habitat==4) ->hab

Time_hab6_4<-sum(hab$total_sec)
Time_hab6_4

Time_hab6_4/60

#####################################################################################################
########################### Behavior 7 ##############################################################
behav_code <-7

##################################################################################
names(behav_dat)
behav_dat %>%
  filter(Crab_behavior==behav_code) ->code7

View(code7)

############################ Total time for behavior code 7 ######################
Total_code_7<-sum(code7$total_sec)
Total_code_7

########################## Behavior 7, Habitat 0 #################################

code7 %>%
  filter(Habitat==0) ->hab

Time_hab7_0<-sum(hab$total_sec)
Time_hab7_0

Time_hab7_0/60
########################## Behavior 7, Habitat 1 #################################

code7 %>%
  filter(Habitat==1) ->hab

Time_hab7_1<-sum(hab$total_sec)
Time_hab7_1

Time_hab7_1/60
########################## Behavior 7, Habitat 2 #################################

code7 %>%
  filter(Habitat==2) ->hab

Time_hab7_2<-sum(hab$total_sec)
Time_hab7_2

Time_hab7_2/60
########################## Behavior 7, Habitat 3 #################################

code7 %>%
  filter(Habitat==3) ->hab

Time_hab7_3<-sum(hab$total_sec)
Time_hab7_3

Time_hab7_3/60
########################## Behavior 7, Habitat 4 #################################

code7 %>%
  filter(Habitat==4) ->hab

Time_hab7_4<-sum(hab$total_sec)
Time_hab7_4

Time_hab7_4/60

########################### Behavior 999 ##############################################################
behav_code <-999

##################################################################################
names(behav_dat)
behav_dat %>%
  filter(Crab_behavior==behav_code) ->code999

View(code999)

############################ Total time for behavior code 999 ######################
Total_code_999<-sum(code999$total_sec)
Total_code_999

########################## Behavior 999, Habitat 0 #################################

code999 %>%
  filter(Habitat==0) ->hab

Time_hab999_0<-sum(hab$total_sec)
Time_hab999_0

Time_hab999_0/60
########################## Behavior 999, Habitat 1 #################################

code999 %>%
  filter(Habitat==1) ->hab

Time_hab999_1<-sum(hab$total_sec)
Time_hab999_1

Time_hab999_1/60
########################## Behavior 999, Habitat 2 #################################

code999 %>%
  filter(Habitat==2) ->hab

Time_hab999_2<-sum(hab$total_sec)
Time_hab999_2

Time_hab999_2/60
########################## Behavior 999, Habitat 3 #################################

code999 %>%
  filter(Habitat==3) ->hab

Time_hab999_3<-sum(hab$total_sec)
Time_hab999_3

Time_hab999_3/60
########################## Behavior 999, Habitat 4 #################################

code999 %>%
  filter(Habitat==4) ->hab

Time_hab999_4<-sum(hab$total_sec)
Time_hab999_4

Time_hab999_4/60

#####################################################################################################
########################### Behavior NA ##############################################################
behav_code <-"NA"

##################################################################################
names(behav_dat)

behav_dat[is.na(behav_dat$Crab_behavior),]->codeNA
View(codeNA)

############################ Total time for behavior code 999 ######################
Total_code_NA<-sum(codeNA$total_sec)
Total_code_NA


#########################################################################################################
#########################################################################################################
#########################################################################################################

Total_code_0
Total_code_1
Total_code_2
Total_code_3
Total_code_4
Total_code_5
Total_code_6
Total_code_7

Total_code_999
Total_code_NA
####################################################################################################
Time_hab0_0
Time_hab0_1
Time_hab0_2
Time_hab0_3
Time_hab0_4

Time_hab1_0
Time_hab1_1
Time_hab1_2
Time_hab1_3
Time_hab1_4

Time_hab2_0
Time_hab2_1
Time_hab2_2
Time_hab2_3
Time_hab2_4

Time_hab3_0
Time_hab3_1
Time_hab3_2
Time_hab3_3
Time_hab3_4

Time_hab4_0
Time_hab4_1
Time_hab4_2
Time_hab4_3
Time_hab4_4

Time_hab5_0
Time_hab5_1
Time_hab5_2
Time_hab5_3
Time_hab5_4

Time_hab6_0
Time_hab6_1
Time_hab6_2
Time_hab6_3
Time_hab6_4

Time_hab7_0
Time_hab7_1
Time_hab7_2
Time_hab7_3
Time_hab7_4

Time_hab999_0
Time_hab999_1
Time_hab999_2
Time_hab999_3
Time_hab999_4



