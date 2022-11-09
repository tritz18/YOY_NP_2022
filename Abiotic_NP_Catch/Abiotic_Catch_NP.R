#### Packages ####
library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(tidymodels)
library(tidyr)
tidymodels_prefer()

###################

setwd("/Users/thorn/OneDrive/Desktop/YOY_NP_2022/R_Project/YOY_NP_2022/Abiotic_NP_Catch/")
YOY_NP<- read_excel("YOY_NP_Final.xlsx")
YOY_NP$Site<- as.factor(YOY_NP$Site)
YOY_NP$Date<- as.POSIXct(YOY_NP$Date,  format="%Y-%m-%d")
YOY_NP$Type<- as.factor(YOY_NP$Type)
YOY_NP$Location<-as.factor(YOY_NP$Location)


Abiotic<- read_excel("Abiotic_Final_2022.xlsx")
Abiotic$Site<- as.factor(Abiotic$Site)
Abiotic$Date<- as.POSIXct(Abiotic$Date,  format="%Y-%m-%d")
Abiotic$Type<- as.factor(Abiotic$Type)
Abiotic$Location<-as.factor(Abiotic$Location)

Abiotic<- Abiotic %>% 
  filter(Date >= "2022-06-14" & Date <= "2022-07-01" )

Abiotic_Catch <- left_join(Abiotic, YOY_NP, by = c("Date", "Location", 
                                                   "Site", "Type")) %>% 
  group_by(Date) %>% 
  mutate(Study_Day = cur_group_id())


ggplot(Abiotic_Catch, aes(PER_SAT_mean, count, color=Type))+
  geom_point()+geom_smooth(method="lm")+
  facet_wrap(~Site, scales="free_y")

