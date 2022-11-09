###### LM of retained LW relationship to predict #####
###### weight of captured YOY NP 2022 #######

library(lubridate)
library(broom)
library(tidyverse)
library(readxl)
library(tidymodels)
library(ggpubr)
tidymodels_prefer()

###################

#### Set working directory and files 
setwd("/Users/thorn/OneDrive/Desktop/YOY_NP_2022/R_Project/YOY_NP_2022/Catch/")
YOY_NP<- read_xlsx("YOY_NP_Retained_2022.xlsx")
YOY_NP$Site<- as.factor(YOY_NP$Site)
YOY_NP$Date<- as.POSIXct(YOY_NP$Date,  format="%Y/%m/%d")
YOY_NP$Type<- as.factor(YOY_NP$Type)
YOY_NP$Length<-as.numeric(YOY_NP$Length)
YOY_NP$Weight<-as.numeric(YOY_NP$Weight)


YOY_NP_Catch<- read_xlsx("2022_Emigration_Esocids.xlsx")
YOY_NP_Catch$Site<- as.factor(YOY_NP_Catch$Site)
YOY_NP_Catch$Date<- as.POSIXct(YOY_NP_Catch$Date,  format="%Y/%m/%d")
YOY_NP_Catch$Type<- as.factor(YOY_NP_Catch$Type)

YOY_NP_Catch <- YOY_NP_Catch %>% 
  filter(Species %in% "NP")

LM_model<-
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit<- 
  LM_model %>% 
  fit(Weight ~ Length, data= YOY_NP)

lm_form_fit %>% extract_fit_engine() %>% vcov()

model_res<- lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

YOY_NP_Length <- YOY_NP_Catch %>% 
  select(Length)

Weight_Pred <- predict(lm_form_fit, new_data = YOY_NP_Length)

Predicted_LW <- YOY_NP_Catch %>% 
  bind_cols(Weight_Pred) %>% 
  select(-Notes, -...9, -Species, -Weight) %>% 
  rename(Pred_Weight=.pred) %>% 
  filter(Length > 47) %>% 
  mutate(K=Pred_Weight/(Length^3)*100,0000) %>% 
  group_by(Date) %>% 
  mutate(Study_Day = cur_group_id()) %>% 
  group_by(across(c(Site,Type))) %>% 
  summarize(Mean_length=mean(Length), length_sd=sd(Length), 
            Mean_K=mean(K), K_SD=sd(K))

ggplot(Predicted_LW, aes(Mean_length,Mean_K))+
  geom_point()+
  geom_smooth(method="lm")+
  stat_regline_equation(aes(alpha=0.5, label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
                        label.x = 1, label.y =0.00055, formula = y~x)


ggplot(Predicted_LW, aes(Site, Mean_K))+
  geom_bar(stat="identity")+
  facet_wrap(~Type)
