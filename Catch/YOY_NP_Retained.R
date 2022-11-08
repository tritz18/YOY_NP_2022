#### Packages ####
library(lubridate)
library(stats)
library(fpp3)
library(fracdiff)
library(factoextra)
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


#################### Calculate fish condition from retained fish ######
YOY_NP<- YOY_NP %>% 
mutate(K=Weight/(Length^3)*100,0000) %>% 
  group_by(Date) %>% 
  mutate(Study_Day = cur_group_id())

ggplot(YOY_NP, aes(Length, Weight))+
  geom_point()+
  geom_smooth(method="lm")+
stat_regline_equation(aes(alpha=0.5, label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
                      label.x = 0, label.y =0, formula = y~x)+
  facet_wrap(~Net)


ggplot(YOY_NP, aes(Length))+
  geom_bar(stat="count")+
  facet_wrap(~Site)


#### summary dataset ####

YOY_NP_Sum<- YOY_NP %>% 
  mutate(K=Weight/(Length^3)*100,0000) %>% 
group_by(across(c(Site, Type))) %>% 
  summarize(Mean_length=mean(Length), length_sd=sd(Length), 
            Mean_K=mean(K), K_SD=sd(K))





ggplot(YOY_NP_Sum, aes(Type, Mean_length, fill=Type))+
  geom_col(position="dodge", color="black")+
  geom_errorbar(aes(ymin=Mean_length-length_sd, ymax=Mean_length+length_sd),
                position=position_dodge(.9),width=.3)+
  ylab("Length (mm) ± SD")+
  scale_fill_manual(values = c("#009E73","#0072B2"))+
  theme_bw()+scale_y_continuous(breaks=seq(0,140,20),limits=c(0,140), minor_breaks = seq(0,140,10))+
  theme(legend.position = "bottom", 
        legend.text = element_text(size=15), legend.title = element_blank(),
        axis.text.y = element_text(size=14), axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        strip.text = element_text(size=15),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14))+
  scale_color_manual(values = c("#009E73","#0072B2"))+
  ggtitle("Young-of-year Northern Pike 2022")+
  facet_wrap(~Site)
ggsave("YOY_NP_Length_2022.png", dpi=300, width = 6, height = 5)



