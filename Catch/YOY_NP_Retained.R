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
YOY_NP<- YOY_NP %>% 
mutate(K=Weight/(Length^3)*100,0000)


ggplot(YOY_NP, aes(Length, K))+
  geom_point()+
  geom_smooth(method="lm")+
stat_regline_equation(aes(alpha=0.5, label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")")), 
                      label.x = 69, label.y =0.0009, formula = y~x)+
  facet_wrap(~Site)

YOY_NP_Sum<- YOY_NP %>% 
  mutate(K=Weight/(Length^3)*100,0000) %>% 
group_by(across(c(Site, Type))) %>% 
  summarize(Mean_length=mean(Length), length_sd=sd(Length), 
            Mean_K=mean(K), K_SD=sd(K))





ggplot(YOY_NP_Sum, aes(Type, Mean_K, fill=Type))+
  geom_col(position="dodge", color="black")+
  geom_errorbar(aes(ymin=Mean_K-K_SD, ymax=Mean_K+K_SD),
                position=position_dodge(.9),width=.3)+
  ylab("Length (mm)")+
  scale_fill_manual(values = c("#009E73","#0072B2"))+
  theme_bw()+#scale_y_continuous(breaks=seq(0,140,20),limits=c(0,140), minor_breaks = seq(0,140,10))+
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
