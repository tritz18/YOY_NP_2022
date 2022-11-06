#### Packages ####
library(lubridate)
library(tidyverse)
library(readxl)
library(writexl)
library(tidymodels)
library(tidyr)
tidymodels_prefer()

###################

#### Set working directory and files 
setwd("/Users/thorn/OneDrive/Desktop/YOY_NP_2022/R_Project/YOY_NP_2022/Catch/")
YOY_NP<- read_excel("2022_Emigration_Esocids.xlsx")
YOY_NP$Site<- as.factor(YOY_NP$Site)
YOY_NP$Date<- as.POSIXct(YOY_NP$Date,  format="%Y-%m-%d")
YOY_NP$Type<- as.factor(YOY_NP$Type)

YOY_NP_Final<- YOY_NP %>% 
  group_by(across(c(Site, Date, Type, Net))) %>% 
  summarize(count=n(), mean_length=mean(Length), length_sd=sd(Length)) 

write_xlsx(YOY_NP_Final, "YOY_NP_Final.xlsx")
   

ggplot(YOY_NP, aes(Type, mean_length, fill=Type))+
  geom_col(position="dodge", color="black")+
  geom_errorbar(aes(ymin=mean_length-length_sd, ymax=mean_length+length_sd),
                position=position_dodge(.9),width=.3)+
  ylab("Length (mm)")+
  scale_fill_manual(values = c("#009E73","#0072B2"))+
  theme_bw()+scale_y_continuous(breaks=seq(0,120,20),limits=c(0,120))+
  theme(legend.position = "none",
        axis.text = element_text(size=13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13))
ggsave("YOY_NP_Length_2022.png", dpi = 300, height = 4, width=4)
