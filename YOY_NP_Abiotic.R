#### Packages ####
library(lubridate)
library(stats)
library(fpp3)
library(broom)
library(fracdiff)
library(factoextra)
library(tidyverse)
library(readxl)
library(tidymodels)
library(rMR)
tidymodels_prefer()

###################

#### Set working directory and files 
#setwd("/Users/thorn/OneDrive/Desktop/YOY_NP_2022/R_Project/YOY_NP_2022/")#
f = list.files(pattern="*.xlsx")

#### Clean logger data ####
Abiotic_Raw <- purrr::map_df(f, function(x) {
  mydata <- read_excel(x)
  mydata$Date_Time <- as.POSIXct(mydata$Date_Time,  format="%Y-%m-%d %H:%M")
  mydata$Location<- as.factor(mydata$Location)
  mydata$Wetland<- as.factor(mydata$Wetland)
  mydata$Type <-as.factor(mydata$Type)
  mydata %>%
    filter(Date_Time < "2022-07-01" & Date_Time > "2022-05-01") %>%
    mutate(Date = as.Date(Date_Time)) %>% 
    select(-Date_Time)
})

######################

#### Percent saturation + add to dataset ####
PERCENT_SAT<- DO.unit.convert(Abiotic_Raw$DO, DO.units.in = "mg/L", 
                              DO.units.out = "pct", bar.units.in = "kpa", bar.units.out = "kpa", bar.press = Abiotic_Raw$PRESSURE,
                              temp.C = Abiotic_Raw$TEMP, salinity = 0.5)

Abiotic_Raw<- bind_cols(Abiotic_Raw, PERCENT_SAT)%>% 
  rename(PER_SAT=...8)

#### Abiotic hours dataset creation ####
Abiotic_Hrs<- Abiotic_Raw %>% 
  group_by(across(c(Date, Location, Type, Wetland))) %>%
  select(DO, TEMP) %>%
  summarise(Hyp_Hrs=sum(DO<3.01), Nor_Hrs=sum(DO>3.00),
            Temp_Hrs=sum(TEMP>28))

#### Summarize abiotic data ####
Abiotic_Sum<- Abiotic_Raw %>% 
  group_by(across(c(Date, Location, Type, Wetland))) %>% 
  select(DO, TEMP, PER_SAT) %>% 
  summarise(across(everything(), list(mean = mean, max = max, min=min, sd=sd), .names = "{col}_{fn}")) 

#################################

#### join data sets ####

Abiotic_Final<- left_join(Abiotic_Hrs,Abiotic_Sum, by=c("Date", "Location", "Type", "Wetland")) %>%
  group_by(Date) %>% 
  mutate(Study_Day = cur_group_id())



##### Plotting #####

ggplot(Abiotic_Final, aes(Location,PER_SAT_mean, color=Type,
  group=interaction(Wetland, Location)))+
  geom_boxplot(fill="grey")+ theme_bw()+
  scale_color_manual(values = c("#009E73","#0072B2"))+
  scale_y_continuous(breaks = seq(0,150,25))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none",
        axis.text = element_text(size=12))+
  facet_wrap(~Wetland, scales = "free_x")
ggsave("persat.png", dpi=300, height = 8, width = 8)
