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
tidymodels_prefer()

###################

#### Set working directory and files 
#setwd("/Users/thorn/OneDrive/Desktop/YOY_NP_2022/R_Project/YOY_NP_2022/")#
f = list.files(pattern="*.xlsx")

#### Clean logger data ####
Abiotic_Raw <- purrr::map_df(f, function(x) {
  mydata <- read_excel(x)
  mydata$Date_Time <- as.POSIXct(mydata$Date_Time,  format="%Y-%m-%d")
  mydata$Location<- as.factor(mydata$Location)
  mydata$Type <-as.factor(mydata$Type)
  mydata %>%
    filter(Date_Time < "2022-07-01" & Date_Time > "2022-06-13") %>%
    select(-PRESSURE) %>% 
    mutate(Date = as.Date(Date_Time))
})

#######################

#### Abiotic hours dataset creation ####
Abiotic_Hrs<- Abiotic_Raw %>% 
  group_by(across(c(Date, Location))) %>%
  select(DO, TEMP) %>%
  summarise(Hyp_Hrs=sum(DO<3.01), Nor_Hrs=sum(DO>3.00),
            Temp_Hrs=sum(TEMP>28))

#### Summarize abiotic data ####
Abiotic_Sum<- Abiotic_Raw %>% 
  group_by(across(c(Date, Location))) %>% 
  select(DO, TEMP) %>% 
  summarise(across(everything(), list(mean = mean, max = max, min=min, sd=sd), .names = "{col}_{fn}")) 

#################################

#### join data sets ####

Abiotic_Final<- left_join(Abiotic_Hrs,Abiotic_Sum, by=c("Date", "Location")) %>%
  group_by(Date) %>% 
  mutate(Study_Day = cur_group_id())
