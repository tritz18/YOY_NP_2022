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
Abiotic_Sum_Type<- Abiotic_Raw %>% 
  group_by(across(c(Type))) %>% 
  select(DO, TEMP, PER_SAT) %>% 
  summarise(across(everything(), list(mean = mean, max = max, min=min, sd=sd), .names = "{col}_{fn}")) 


ggplot(Abiotic_Sum_Type, aes(Type,PER_SAT_mean, fill=Type, group=Type))+
  geom_bar(stat="identity",position=position_dodge(), color="black")+ theme_bw()+
  geom_errorbar(aes(ymin=PER_SAT_mean-PER_SAT_sd, ymax=PER_SAT_mean+PER_SAT_sd),
        position=position_dodge(.9),width=.3)+
  theme(legend.position = "none", 
        axis.text = element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14))+
  scale_fill_manual(values = c("#009E73","#0072B2"))+
  scale_y_continuous(breaks = seq(0,100,25), limits = c(0,100))+
  ylab("Daily % Oxygen Saturation ± SD")

ggsave("persat_type.png", dpi=300, height = 6, width = 4)


Abiotic_Final

ggplot(Abiotic_Sum_Type, aes(Type,TEMP_mean, fill=Type, group=Type))+
  geom_bar(stat="identity",position=position_dodge(), color="black")+ theme_bw()+
  geom_errorbar(aes(ymin=TEMP_mean-TEMP_sd, ymax=TEMP_mean+TEMP_sd),
                position=position_dodge(.9),width=.3)+
  theme(legend.position = "none", 
        axis.text = element_text(size=14),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14))+
  scale_fill_manual(values = c("#009E73","#0072B2"))+
  scale_y_continuous(breaks = seq(0,24,2), limits = c(0,24))+
  ylab("Daily Water Temperature ± SD")

ggsave("Water_TEMP_type.png", dpi=300, height = 6, width = 4)




ggplot(Abiotic_Final, aes(Location,PER_SAT_mean, color=Type,
  group=interaction(Location, Type)))+
  geom_boxplot(fill="grey")+ theme_bw()+
  theme(legend.position = "bottom", legend.text = element_text(size=14), 
              legend.title = element_blank(),
              strip.text = element_text(size=14),
              axis.text = element_text(size=14),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=14))+
  scale_color_manual(values = c("#009E73","#0072B2"))+
  scale_y_continuous(breaks = seq(0,125,25), limits = c(0,125))+
  ylab("Daily % Oxygen Saturation")+
  facet_wrap(~Wetland, scales = "free_x")

ggsave("persat_boxplot_2022.png", dpi=300, height = 6, width = 10)

ggplot(Abiotic_Final, aes(Location,PER_SAT_mean, color=Type,
                          group=interaction(Wetland, Location)))+
  geom_col()+ theme_bw()+
  scale_color_manual(values = c("#009E73","#0072B2"))+
  #scale_y_continuous(breaks = seq(0,150,25))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none",
        axis.text = element_text(size=12))+
  facet_wrap(~Wetland, scales = "free_x")



#### M chemistry table ####

Otolith_M_Chemistry_Summary<- Abiotic_Final %>% 
  group_by(across(c(Location, Wetland))) %>% 
  select(DO_mean, PER_SAT_mean, Hyp_Hrs, Wetland) %>% 
  summarise(Mean_Daily_DO=mean(DO_mean), 
            Mean_Daily_Per_Sat=mean(PER_SAT_mean), Mean_Hyp_Hrs=mean(Hyp_Hrs)) %>% 
  filter(Location %in% c("CM_REF5", "CM_SP2", "FC_REF6", "FC_SP5", "FC_REF6", 
                         "CHIP_REF3", "CHIP_SP2", "PV_REF2", "PV_SP4")) %>% 
  mutate(N=5, .after=Wetland) %>% 
  group_by(Wetland) %>% 
  arrange(Wetland, desc(Mean_Daily_Per_Sat)) %>% 
gt() %>% fmt_number(columns = 4:6 ,decimals = 2) %>% 
  tab_header(
    title = md("**Otolith Microchemistry Selection**")) %>% 
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(rows = Mean_Daily_Per_Sat > 50.5)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "red"),
      cell_text(color = "white")
    ),
    locations = cells_body(rows = Mean_Daily_Per_Sat < 50.5)
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  cols_label(
    Mean_Daily_DO = "Mean Daily DO",
    Mean_Daily_Per_Sat = "Mean Daily Percent Saturation",
   Mean_Hyp_Hrs = "Mean Hypoxic Hours"
  ) %>% 
  cols_width(everything() ~ px(150)) 
Otolith_M_Chemistry_Summary %>% 
gtsave("Otolith_Microchemistry Table.png")
