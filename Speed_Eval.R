library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)


setwd("H:/Projects/11000/11187/TS/Task 1")

scott_read <- read.csv("scott_county_and_supplement.csv",stringsAsFactors = FALSE)

##str(scott_read)

scott <- scott_read
scott$lubridate <- as.POSIXct(scott$date_time,format = "%m/%d/%Y %H:%M")
scott$speed <- scott$distance/(scott$travel_time_all/3600)

## We will try to use the lubridate column to filter for specific time periods
## in case of emergency break glass below

##scott$month <- as.factor(months.POSIXt(scott$lubridate))
##scott$day <- as.factor(weekdays.POSIXt(scott$lubridate))
##cott$hour <- as.factor(format(scott$lubridate,"%H"))
##scott$min <- as.factor(format(scott$lubridate,"%M"))
##scott$speed <- scott$distance/(scott$travel_time_all/3600)

##str(scott)


TMC_All <- readxl::read_excel("TMC_Scott.xlsx",sheet = 1)

##str(TMC_All)

list_of_Months <- c("April","October")

##unsure of what 1320 is refering to...
x <- 1320


Data_Charactor <- scott %>%
  group_by(tmc,month=as.factor(months.POSIXt(ceiling_date(lubridate,"month"))))%>%
  ##group_by(tmc,month)%>%
  summarise(count = n(),
            speed_std = sd(speed),
            speed_mean = mean(speed),
            speed_median = median(speed),
            speed_max = max(speed),
            '5%' = quantile(speed,probs = 0.05),
            '15%' = quantile(speed,probs = 0.15),
            '50%' = quantile(speed,probs = 0.5),
            '85%' = quantile(speed,probs = 0.85),
            '95%' = quantile(speed,probs = 0.95),
            dist =  mean(distance))%>%
  filter(month %in% list_of_Months)%>%
  arrange(desc(speed_max))
Data_Charactor$perc <- Data_Charactor$count/x
Data_Charactor



##some exploratory plots
##d <- ggplot(Data_Charactor,aes(month,speed_std))+
##  geom_boxplot(aes(group=month))+
##  geom_point()+
##  geom_jitter(width = 0.35,height = 0.25)
##ggplotly(d)


##ggplot(scott[1:75000,],aes(x=speed))+
##  geom_histogram(bins = 75)+
##  facet_wrap(~tmc,nrow = 5)

Data_Charactor_merge <- as_tibble(merge(Data_Charactor,TMC_All,by.y = "tmc"))




View(Data_Charactor_merge) ##[Data_Charactor_merge$county=="Scott",])







