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

## this list of months will be used to filter the dataframe for what we want. 
##this list needs to be compeleted regardless if the original dataframe only 
##contains all relevant records.

list_of_Months <- c("April","October")

##the below dataframe summarizes by tmc and month and provides
##some summary statistics

Data_Charactor <- scott %>%
  group_by(tmc,
           month=as.factor(months.POSIXt(ceiling_date(lubridate,"month"))))%>%
  ##Grouping using lubridate (as above) provides neater code and more functionality.
  ##commented out factor group. 
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

##unsure of what 1320 is reffering to here...
x <- 1320
Data_Charactor$perc <- Data_Charactor$count/x


## We want to remove all speed points that are classed as outliers.
## we join the Data_Charactor dataframe to the original data. 
## This new dataset provides the percentiles by TMC.
## The filter willretain all the speeds that are 15%>X<85%

scott_outlier <- scott %>% 
  merge(Data_Charactor,by.y = "tmc")%>%
  filter(speed>='15%',
         speed<='85%')%>%
  arrange(speed)


## Group by each tmc and each hour and returm the number of records and 
##the mean speed.
Data_sum <- scott_outlier %>%
  group_by(tmc,
           ##month=as.factor(months.POSIXt(ceiling_date(lubridate,"month"))),
           hour = as.factor(format(lubridate,"%H")))%>%
  summarise(count = n(),mean_speed_by_hour = mean(speed))%>%
  group_by(tmc)%>%
  summarise(Congestion_Speed=mean(mean_speed_by_hour))
  
##some exploratory plots
##d <- ggplot(Data_Charactor,aes(month,speed_std))+
##  geom_boxplot(aes(group=month))+
##  geom_point()+
##  geom_jitter(width = 0.35,height = 0.25)
##ggplotly(d)

##ggplot(scott[1:75000,],aes(x=speed))+
##  geom_histogram(bins = 75)+
##  facet_wrap(~tmc,nrow = 5)

county_of_interest <- c("Scott")


## We merge the summarised dataframe 'Data_sum' with all of the information 
## for each tmc located in TMC_ALL and filter for our relevant county.
Data_Sum_merge <- as_tibble(merge(Data_sum,TMC_All,by.y = "tmc"))%>%
  filter(county==county_of_interest)

## Rearrange the data.frame to get FFS beside 
Data_Sum_merge <- Data_Sum_merge[c(1,
                                   2,
                                   14,
                                   3:(length(colnames(Data_Sum_merge))-2))]







