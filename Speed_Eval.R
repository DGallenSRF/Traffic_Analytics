library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)

setwd("H:/Projects/11000/11187/TS/Task 1")

scott_read <- read.csv("scott_county_and_supplement.csv",stringsAsFactors = FALSE)

str(scott_read)

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

str(scott)


TMC_All <- readxl::read_excel("TMC_Scott.xlsx",sheet = 1)

str(TMC_All)

Month1 <- "April"
Month2 <- "October"
quantile_prob <- c(0.05,0.15,.5,0.85,0.95)
quantile_perc <- c("5%","15%","50%","85%","95%")
quantiles <- data.frame(quantile_prob,quantile_perc)


Data_Charactor <- scott %>%
  group_by(tmc,month=ceiling_date(lubridate,"month"))%>%
  ##group_by(tmc,month)%>%
  summarise(count = n(), 
            speed_std = sd(speed),
            speed_mean = mean(speed),
            speed_median = median(speed),
            dist =  mean(distance))%>%
  arrange((speed_std))
Data_Charactor

ggplot(Data_Charactor)+
  geom_boxplot(aes)

ggplot(scott[1:75000,],aes(x=speed))+
  geom_histogram(bins = 75)+
  facet_wrap(~tmc,nrow = 5)

Data_Charactor_merge <- as_tibble(merge(Data_Charactor,TMC_All,by.y = "tmc"))

##unsure of what 1320 is refering to...
x <- 1320

## it was unclear where count was coming from in "Speed_DB". "Sheet 2" looks to be doing some group bys, but the April and
## Oct counts don't seem to match
Data_Charactor_merge$percent <- Data_Charactor_merge$count/x

View(Data_Charactor_merge) ##[Data_Charactor_merge$county=="Scott",])







