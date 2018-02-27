library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(scales)
library(stringr)
library(forecast)
library(ggseas)

install.packages("ggseas")

setwd("H:/Projects/11000/11187/TS/Task 2")

dat <- read.csv(dir()[2],stringsAsFactors = FALSE)

dim(dat)

dat$Travel.Time..minutes. <- as.numeric(dat$Travel.Time..minutes.)
dat$lubridate <- as.POSIXct(dat$Timestamp,format = "%Y-%m-%d %H:%M:%S")
dat$hour <- as.factor(hour(dat$lubridate))
dat$month <- as.factor(month(dat$lubridate))
dat$year <- as.factor(year(dat$lubridate))
dat$weekday <- factor(weekdays(dat$lubridate),levels = c("Monday","Tuesday","Wednesday",
                                                         "Thursday","Friday","Saturday","Sunday"))
dat$day <- as.factor(day(dat$lubridate))

head(dat)

##NAs per hour
dat[!is.na(dat$lubridate),] %>% 
  group_by(hour) %>%
  summarise(Count = sum(is.na(Travel.Time..minutes.)),
            Total = n(),
            Percent_NA=Count/Total) %>%
  mutate(positionInCategory = 1:n())%>%
  ggplot(aes(x=hour,y=Percent_NA))+
  geom_bar(stat="identity")


##disstribution of NAs per day between 5am and 9pm

dat[!is.na(dat$lubridate)&dat$hour %in% c(5:21),] %>% 
  group_by(weekday,year) %>%
  summarise(Count = sum(is.na(Travel.Time..minutes.)),
            Total = n(),
            Percent_NA=Count/Total) %>%
  mutate(positionInCategory = 1:n())%>%
  ggplot(aes(x=weekday,y=Percent_NA,fill=year))+
  geom_bar(stat="identity")+
  facet_wrap(~year)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        legend.position = 'none')

##plot of average travel time per day
Mean_Travel <- dat[!is.na(dat$Travel.Time..minutes.),] %>% 
  group_by(day=(floor_date(lubridate,"day"))) %>%
  summarise(Time=mean((Travel.Time..minutes.)),
            Max=max((Travel.Time..minutes.)),
            Min=min((Travel.Time..minutes.)),
            Std=sd((Travel.Time..minutes.)),
            Median=median((Travel.Time..minutes.))) %>%
  arrange(day) %>%
  print()

Mean_Travel$sm <- ma(Mean_Travel$Time,order=7)
Mean_Travel$trend <- Mean_Travel$Time - Mean_Travel$sm

f <- ggplot(Mean_Travel)+
  geom_line(aes(day,sm),color='red')+
  #geom_line(aes(day,Time),linetype="dashed",size=.5)+
  #geom_line(aes(day,Median),color='red')+
  scale_x_datetime(breaks = date_breaks("1 month"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
ggplotly(f)

ts_Time = ts(Mean_Travel$Time, frequency = 365,start = c(2014,1))
decompose_Time = decompose(ts_Time, "additive")

#plot(as.ts(decompose_Time$seasonal))
#plot(as.ts(decompose_Time$trend))
#plot(as.ts(decompose_Time$random))
plot(decompose_Time)



  ## plot of totoal NAs per day from 5am to 9pm
g <- dat[dat$hour %in% c(5:21),] %>%
  ##dat %>%
  group_by(day=(floor_date(lubridate,"day"))) %>%
  summarise(Count = sum(str_count(Travel.Time..minutes., "N/A")),
            Total = n(),
            Percent_NA=round(Count/Total,2)) %>%
  mutate(positionInCategory = 1:n())%>%
  arrange((day))

p <- ggplot(g,aes(day,Percent_NA))+
  geom_line()+
  geom_point()+
  scale_x_datetime(breaks = date_breaks("1 day"),
                   labels = date_format("%a"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
p


