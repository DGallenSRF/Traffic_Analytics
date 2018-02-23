library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(scales)
library(stringr)



dat <- read.csv(dir()[3])

dim(dat[(dat$Timestamp=='N/A'),])
dim(dat)

dat[is.na(dat$lubridate),]

head(dat)

dat$lubridate <- as.POSIXct(dat$Timestamp,format = "%Y-%m-%d %H:%M:%S")
dat$hour <- as.factor(hour(dat$lubridate))
dat$month <- as.factor(month(dat$lubridate))
dat$year <- as.factor(year(dat$lubridate))
dat$weekday <- factor(weekdays(dat$lubridate),levels = c("Monday","Tuesday","Wednesday",
                                                         "Thursday","Friday","Saturday","Sunday"))
dat$day <- as.factor(day(dat$lubridate))

head(dat$month)

##NAs per hour
dat[!is.na(dat$lubridate),] %>% 
  group_by(hour) %>%
  summarise(Count = sum(str_count(Travel.Time..minutes., "N/A")),
            Total = n(),
            Percent_NA=Count/Total) %>%
  mutate(positionInCategory = 1:n())%>%
  print() %>%
  ggplot(aes(x=hour,y=Percent_NA,fill=hour))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  scale_color_gradient(low = "#132B43", high = "#56B1F7")


##disstribution of NAs per day between 5am and 9pm

dat[!is.na(dat$lubridate)&dat$hour %in% c(5:21),] %>% 
  group_by(weekday,year) %>%
  summarise(Count = sum(str_count(Travel.Time..minutes., "N/A")),
            Total = n(),
            Percent_NA=Count/Total) %>%
  mutate(positionInCategory = 1:n())%>%
  print() %>%
  ggplot(aes(x=weekday,y=Percent_NA,fill=weekday))+
  geom_bar(stat="identity")+
  facet_wrap(~year)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  scale_color_gradient(low = "#132B43", high = "#56B1F7")

##plot of average travel time per day
dat[dat$Travel.Time..minutes.!='N/A',] %>% 
  group_by(day=(floor_date(lubridate,"day"))) %>%
  summarise(Time=mean(as.numeric(Travel.Time..minutes.)),
            Max=max(as.numeric(Travel.Time..minutes.)),
            Min=min(as.numeric(Travel.Time..minutes.)),
            Std=sd(as.numeric(Travel.Time..minutes.)),
            Median=median(as.numeric(Travel.Time..minutes.))) %>%
  arrange(desc(Time)) %>%
  ggplot()+
  geom_line(aes(day,Time))+
  geom_line(aes(day,Median),color='red')+
  scale_x_datetime(breaks = date_breaks("1 month"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

## plot of totoal NAs per day from 5am to 9pm
g <- dat[dat$hour %in% c(5:21),] %>%
  ##dat %>%
  group_by(day=(floor_date(lubridate,"day"))) %>%
  summarise(Count = sum(str_count(Travel.Time..minutes., "N/A")),
            Total = n(),
            Percent_NA=round(Count/Total,2)) %>%
  mutate(positionInCategory = 1:n())%>%
  arrange((day))

p <- ggplot(g[1000:1750,],aes(day,Percent_NA))+
  geom_line()+
  geom_point()+
  scale_x_datetime(breaks = date_breaks("1 day"),
                   labels = date_format("%a"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
ggplotly(p,tooltip = "Percent_NA")


