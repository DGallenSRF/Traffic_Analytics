library(lubridate)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(scales)
library(stringr)
library(forecast)




setwd("H:/Projects/11000/11187/TS/Task 2")

TMC_all <- readxl::read_xlsx("TMC_All.xlsx")

list_csv <- list.files(pattern = "*.csv")
myfiles <- lapply(list_csv,read.csv)
files <- mapply(cbind,myfiles,'filename' = list_csv,SIMPLIFY = F)

TMC_data <- do.call("rbind",files)

dim(TMC_data)

TMC_data$Travel.Time..minutes. <- as.numeric(TMC_data$Travel.Time..minutes.)
TMC_data$lubridate <- as.POSIXct(TMC_data$Timestamp,format = "%Y-%m-%d %H:%M:%S")
TMC_data$hour <- as.factor(hour(TMC_data$lubridate))
TMC_data$month <- as.factor(month(TMC_data$lubridate))
TMC_data$year <- as.factor(year(TMC_data$lubridate))
TMC_data$weekday <- factor(weekdays(TMC_data$lubridate),levels = c("Monday","Tuesday","Wednesday",
                                                         "Thursday","Friday","Saturday","Sunday"))
TMC_data$day <- as.factor(day(TMC_data$lubridate))

head(TMC_data)

dat <- TMC_data %>%
  filter(filename == paste('US-169_Northbound_118P05065_to_118P05066','.csv',sep=))

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
  summarise(Mean=mean((Travel.Time..minutes.)),
            Max=max((Travel.Time..minutes.)),
            Min=min((Travel.Time..minutes.)),
            Std=sd((Travel.Time..minutes.)),
            Median=median((Travel.Time..minutes.))) %>%
  arrange(day)

##x <- dat[dat$year == 2014 &
           dat$month==9 &
           dat$day == 15 &
           !is.na(dat$Travel.Time..minutes.),2]
##hist(x,breaks=40)


Mean_Travel$sm <- ma(Mean_Travel$Mean,order=7)=
Mean_Travel

##plot(Mean_Travel$diff)

f <- ggplot(Mean_Travel)+
  geom_line(aes(day,sm),color='red')+
  #geom_line(aes(day,Time),linetype="dashed",size=.5)+
  #geom_line(aes(day,Median),color='red')+
  scale_x_datetime(breaks = date_breaks("1 month"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
ggplotly(f)

ts_Time = ts(Mean_Travel$Mean, frequency = 365,start = c(2014,1))
decompose_Time = decompose(ts_Time, "additive")

#plot(as.ts(decompose_Time$seasonal))
#plot(as.ts(decompose_Time$trend))
#plot(as.ts(decompose_Time$random))
my_plot.decomposed.ts(decompose_Time,"Analysis of Mean Travel Time per Day")

Mean_Travel$seasonal <- as.numeric((decompose_Time$seasonal))

arrange(Mean_Travel,desc(seasonal))%>% print(n=40)
hist(Mean_Travel$seasonal)

my_plot.decomposed.ts = function(x, title="", ...) {
  xx <- x$x
  if (is.null(xx)) 
    xx <- with(x, if (type == "additive") 
      random + trend + seasonal
      else random * trend * seasonal)
  plot(cbind(observed = xx, trend = x$trend, seasonal = x$seasonal, random = x$random), 
       main=title, ...)
}



