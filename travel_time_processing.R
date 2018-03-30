library(magrittr)
library(scales)
library(stringr)
library(forecast)
library(leaflet)
library(tidyverse)
library(reshape2)
library(plotly)
library(lubridate)
library(kableExtra)
library(gridExtra)
options(knitr.table.format = "html") 



setwd("H:/Projects/11000/11187/TS/Task 2")

TMC_all <- readxl::read_xlsx("TMC_All.xlsx")

summary(TMC_all)
TMC_all <- TMC_all[!is.na(TMC_all$distance),]


list_csv <- list.files(pattern = "*.csv")
myfiles <- lapply(list_csv,read.csv)
files <- mapply(cbind,myfiles,'filename' = list_csv,SIMPLIFY = F)

TMC_data <- rbind_list(files)

dim(TMC_data)[1]

TMC_data %<>% 
  mutate(Travel.Time..minutes. = as.numeric(Travel.Time..minutes.),
         lubridate =as.POSIXct(TMC_data$Timestamp,format = "%Y-%m-%d %H:%M:%S"))
TMC_data %<>% 
  mutate(hour = as.factor(hour(TMC_data$lubridate)),
         month = as.factor(month(TMC_data$lubridate)),
         year = as.factor(year(TMC_data$lubridate)),
         weekday = factor(weekdays(TMC_data$lubridate),
                          levels = c("Monday","Tuesday",
                                     "Wednesday","Thursday",
                                     "Friday","Saturday","Sunday")),
         day = as.factor(day(TMC_data$lubridate)),
         filename_split = filename) 

TMC_data %<>% 
  separate(filename_split,into = c('Route','Direction','From','NA','To'),sep='_')
TMC_data$To <- sub(".csv",'',TMC_data$To)

TMC_data_merge <- merge(TMC_data,TMC_all[,c('tmc','latitude','longitude')], 
                        by.x = 'From',by.y='tmc',all.x=TRUE)
colnames(TMC_data_merge)[which(names(TMC_data_merge) 
                               %in% c('latitude','longitude'))] <- c('from_latitude','from_longitude')

TMC_data_merge <- merge(TMC_data_merge,TMC_all[,c('tmc','latitude','longitude')], 
                        by.x = 'To',by.y='tmc',all.x=TRUE)
colnames(TMC_data_merge)[which(names(TMC_data_merge) 
                               %in% c('latitude','longitude'))] <- c('to_latitude','to_longitude')

TMC_Unique <- TMC_data_merge[!duplicated(TMC_data_merge$To),c('To','From',
                                                              "from_latitude","from_longitude",
                                                              "to_latitude","to_longitude")]

TMC_GPS1 <- melt(TMC_Unique,measure.vars=c('From'),
                 id.vars=c('from_latitude','from_longitude'),
                 value.name = 'TMC')
TMC_GPS2 <- melt(TMC_Unique,measure.vars=c('To'),
                 id.vars=c('to_latitude','to_longitude'),
                 value.name = 'TMC')

colnames(TMC_GPS1) <- c('Lat','Long','Variable','TMC')
colnames(TMC_GPS2) <- c('Lat','Long','Variable','TMC')
TMC_GPS <- bind_rows(TMC_GPS1,TMC_GPS2)
TMC_GPS$label <- paste(TMC_GPS$Variable,':',TMC_GPS$TMC,sep = '') 


###dataset
dat <- TMC_data_merge %>%
  filter(filename == paste('MN-41_Northbound_118P05380_to_118P05381','.csv',sep=''))

dat_GPS1 <- melt(dat[1,],measure.vars=c('From'),
                id.vars=c('from_latitude','from_longitude'),
                value.name = 'TMC')
dat_GPS2 <- melt(dat[1,],measure.vars=c('To'),
                     id.vars=c('to_latitude','to_longitude'),
                     value.name = 'TMC')

colnames(dat_GPS1) <- c('Lat','Long','Variable','TMC')
colnames(dat_GPS2) <- c('Lat','Long','Variable','TMC')
dat_GPS <- bind_rows(dat_GPS1,dat_GPS2)
dat_GPS$label <- paste(dat_GPS$Variable,':',dat_GPS$TMC,sep = '') 


##NAs per hour
d <- dat[!is.na(dat$lubridate),] %>% 
  group_by(hour) %>%
  summarise(Count = sum(is.na(Travel.Time..minutes.)),
            Total = n(),
            Percent_NA=Count/Total) %>%
  mutate(positionInCategory = 1:n())%>%
  ggplot(aes(x=hour,y=Percent_NA))+
  geom_bar(stat="identity")+
  scale_y_continuous(name="% of Hourly Readings are NA")


##disstribution of NAs per day between 5am and 9pm

c <-dat[!is.na(dat$lubridate)&dat$hour %in% c(5:21),] %>% 
  group_by(weekday,year) %>%
  summarise(Count = sum(is.na(Travel.Time..minutes.)),
            Total = n(),
            Percent_NA=Count/Total) %>%
  mutate(positionInCategory = 1:n())


p2 <-  nPlot(Count ~ year, group = 'weekday', data = c, type = 'multiBarChart')
p2$chart(color = c('brown', 'blue', '#594c26', 'green'))
p2
  
  ggplot(aes(x=weekday,y=Percent_NA,fill=year))+
  geom_bar(stat="identity")+
  facet_wrap(~year)+
  scale_y_continuous(name="% of Daily Readings are NA")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        legend.position = 'none',
        plot.title = element_text(hjust=0.5))
  



##plot of average travel time per day
Mean_Travel <- dat[!is.na(dat$Travel.Time..minutes.),] %>% 
  group_by(day=(floor_date(lubridate,"day"))) %>%
  summarise(Mean=mean((Travel.Time..minutes.)),
            Max=max((Travel.Time..minutes.)),
            Min=min((Travel.Time..minutes.)),
            Std=sd((Travel.Time..minutes.)),
            Median=median((Travel.Time..minutes.))) %>%
  arrange(day)
Mean_Travel$sm <- ma(Mean_Travel$Mean,order=365)



colnames(Mean_Travel) %in% colnames(Mean_Travel[,2:7])

##plot(Mean_Travel$diff)

f <- ggplot(Mean_Travel)+
  geom_line(aes(day,sm),color='red')+
  geom_line(aes(day,Mean),linetype="dashed",size=.5)+
  #geom_line(aes(day,Median),color='red')+
  scale_x_datetime(breaks = date_breaks("1 month"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
ggplotly(f)

ts_Time = ts(Mean_Travel$Mean, frequency = 365,start = c(2014))
decompose_Time = decompose(ts_Time, "additive")

#plot(as.ts(decompose_Time$seasonal))
#plot(as.ts(decompose_Time$trend))
#plot(as.ts(decompose_Time$random))
par(family = 'sans',font=10,las=3,pty='m')
my_plot.decomposed.ts(decompose_Time)
title(main = "US-169_Southbound_118N04465_to_118N04464\n Mean Travel Time Series Analysis")


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

head(dat)

m <- leaflet(data=dat_GPS) %>% 
  setView(lng = -93.554508, lat =  44.766168, zoom = 11)

Traffic_camera <- makeIcon('Traffic_camera.png',iconWidth = 24, iconHeight = 20)

m %>% 
  addTiles() %>% 
  addMarkers(~Long,~Lat,icon=Traffic_camera,label = ~label) %>%
  addProviderTiles(providers$OpenStreetMap)

