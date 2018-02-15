library(lubridate)
library(magrittr)
library(dplyr)

setwd("H:/Projects/11000/11187/TS/Task 1")

scott_read <- read.csv("scott_county_and_supplement.csv",stringsAsFactors = FALSE)

str(scott_read)

scott <- scott_read
scott$lubridate <- as.POSIXct(scott$date_time,format = "%m/%d/%Y %H:%M")

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

Data_Charactor <- scott %>%
  group_by(tmc,months=ceiling_date(lubridate,"month"))%>%
  ##group_by(tmc,month)%>%
  summarise(count = n())%>%
  arrange(desc(count))
Data_Charactor

Data_Charactor_merge <- as_tibble(merge(Data_Charactor,TMC_All,by.y = "tmc"))
x <- 1320
Data_Charactor_merge$percent <- Data_Charactor_merge$count/x

View(Data_Charactor_merge##[Data_Charactor_merge$county=="Scott",]
     )
View(scott)