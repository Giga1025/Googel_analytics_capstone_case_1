## PREPARE
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(hydroTSM)
library(janitor)


## reading all the tables of data for a complete year starting from july22 to july23
## NOTE: Your file path would be different from mine, so using the link provided in the READ.ME, download the data files and upload their paths, apart from this the rest of the code is same.

july22<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202207-divvy-tripdata.csv")
aug22<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202208-divvy-tripdata.csv")
sept22<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202209-divvy-publictripdata.csv")
oct22<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202210-divvy-tripdata.csv")
nov22<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202211-divvy-tripdata.csv")
dec22<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202212-divvy-tripdata.csv")
jan23<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202301-divvy-tripdata.csv")
feb23<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202302-divvy-tripdata.csv")
mar23<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202303-divvy-tripdata.csv")
apr23<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202304-divvy-tripdata.csv")
may23<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202305-divvy-tripdata.csv")
jun23<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202306-divvy-tripdata.csv")
july23<-read.csv("C:/Users/byash/OneDrive/Desktop/capstone files/202307-divvy-tripdata.csv")

full_data<-rbind(july22,aug22,sept22, oct22, nov22, dec22,jan23,feb23,mar23,apr23,may23,jun23,july23) ## stacking all the tables one top of other
rows_before_cleaning<-nrow(full_data)                                                                 
remove(july22,aug22,sept22, oct22, nov22, dec22,jan23,feb23,mar23,apr23,may23,jun23,july23)         ## removing the individual tables since all of them had been binded into one

## PROCESS

full_data<-remove_empty(full_data, which = c("cols"))              ## removing empty columns
full_data<-remove_empty(full_data, which = c("rows"))              ##  removing empty rows

full_data<-distinct(full_data)                                     ## removing any duplicate rows in the dable
full_data1 <- full_data[!duplicated(full_data$ride_id), ]          

full_data %>% colnames()
full_data<-full_data %>% select(-c(start_station_name, end_station_name, start_lat, start_lng, end_lat, end_lng))  ## de-selecting the columns that are not required
colnames(full_data)

unique(full_data$rideable_type)  ## checking unique values in the column



full_data$started_at <- as.POSIXct(full_data$started_at, format = "%Y-%m-%d %H:%M:%S")  ##converting start time to date format
full_data$ended_at <- as.POSIXct(full_data$ended_at, format = "%Y-%m-%d %H:%M:%S")      ## converting end time to date format
full_data<- full_data %>% mutate(ride_duration = difftime(ended_at, started_at, units = "mins")) ## getting the ride duration
full_data$ride_duration<-as.integer(full_data$ride_duration)
full_data <-full_data %>%                                                               ## adding the year-month(month) section to the data table
  mutate(year_month = paste(strftime(full_data$started_at, "%Y"),
                            "-",
                            strftime(full_data$started_at, "%m"),
                            paste("(",strftime(full_data$started_at, "%b"), ")", sep="")))
unique(full_data$year_month)
full_data <- full_data %>%                                                             ## converting started_at to started_hour and adding to table
  mutate(start_hour = strftime(full_data$started_at, "%H"))

full_data<- full_data %>% mutate(day = weekdays(full_data$started_at))                 ## adding days column to the table
full_data<- full_data %>% filter(ride_duration>0) %>% drop_na()                        ## eliminating -ve ride_duration with filter
write.csv(full_data, file = "cyclist(jul22-july23).csv")

##plot

full_data %>% ggplot(aes(x = member_casual, fill = member_casual))+ geom_bar()  ## bar plot for member types and total number customers(count), with filling member type

## creating a plot for year_month and number of customers(count) using coordinate flipping and adjusting the title to the middle with the help of theme
full_data %>% ggplot(aes(year_month, fill = member_casual))+ geom_bar()+ labs(x = "Months", y = " customers", title = " Trips according to months")+ coord_flip()+
  theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5)) +ggtitle("Trips According to Months")

## table for total usage count , bikes usage by both the types of customers and their difference in each month
month_customers1 <- full_data %>%
  group_by(year_month) %>%
  summarise(
    count = n(),
    members = sum(member_casual == 'member'),
    casuals = sum(member_casual == 'casual')
  ) %>%
  mutate(
    '%' = (count / nrow(full_data)) * 100,
    'members' = (members / count) * 100,
    'casuals' = (casuals / count) * 100,
    'diff_in_percent' = members - casuals
  )

full_data %>% ggplot(aes(x = year_month, fill = member_casual))+ geom_bar()+coord_flip()  ##year_month and member_casual

## Getting the difference in percentage of the customers using the month_customers1 table in all the months
month_customers1 %>% ggplot(aes(x = diff_in_percent, y = year_month,fill = factor(sign(diff_in_percent)))) +
  geom_bar(stat = "identity")+scale_fill_manual(values = c("1" = "blue", "-1" = "red"),labels = c("members"), name = "Difference in the percentage customers")



## table for total usage count , bikes usage by both the types of customers and their difference in each day of the week
day_wise_customers <- full_data %>%
  group_by(day) %>%
  summarise(
    count = n(),
    members = sum(member_casual == 'member'),
    casuals = sum(member_casual == 'casual')
  ) %>%
  mutate(
    '%' = (count / nrow(full_data)) * 100,
    'members' = (members / count) * 100,
    'casuals' = (casuals / count) * 100,
    'diff_in_percent' = members - casuals
  )

full_data %>% ggplot(aes(x = day, fill = member_casual))+ geom_bar(position = "dodge") ## bar plot for usage of bikes by both the customers in each day of the week

## Getting the difference in percentage of the customers using the data_wise_customers table in all the days of the week
day_wise_customers %>% ggplot(aes(x = diff_in_percent, y = day,fill = factor(sign(diff_in_percent)))) +
  geom_bar(stat = "identity")+scale_fill_manual(values = c("1" = "blue", "-1" = "red"),labels = c("More_Casuals", "More_members"), name = "Difference in the customers")

## table for total usage count , bikes usage by both the types of customers and their difference in every hour of the day
start_hour_customers <- full_data %>%
  group_by(start_hour) %>%
  summarise(
    count = n(),
    members = sum(member_casual == 'member'),
    casuals = sum(member_casual == 'casual')
  ) %>%
  mutate(
    '%' = (count / nrow(full_data)) * 100,
    'members' = (members / count) * 100,
    'casuals' = (casuals / count) * 100,
    'diff_in_percent' = members - casuals
  )
unique(start_hour_customers)




full_data %>% ggplot(aes(x = start_hour, fill = member_casual))+ geom_bar(position = "dodge")+facet_wrap(~day) ## bar plots for usage of bikes by both the customers in each day of the week, we get mupliple plot for each day as we  are using the facet_wrap function

## Getting the difference in percentage of the customers using the data_wise_customers table in all the hours of the day
start_hour_customers %>% ggplot(aes(x = diff_in_percent, y = start_hour,fill = factor(sign(diff_in_percent)))) +
   geom_bar(stat = "identity")+scale_fill_manual(values = c("1" = "blue", "-1" = "red"),labels = c("More_Casuals", "More_members"), name = "Difference in the customers")
 
## table for total usage count , bikes usage by both the types of customers and their difference by different ride type
 rides_type <- full_data %>%
   group_by(rideable_type) %>%
   summarise(
     count = n(),
     members = sum(member_casual == 'member'),
     casuals = sum(member_casual == 'casual')
   ) %>%
   mutate(
     '%' = (count / nrow(full_data)) * 100,
     'members' = (members / count) * 100,
     'casuals' = (casuals / count) * 100,
     'diff_in_percent' = members - casuals
   )
 
 full_data %>% ggplot(aes(x = rideable_type, fill = member_casual))+ geom_bar(position = "dodge") ## bar plot of rideable_types to the customers

 summary(full_data$ride_duration) ## getting the summary of the ride_duration column

quin = quantile(full_data$ride_duration, seq(0,1,by = 0.01)) ## getting the value for every 1 quarterly taking 0 as start 1(100) as end increment by 0.01(1)

full_data_filtered <- full_data %>%                           ## since the last 1% of the data has very high values, we only consider from 0-99% and store in a new data frame
  filter(ride_duration > as.numeric(quin['1%'])) %>%
  filter(ride_duration < as.numeric(quin['99%']))

print(paste("Removed", nrow(full_data) - nrow(full_data_filtered), "rows as outliners" ))  ## printing the number of rows that had been filterred out.

## getting the mean ride duration from the both the category of cusstomers using bar plot.
ggplot(full_data_filtered, aes(x = member_casual, y = ride_duration, fill = member_casual)) +                     
  labs(x = "Member x Casual", y = "Riding time", title = "Distribution of Mean Riding time for Casual x Member") +
  geom_bar(stat = "summary", fun = "mean") +
  theme_minimal()


