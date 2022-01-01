# load libraries
library(tidyverse)
library(reshape2)
library(data.table)
library(zoo)
library(hms)


# library(skimr)

setwd("~/../Joshua PFDS/r/coursework/data")

asTime <- function(x, format) {
   strptime(x, "%H%M") %>% 
    format(format) %>% 
    as_hms()
  }
# as.ITime(format = "%H:%M")}


# load data
plane_data <- read.csv("plane-data.csv", header = TRUE)
carriers <- read.csv("carriers.csv", header = TRUE)
airports <- read.csv("airports.csv", header = TRUE)
fd_1995 <- read.csv("1995.csv", header = TRUE)
fd_1996 <- read.csv("1996.csv", header = TRUE)
#fd_1997 <- read.csv("1997.csv", header = TRUE)
#fd_1998 <- read.csv("1998.csv", header = TRUE)
#fd_1999 <- read.csv("1999.csv", header = TRUE)
#fd_2000 <- read.csv("2000.csv", header = TRUE)

fd_raw <- rbind(fd_1995, fd_1996)

# Converting time observations to time objects
# flight_data_raw$CRSDepDateTime <- sprintf("%04d", flight_data_raw$CRSDepTime) %>% 
#   asTime() %>%
#   paste(flight_data_raw$Date, .) %>% 
#   as.POSIXct(format = "%Y-%m-%d %H:%M", tz = "GMT")
# 
# flight_data_raw$CRSDepTime <- sprintf("%04d", flight_data_raw$CRSDepTime) %>% 
#   asTime()
#   
# flight_data_raw$CRSArrTime <- sprintf("%04d", flight_data_raw$CRSArrTime) %>% 
#   asTime()
# 
# flight_data_raw$DepTime <- sprintf("%04d", flight_data_raw$DepTime) %>% 
#   asTime()
# 
# flight_data_raw$ArrTime <- sprintf("%04d", flight_data_raw$ArrTime) %>% 
#   asTime()

# write.csv(flight_data_raw, "flight_data.csv", row.names = FALSE)

# load raw data (on-time data year 1995)
# flight_data_raw <- read.csv("flight_data.csv", header = TRUE)  

# Before we begin, lets check the number of missing values in each column
missing_values <- fd_raw %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  t() %>% as.data.frame() %>% 
  rename(count = 1) %>% 
  filter(count > 0) %>% 
  arrange(count) 

# We can create a visualization of the missing values with a bar chart, and
# set a filter to remove any variables with NA = total number of observations.
missing_values %>% 
  rownames_to_column() %>% 
  filter(count < as.numeric(count(fd_raw))) %>% 
  ggplot() +
  geom_bar(aes(x=rowname, y=count), stat = 'identity') +
  labs(x='variable', y="number of missing values", 
       title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# It appears that there might be some connections between the missing values across
# the variables. Intuitively, cancelled or diverted flights might explain for the missing
# values. Lets check the number of cancelled or diverted flights.
fd_raw %>% 
  count(Cancelled == 1)

fd_raw %>% 
  count(Cancelled == 1 | Diverted == 1 )

missing_values

# As observed, there are some connections between the number of cancelled or diverted flights
# and the missing values. More specifically, the number of missing values in departure 
# and arrival time.

# Before we proceed, we will drop the records containing missing values.
fd_comp <- fd_raw %>% 
  filter(Cancelled == 0, Diverted == 0) %>% 
  select(-TaxiIn, -TaxiOut, -CancellationCode, -CarrierDelay, 
         -WeatherDelay, -NASDelay, -SecurityDelay, -LateAircraftDelay, 
         -Distance, -FlightNum, -ActualElapsedTime, -CRSElapsedTime)

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

# # Check the structure of our data and the number of records  
# glimpse(fd_comp)
# 
# # Check for the number of unique Tail Numbers
# fd_comp %>% 
#   group_by(TailNum) %>% 
#   summarise(Count = n()) %>% 
#   arrange(desc(Count))
# 
# # Let's look at the planes (identified by their tail number) that have the highest average delays
# tail_delay <- fd_comp %>% 
#   group_by(TailNum) %>% 
#   summarise(AvgArrDelay = mean(ArrDelay)) %>% 
#   arrange(desc(AvgArrDelay))

-----------------------------------------------------------------------------------------------

# When is the best time of the day, day of the week, and time of the year to fly to minimize
# delays?  
  
# We calculate for the number of arrival delays with a 15-minute "grace period" given. 
# A flight would be considered delayed only if the arrival delay exceed 15-minutes so as 
# to speak. Lets create a delay indicator.
fd_comp <- fd_comp %>% 
  mutate(DepDel15 = ifelse(DepDelay >= 15, 1, 0),
         ArrDel15 = ifelse(ArrDelay >= 15, 1, 0),
         Delayed = ifelse(DepDel15 == 1 | ArrDel15 == 1, 1, 0)) %>% 
  select(-Cancelled, -Diverted)  


# Intuitively, flights can arrive on-time despite a late departure, hence we will be using
# ArrDel15 (arrival delay > 15) as our indicator for delay to analyze the best time to travel.
# First, we create a new variable - season. 
fd_comp <- fd_comp %>% 
  mutate(season = ifelse(Month %in% 3:5, "Spring",
                  ifelse(Month %in% 6:8, "Summer",
                  ifelse(Month %in% 9:11, "Autumn", "Winter"))) %>% 
           factor(levels = c("Winter", "Spring", "Summer", "Autumn")))

depdel_mth <- fd_comp %>%  
  select(DayofMonth, Month, season, ArrDel15)

# Lets visualize the best season to travel.
depdel_mth %>% 
  group_by(DayofMonth, season) %>% 
  summarise(per_del = mean(ArrDel15)) %>% 
  ggplot(aes(DayofMonth, per_del, group = season)) +
  geom_line(aes(color = season), size = 1) +
  labs(title = "Percentage of Arrival Delay by Season",
       x = "Day of Month",
       y = "Percentage of Arrival Delay") +
  theme_get() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_discrete(limits = c(1:31)) +
  scale_y_continuous(labels = scales::percent)

# From the graph, Autumn - most bottom line - has the least percentage of flights delayed.
# During the Autumn season, the day with the least amount of delays seems to be within
# the first week of the months.

------------------------------------------------------------------------------------------------
  
# depdel_mth %>%
#   group_by(DayofMonth, season) %>%
#   summarise(per_del = mean(ArrDel15)) %>%
#   ggplot(aes(DayofMonth, per_del)) +
#   geom_bar(stat = "identity", aes(fill = per_del)) +
#   geom_text(aes(label = paste(round(per_del, 4)*100, "%")),
#             hjust = 1.2, angle = 90) +
#   scale_fill_gradientn(colors = rev(colorspace::heat_hcl(2))) +
#   labs(title = "Percentage of Arrival Delay by Season",
#        x = "Day of Month",
#        y = "Percentage of Arrival Delay") +
#   theme_classic() +
#   theme(plot.title = element_text(size = 15, hjust = 0.5)) +
#   scale_x_discrete(limits = c("Week 1", rep("", 6), "Week 2", rep("", 6),
#                               "Week 3", rep("", 6), "Week 4", rep("", 6),
#                               "Week 5")) +
#   scale_y_continuous(labels = scales::percent) +
#   guides(fill = guide_legend(title = "Percentage")) +
#   facet_wrap(~ season)

------------------------------------------------------------------------------------------------

depdel_mth %>% 
  group_by(Month) %>%
  summarise(per_del = mean(ArrDel15)) %>% 
  ggplot(aes(Month, per_del)) +
  geom_bar(stat = "identity", aes(fill = per_del)) +
  geom_text(aes(label = paste(round(per_del, 4)*100, "%")),
            vjust = -1) +
  scale_fill_gradientn(colors = rev(colorspace::heat_hcl(2))) +
  labs(title = "Percentage of Arrival Delay by Month",
       x = "Month",
       y = "Percentage of Arrival Delay") +
  theme_get() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(title = "Percentage"))  
  
# The best Month to travel are in April, May (Spring) and September (Autumn) with chance 
# delay flights on arrival - 18.05%, 18.82% and 16.54% respectively.

# Now let us look at which day of the week is the best to travel.
depdel_wk <- fd_comp %>%
  select(DayOfWeek, season, ArrDel15)

depdel_wk %>% 
  group_by(DayOfWeek, season) %>%  
  summarise(delay_count = sum(ArrDel15),
            per_del = mean(ArrDel15)) %>% 
  ggplot(aes(DayOfWeek, per_del)) +
  geom_bar(stat = "identity", aes(fill = per_del)) +
  geom_text(aes(label = paste(round(per_del, 4)*100, "%")),
            vjust = -1, size = 3) +
  scale_fill_gradientn(colors = rev(colorspace::heat_hcl(4))) +
  labs(title = "Percentage of Arrival Delay by Day of Week",
       x = "Day of Week",
       y = "Percentage of Arrival Delay") +
  theme_get() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(title = "Percentage")) +
  facet_wrap(~ season)

# Its best to fly on the weekends. Most notably, the best day of week to fly during Autumn
# season is on the Saturday(s), with a low of 15.13% of flights delayed. 
# Summary: Winter: Sunday   - 23.06%
#          Spring: Saturday - 16.38%
#          Summer: Saturday - 19.30%
#          Autumn: Saturday - 15.13%


# Using CRSDepTime (Scheduled Departure Time) with every hour binned as a group (24 groups in
# total).

depdel_tm <- fd_comp %>%
  mutate(CRSDepTime = sprintf("%04d", fd_comp$CRSDepTime) %>%
           asTime(format = "%H:00:00")) %>%
  select(CRSDepTime, ArrDel15) %>%
  filter(ArrDel15 == 1) %>%
  group_by(CRSDepTime) %>%
  summarise(delay_count = n())

# Lets create a visualization - Number of Arrival Delays versus CRSDepTime
depdel_tm %>%
  ggplot(aes(x = CRSDepTime, y = delay_count)) +
  geom_line(color="#69b3a2", size = 1.5) +
  labs(title = "Delay versus Scheduled Departure Time",
    x = "Schedule Departure Time",
    y = "Number of Arrival Delays") +
  theme_get() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_time(labels = scales::label_time(format = "%H:%M"),
               breaks = hms(hours = seq(0, 23, 1)))

# According to the chart, flight delays were low in the early morning and picked up after 10:00
# with the most number of delays occurring between 16:00-19:00. The delays decline towards
# late night.
# Minimize the chance of flight delays by traveling in the morning and avoid flights that
# depart between 16:00-19:00.

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

# # Since we have looked at the number of arrival delays by departure time, let us now study 
# # the average arrival delay time.   
# depdel_tm2 <- fd_comp %>%
#   mutate(CRSDepTime = sprintf("%04d", fd_comp$CRSDepTime) %>%
#            asTime(format = "%H:%M:00")) %>%
#   filter(ArrDelay >= 15) %>%
#   group_by(CRSDepTime) %>%
#   summarise(avg_del = mean(ArrDelay))
# 
# # Alternate visualization - Average Arrival Delayed Time (min) versus CRSDepTime
# depdel_tm2 %>%
#   ggplot(aes(CRSDepTime, avg_del)) +
#   geom_point(color = "#69b3a2", size = 1.2) +
#   geom_smooth(method = "loess", col = "#CC6666", size = 1.5, se = FALSE) +
#   geom_hline(yintercept = 15) +
#   labs(title = "Average delay (minutes) versus Scheduled Departure Time",
#        x = "Schedule Departure Time",
#        y = "Average Arrival Delayed Time (minutes)") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         plot.title = element_text(size = 15, hjust = 0.5)) +
#   scale_x_time(labels = scales::label_time(format = "%H:%M"),
#                breaks = hms(hours = seq(0, 23, 1))) +
#   scale_y_continuous(breaks = sort(c(seq(min(depdel_tm2$avg_del),
#                                          max(depdel_tm2$avg_del),
#                                          length.out=5), 15)))
#    
# # Once again, we were able to observe from the graph that the average arrival delayed time 
# # is the highest for flights that are scheduled between 16:00-19:00.
# # In general, expected arrival delay is between 30-45 minutes. 
  
-----------------------------------------------------------------------------------------------
  
# Do older planes suffer more delays than newer planes?
# Lets check the structure of the data before we begin   
glimpse(plane_data)

# Coerce issue_date to date class
plane_data$issue_date <- plane_data$issue_date %>% as.Date("%d/%m/%Y")

-----------------------------------------------------------------------------------------------
# # Check for missing values
# plane_data %>%
#   summarise(across(everything(), ~ sum(is.na(.)))) %>%
#   t() %>% as.data.frame() %>%
#   rename(misVal = 1) %>%
#   filter(misVal > 0)
# 
# # Since we are interested in the age of the planes versus delay, we shall drop records
# # with missing data.
# plane_data <- plane_data %>%
#   filter(!is.na(issue_date)) %>%
#   select(manufacturer, tailnum, issue_date)

-----------------------------------------------------------------------------------------------

# Similarly, we coerce the date information from the flight data to date class 
fd_comp$flight_date <- with(fd_comp, paste(Year, Month, DayofMonth, sep = "-")) %>%
  as.Date("%Y-%m-%d")
    
# We will be using the delay indicator formulated at the beginning of the study 
# to analyze how the age of a plane impact its performance. 
plane_date <- merge(fd_comp, plane_data, by.x = "TailNum", by.y = "tailnum",
                    all.x = TRUE, all.y = FALSE) %>% 
  merge(carriers, by.x = "UniqueCarrier", by.y = "Code",
                       all.x = TRUE, all.y = FALSE) %>% 
  select(Description, UniqueCarrier, TailNum, manufacturer, flight_date, 
         issue_date, season, Delayed)


# Before we proceed further, lets do a quick check for any missing values 
# after the merger of various different data frames.
plane_date %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  t() %>% as.data.frame() %>% 
  rename(misVal = 1) %>% 
  filter(misVal > 0)

# Since we are interested in the age of the planes versus delay, we shall drop records
# with missing data.
plane_date <- plane_date %>%
  filter(!is.na(issue_date))

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
  
# # Check for possible duplication of Tail Numbers  
# plane_date %>% 
#   group_by(TailNum) %>% 
#   summarise(count = n()) %>% 
#   arrange(desc(count))

  
# # From the plane data, we calculate the average manufacturing year as a reference, where;
# # before average = older planes,
# # after average = newer planes.
# plane_data %>%
#   filter(!is.na(year), year !=  0) %>% 
#   select(year) %>%
#   arrange(year) %>% 
#   summarise(avg_year = round(mean(year), 0))

-----------------------------------------------------------------------------------------------

# The age of a plane depends on multiple factors beside its chronological age. We will not
# be going into the details in this analysis and with reference, take 11 years - the average
# age of U.S. commercial aircraft - as our standard for old planes.
# Create an age indicator; 0 = old planes, 1 = new planes.  
plane_date <- plane_date %>% 
  mutate(step1 = ifelse(lubridate::time_length(difftime(plane_date$flight_date, 
                                                      plane_date$issue_date), "years") < 0, NA, 1),
         age = ifelse(lubridate::time_length(difftime(plane_date$flight_date, 
                                                        plane_date$issue_date), "years") <= 11, 1, 0)) %>% 
  filter(!is.na(step1))

# Flight performance (Delayed) indicator; 0 = On-time, 1 = Delayed
obsvtn <- plane_date %>% 
  select(UniqueCarrier, Delayed , age) %>% 
  mutate(age = factor(plane_date$age, c(0, 1), labels = c("old", "new")),
         Delayed = factor(plane_date$Delayed, c(0:1), labels = c("On-time", "Delayed")),
         UniqueCarrier = factor(plane_date$UniqueCarrier)
  ) 

# Our interest: if older planes suffer more delays than newer planes. Using the observed
# frequencies create a contingency table.
xtbl_count <- table(obsvtn$age, obsvtn$Delayed)

# Row percentages
xtbl_per <- round(prop.table(xtbl_count, 1)*100, 2)

# We display the information on a clustered bar chart, with percentages of flight on-time
# and delayed within each group.
xtbl_per %>%
  as.data.frame() %>%
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = (paste(round(Freq, 2), "%"))),
          vjust = -1, size = 4, position = position_dodge(width = 0.9)) +
  labs(title = "Percentage of Planes by Flight Performance grouped by Aircraft Age",
       x = "Flight Perfomance",
       y = "Percentage of Planes") +
  theme_get() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  guides(fill = guide_legend(title = "Plane"))

# There seems to be no association between the planes' age and their flight performance.


addmargins(xtbl_count)

# Overall 74.75% of flights arrived on-time, if no association exist between planes' age and 
# flight performance, approximately 75% of the planes would be on-time.


# H0: There is no association between the planes' age and flight performance
# H1: There is such an association
chisq.test(xtbl_count)

# As the p-value = 3.39%, which is more than the 1% significance level. The test result is
# moderately significant. There is not enough evidence to reject the null hypothesis and to 
# conclude that older planes do suffer from more delays

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

# # For each carrier, we calculate the average delays (in minutes)
# fd_comp %>% 
#   group_by(UniqueCarrier) %>% 
#   summarise(
#     avg_depdel = mean(DepDelay),
#     avg_arrdel = mean(ArrDelay)
#     ) %>% 
#   arrange(desc(avg_arrdel))
#   
# # Percentage of delays within each carrier
# fd_comp %>% 
#   group_by(UniqueCarrier) %>% 
#   select(UniqueCarrier, Delayed) %>% 
#   summarise(
#     total_delays = sum(Delayed),
#     per_del = mean(Delayed))
# 
# # Percentage of delays between the carrier
# donut <- fd_comp %>% 
#   group_by(UniqueCarrier) %>% 
#   summarise(
#     flight_count = n(),
#     n = sum(Delayed)) %>% 
#   ungroup() %>% 
#   summarise(
#     UniqueCarrier,
#     per_del = n/ sum(n)
#   ) %>% 
#   arrange(per_del)
# 
# 
# donut$ymax = cumsum(donut$per_del)
# donut$ymin = c(0, head(donut$ymax, n=-1))
# donut$labelPosition <- (donut$ymax + donut$ymin)/ 2
# donut$label <- paste0(donut$UniqueCarrier, "\n", round(donut$per_del, 4)*100, " %")
# 
# 
# donut %>% 
#   ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = UniqueCarrier)) +
#   geom_rect() + 
#   geom_text(x = 3.5, aes(y = labelPosition, label = label), size = 3.4,
#             color = "white") +
#   scale_fill_brewer(palette = "BrBG") +
#   scale_color_brewer(palette = "BrBG") +
#   coord_polar(theta="y") +
#   xlim(c(0.5, 5)) +
#   theme_void() +
#   labs(title = "Donut Plot - Percentage of Carrier by Delays")
 
-----------------------------------------------------------------------------------------------

# How does the number of people flying between different locations change over time?
# Lets check the structure of the data before we begin  
glimpse(fd_comp) 
   
fd_comp$route <- with(fd_comp, paste(Origin, Dest, sep = "-"))  
  
fd_routes <- fd_comp %>% 
  select(-contains("Time"), -contains("Del"), -contains("Num"))
  
head(fd_routes)
  
# Check for number of unique routes
fd_routes %>% 
  group_by(route) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

-----------------------------------------------------------------------------------------------
# # For each month, we calculate the change in flights by carrier
# fd_routes %>% 
#   group_by(Year, Month, route, UniqueCarrier) %>% 
#   summarise(count = n())
#   
# monthly_rep <- fd_routes %>% 
#   group_by(Month, route) %>% 
#   summarise(count = n()) %>% 
#   arrange(route) %>% 
#   group_by(route) %>% 
#   mutate(
#     MoM = round((count - lag(count, n = 1, default = NA))*100/ lag(count, n = 1, default = NA), 2)
#   ) 
# 
# route_count <- fd_routes %>%
#   group_by(Month, route) %>%
#   summarise(count = n()) %>%
#   spread(key = route, value = count)
# 
# perlag <- monthly_rep %>%
#   mutate(Month  = factor(Month, levels = c(1:12))) %>% 
#   select(Month, route, MoM) %>% 
#   spread(key = route, value = MoM)
# 
# 
# report <- mapply(rbind, route_count, perlag) 
# 
# report <- t(report[,-1])
# 
# col_lab1 <- as.data.frame(month.abb)
# col_lab2 <- rep("MoM (%)", 12) %>% 
#   as.data.frame()
# 
# report_label <- mapply(rbind, col_lab1, col_lab2)
# 
# colnames(report) <- as.character(report_label)

------------------------------------------------------------------------------------------------

# Multiple Bar chart - Number of Flights Inbound by Destinations
fd_routes %>% 
  group_by(Dest, Month) %>% 
  summarise(count = n()) %>%
  mutate(Month = month.abb[Month] %>% factor(levels = month.abb)) %>% 
  ggplot(aes(x = Month, y = count)) +
  geom_bar(stat = "identity", fill = "pink") +
  facet_wrap(~Dest, ncol = 13) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# The top 3 most popular destination seems to be (in no order) ATL, DFW, ORD. 

# Summary table for number of flights by destination per month
fd_mth <- fd_comp %>% 
  select(route, Origin, Dest, season, Month) %>% 
  group_by(Month, Dest) %>% 
  summarise(count = n()) %>% 
  arrange(Dest, Month) %>% 
  spread(key = Month, value = count) %>%
  column_to_rownames("Dest") %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_flight = rowSums(.))

# Proportion of Flights by Month by Destination
per_fd_mth <- fd_mth%>% 
  apply(2,
        function(x) {
          round(x*100/ sum(fd_mth$total_flight), 4)
          }) %>% 
  as.data.frame() %>% 
  filter(total_flight > 0.5) %>% 
  rownames_to_column("dest") %>% 
  gather(key = "month", value = "per_flight", -dest, -total_flight) %>% 
  mutate(month = factor(month, levels = c(1:12)))

per_fd_mth %>% 
  group_by(month, dest) %>%
  ggplot(aes(x = month, y = per_flight)) +
  geom_point(color = "orange") +
  geom_segment(aes(x = month, xend = month, y = 0, yend = per_flight)) +
  labs(title = "Proportion of Flights by Month by Destination",
     x = "Month",
     y = "Percentage of Flights") +
  theme_get() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  facet_wrap(~dest, ncol = 13)

# In general, people seem to fly less in February as the proportion of flights appears
# to decline on February across the destinations used in the graph. Eg. Flights inbound 
# to SEA is lowest in February and higher in August.

# Check popularity of destination by number of flight inbound by month
fd_routes %>% 
  group_by(Dest, Month) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Multiple Bar Plot - Number of Flights by Month by Flights outbound from ORD (Chicago)
fd_routes %>% 
  filter(Origin == "ORD") %>% 
  group_by(Month, route) %>% 
  summarise(count = n()) %>%
  mutate(Month = month.abb[Month] %>% factor(levels = month.abb)) %>% 
  ggplot(aes(x = Month, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  facet_wrap(~route, ncol = 13) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

-----------------------------------------------------------------------------------------------

# # Multiple Line Chart - Number of Flights by Time by Destination 
# # Removed records with less than 100 flights to destination
# fd_routes %>% 
#   mutate(date = with(fd_routes, paste(Year, Month, sep = "-")) %>% as.yearmon()) %>% 
#   group_by(date, Dest) %>% 
#   summarise(count = n()) %>%
#   filter(count > 100) %>% 
#   ggplot(aes(x = date, y = count)) +
#   geom_line() +
#   facet_wrap(~Dest, ncol = 20) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

-----------------------------------------------------------------------------------------------

# RE: How does the number of people flying between different locations change over time?
# We will be using the airport with the most number of outbound flights as our sample to
# this analysis. Lets check the airport with the most number of outbund flights.
fd_routes %>%
  group_by(Year, Origin) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

# ORD - Chicago O'Hare International has the most number of outgoing flights yearly.
# We will use fight data from the ORD airport to continue our analysis.
fd_routes %>% 
  mutate(date = with(fd_routes, paste(Year, Month, sep = "-")) %>% as.yearmon()) %>% 
  filter(Origin == "ORD") %>% 
  group_by(date, route) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_wrap(~route, ncol = 13) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# From the graphs, we are able to observe the trend of outgoing flights from ORD (Chicago) 
# over the years from 1995 to 1996 (left to right). Majority of the routes showed consistent
# outgoing flights with a few notable ones, namely "ORD-BOS", "ORD-FLL", "ORD-LAX", "ORD-SEA".
# Flights from Chicago to Los Angeles has picked up from Mar 1995 and remained consistently high.

------------------------------------------------------------------------------------------------

  # ord_rep <- fd_routes %>% 
#   filter(Origin == "ORD") 
# 
# ord_rep$date <- with(ord_rep, paste(Year, Month, sep = "-")) %>% 
#   as.yearmon()
# 
# 
# ord_rep <- ord_rep %>% 
#   group_by(date, route) %>%
#   summarise(count = n()) %>% 
#   arrange(route) %>% 
#   group_by(route) %>% 
#   mutate(
#     MoM = round((count - lag(count, n = 1, default = NA))*100/ lag(count, n = 1, default = NA), 2)
#   ) 
#   
# ord_rep %>% 
#   ggplot(aes(x = date, y = count)) +
#   geom_line() +
#   facet_wrap(~route, ncol = 13) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# count, location, time

  
------------------------------------------------------------------------------------------------
  
# For each day of the week, calculate the number of delays by each carrier
carrier_del <- flight_data_raw %>% 
  filter(DepDelay >= 15) %>% 
  group_by(DayOfWeek, UniqueCarrier) %>% 
  summarise(DelayCount = n()) 

# reshape data frame to a wide format to improve readability  
carrier_del_wide <- dcast(carrier_del, 
                          formula = UniqueCarrier ~ DayOfWeek, 
                          value.var = "DelayCount")

# For each day of the week, calculate the number of delays and the change from the previous 
# day of the week by each carrier
carrier_del_chg <- flight_data_raw %>%
  filter(DepDelay >= 15) %>% 
  group_by(UniqueCarrier, DayOfWeek) %>% 
  summarise(DelayCount = n()) %>% 
  mutate(Change = DelayCount -lag(DelayCount)) %>% 
  dcast(formula = UniqueCarrier ~ DayOfWeek,
        value.var = "Change") 

# Rename the columns of the table to indicate changes
colnames(carrier_del_chg) <- c("UniqueCarrier", "Mon-Chg", "Tue-Chg", "Wed-Chg", 
                               "Thu-Chg", "Fri-Chg", "Sat-Chg", "Sun-Chg")

# Merge carrier delay by day of week and changes 
joined_crrdel <- merge(carrier_del_wide, carrier_del_chg, by.x = "UniqueCarrier",
                       all.x = TRUE, all.y = TRUE) %>% 
  select(UniqueCarrier, Mon, `Mon-Chg`, Tue, `Tue-Chg`, Wed, `Wed-Chg`,
         Thu, `Thu-Chg`, Fri, `Fri-Chg`, Sat, `Sat-Chg`, Sun, `Sun-Chg`)

# Plot number of delays by each carrier for each day of the week
carrier_del %>% 
  ggplot(aes(x = DayOfWeek, y = DelayCount, group = UniqueCarrier)) + 
    geom_line(aes(color = UniqueCarrier), size = 2) + 
    labs(title = "Flight Delays by Airline (For Each Day of the Week)", 
         x = "Day of the Week", y = "Number of Delays") + 
    theme_light() + 
    theme(plot.title = element_text(hjust=0.5)) + 
    geom_point(shape=21, size=3, color="black", fill="white") + 
    scale_y_continuous(labels=,)

------------------------------------------------------------------------------------------------


# Calculates the average arrival delays across the carriers
fd_comp %>% 
  group_by(UniqueCarrier) %>% 
  summarise(Avg_ArrDelay = mean(ArrDelay)) %>% 
  arrange(Avg_ArrDelay)



