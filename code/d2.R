# load libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(DBI)
library(hms)

setwd("~/../Joshua PFDS/r/coursework/data")

asTime <- function(string, units = NULL) {
  strptime(string, "%H%M") %>% 
    round(units) %>% 
    format("%H:%M:%S") %>% 
    as_hms()
}

# create the database
if (file.exists("flights.db"))
  file.remove("flights.db")

# Create a connection to database
# dbDisconnect(conn)
conn <- dbConnect(RSQLite::SQLite(), "flights.db")

# load data
carriers <- read.csv("carriers.csv", header = TRUE)
airports <- read.csv("airports.csv", header = TRUE)
plane_data <- read.csv("plane-data.csv", header = TRUE)

dbWriteTable(conn, "airports", airports)
dbWriteTable(conn, "carriers", carriers)
dbWriteTable(conn, "plane_data", plane_data)

for (i in c(1995:2000)) {
  fd <- read.csv(paste0(i, ".csv"), header = TRUE)
  if (i == 1995) {
    dbWriteTable(conn, "flight_data", fd, overwrite = TRUE)
  } else {
    dbWriteTable(conn, "flight_data", fd, append = TRUE)
  }
}

flight_data <-tbl(conn, "flight_data")

# Quick check
glimpse(flight_data)

# Before we begin, lets check the number of missing values in each column
missing_values <- flight_data %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  collect() %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(count = 1) %>% 
  filter(count > 0) %>% 
  arrange(count) 

print(missing_values)

# We can create a visualization of the missing values with a bar chart, and
# set a filter to remove any variables with NA = total number of observations.
total_obs <- flight_data %>%  
  count() %>%
  collect()

missing_values %>% 
  rownames_to_column() %>% 
  filter(count < as.numeric(total_obs)) %>% 
  ggplot() +
  geom_bar(aes(x=rowname, y=count), stat = 'identity') +
  labs(x='Variable', y="Number of Missing Values", 
       title='Number of Missing Values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# It appears that there might be some connections between the missing values across
# the variables. Intuitively, cancelled or diverted flights might explain for the missing
# values. Lets check the number of cancelled or diverted flights.
flight_data %>% 
  count(Cancelled == 1)

flight_data %>% 
  count(Cancelled == 1 | Diverted == 1 )

print(missing_values)

# As observed, there are some connections between the number of cancelled or diverted flights
# and the missing values. More specifically, the number of missing values in departure 
# and arrival time.

# Before we proceed, we remove records with missing values
flight_data <- flight_data %>% 
  filter(Cancelled == 0, Diverted == 0) 

# We will drop columns with little interests to us.
fd_mod <- flight_data %>% 
  select(-FlightNum, -ActualElapsedTime, -CRSElapsedTime, -AirTime, -Distance,
         -TaxiIn, -TaxiOut, -Cancelled, -CancellationCode, -Diverted, -CarrierDelay,
         -WeatherDelay, -NASDelay, -SecurityDelay, -LateAircraftDelay) %>% 
  collect()

# Lets make some modifications to our existing data where necessary
fd_mod <- fd_mod %>% 
  mutate(flight_date = with(fd_mod, paste(DayofMonth, Month, Year, sep = "/")), # as.Date("%Y-%m-%d"), # might not be readable by SQL
         season = ifelse(Month %in% 3:5, "Spring", 
                         ifelse(Month %in% 6:8, "Summer", 
                                ifelse(Month %in% 9:11, "Autumn", "Winter"))) %>% 
           factor(levels = c("Winter", "Spring", "Summer", "Autumn")),
         DepDel15 = ifelse(DepDelay >= 15, 1, 0),
         ArrDel15 = ifelse(ArrDelay >= 15, 1, 0)) #, Delayed = ifelse(DepDel15 == 1 | ArrDel15 == 1, 1, 0)) # A delay indicator.

# Update the changes to our connection
dbWriteTable(conn, "flight_data", fd_mod, overwrite = TRUE)

flight_data <- tbl(conn, "flight_data")

# When is the best time of the day, day of the week, and time of the year to fly to minimize
# delays?  

# Check the structure of our data and the number of records  
glimpse(flight_data)
 
# We calculate for the number of arrival delays with a 15-minute "grace period" given. 
# A flight would be considered delayed only if the arrival delay exceed 15-minutes so as 
# to speak. 

# Intuitively, flights can arrive on-time despite a late departure, hence we will be using
# ArrDel15 (arrival delay > 15) as our indicator for delay to analyze the best time to travel.

# Overview of arrival delays
arrdel_htmap <- dbGetQuery(conn, 
                           "SELECT Year, Month, DayOfWeek, flight_date, ArrDel15
                           FROM flight_data
                           WHERE ArrDel15 == 1")

arrdel_htmap <- arrdel_htmap %>% 
  mutate(Month = Month %>% factor(labels = month.abb),
         DayOfWeek = DayOfWeek %>% factor(levels = rev(c(7, 1, 2, 3, 4, 5, 6)),
                                          labels = rev(c("Sun", "Mon", "Tue", "Wed", 
                                                         "Thu", "Fri", "Sat"))),
         flight_date = flight_date %>% as.Date("%d/%m/%Y"),
         WeekofMonth = stringi::stri_datetime_fields(flight_date)$WeekOfMonth) %>% 
  group_by(Year, Month, WeekofMonth, DayOfWeek) %>% 
  summarise(total_delay = n())

arrdel_htmap %>% 
  ggplot(aes(WeekofMonth, DayOfWeek, fill = total_delay)) +
  geom_tile(colour = "black") +
  labs(title = "Frequency of Arrival Delays Overview",
      x = "Month / Week of Month",
      y = "Year / Day of Week") +
  theme_grey() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_discrete(limits = c(1:5)) +
  facet_grid(Year ~ Month) +
  guides(fill = guide_legend(title = "Arrival Delays")) 


arrdel_dom <- dbGetQuery(conn,
                         "SELECT DayofMonth, season, AVG(ArrDel15) AS per_del
                         FROM flight_data
                         GROUP BY DayofMonth, season")

# Lets visualize the best season to travel.
arrdel_dom %>% 
  ggplot(aes(DayofMonth, per_del, group = season)) +
  geom_line(aes(color = season), size = 1) +
  labs(title = "Percentage of Arrival Delay by Season",
       x = "Day of Month",
       y = "Percentage of Arrival Delay") +
  theme_grey() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_discrete(limits = c(1:31)) +
  scale_y_continuous(labels = scales::percent)

# From the graph, Autumn - most bottom line - has the least percentage of flights delayed.
# During the Autumn season, the point with the lowest percentage of arrival delays seems to 
# be near the end of the months.

arrdel_mth <- dbGetQuery(conn,
                         "SELECT Month, AVG(ArrDel15) AS per_del
                         FROM flight_data
                         GROUP BY Month")

arrdel_mth %>% 
  ggplot(aes(Month, per_del)) +
  geom_bar(stat = "identity", aes(fill = per_del)) +
  geom_text(aes(label = paste(round(per_del, 4)*100, "%")),
            vjust = -1) +
  scale_fill_gradientn(colors = rev(colorspace::heat_hcl(3))) +
  labs(title = "Percentage of Arrival Delay by Month",
       x = "Month",
       y = "Percentage of Arrival Delay") +
  theme_grey() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(title = "Percentage"))  

# The best Months to travel are in May (Spring), September, and October (Autumn) with chance 
# delay flights on arrival - 19.77%, 16.54% and 18.71% respectively.

arrdel_wk <- dbGetQuery(conn,
                        "SELECT Month, flight_date, ArrDel15
                        FROM flight_data")


arrdel_wk %>% 
  mutate(flight_date = flight_date %>% as.Date("%d/%m/%Y"),
         WeekofMonth = stringi::stri_datetime_fields(flight_date)$WeekOfMonth) %>% 
  group_by(Month, WeekofMonth) %>% 
  summarise(per_del = mean(ArrDel15)) %>% 
  ggplot(aes(WeekofMonth, per_del)) +
  geom_bar(stat = "identity", aes(fill = per_del)) +
  geom_text(aes(label = paste(round(per_del, 4)*100, "%")),
            hjust = 1.2, angle = 90) +
  scale_fill_gradientn(colors = rev(colorspace::heat_hcl(2))) +
  labs(title = "Percentage of Arrival Delay by Week by Month",
       x = "Day of Month",
       y = "Percentage of Arrival Delay") +
  theme_grey() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_discrete(limits = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(title = "Percentage")) +
  facet_wrap(~ Month, labeller = as_labeller(c(`1` = "Jan", `2` = "Feb", `3` = "Mar", 
                                               `4` = "Apr", `5` = "May", `6` = "Jun", 
                                               `7` = "Jul", `8` = "Aug", `9` = "Sep",
                                               `10` = "Oct", `11` = "Nov", `12` = "Dec")))


# Statistically, the best week to travel during the Autumn season is in fact the first week.

# Now let us look at which day of the week is the best to travel.
arrdel_dy <- dbGetQuery(conn,
                        "SELECT DayOfWeek, season, AVG(ArrDel15) AS per_del
                        FROM flight_data
                        GROUP BY DayOfWeek, season")

arrdel_dy %>% 
  ggplot(aes(DayOfWeek, per_del)) +
  geom_bar(stat = "identity", aes(fill = per_del)) +
  geom_text(aes(label = paste(round(per_del, 4)*100, "%")),
            vjust = -1, size = 3) +
  scale_fill_gradientn(colors = rev(colorspace::heat_hcl(4))) +
  labs(title = "Percentage of Arrival Delay by Day of Week",
       x = "Day of Week",
       y = "Percentage of Arrival Delay") +
  theme_grey() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(title = "Percentage")) +
  facet_wrap(~ season)

# Its best to fly on the weekends. Most notably, the best day of week to fly during Autumn
# season is on the Saturday(s), with a low of 14.10% of flights delayed. 
# Summary: Winter: Saturday - 21.79%
#          Spring: Saturday - 16.85%
#          Summer: Saturday - 20.69%
#          Autumn: Saturday - 14.10%


# Using CRSDepTime (Scheduled Departure Time) with every hour binned as a group (24 groups in
# total).

arrdel_tm <- dbGetQuery(conn, 
                        "SELECT CRSDepTime, ArrDel15
                        FROM flight_data
                        WHERE ArrDel15 == 1")

arrdel_tm <- arrdel_tm %>%
  mutate(CRSDepTime = sprintf("%04d", arrdel_tm$CRSDepTime) %>%
           asTime(units = "hours")) %>% 
  group_by(CRSDepTime) %>%
  summarise(delay_count = n()) 

# Lets create a visualization - Number of Arrival Delays versus CRSDepTime
arrdel_tm %>%
  ggplot(aes(x = CRSDepTime, y = delay_count)) +
  geom_line(color="#69b3a2", size = 1.5) +
  labs(title = "Arrival Delays versus Scheduled Departure Time",
       x = "Schedule Departure Time",
       y = "Number of Arrival Delays") +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_time(labels = scales::label_time(format = "%H:%M"),
               breaks = hms(hours = seq(0, 23, 1)))

# According to the chart, flight delays were low in the early morning and picked up after 10:00
# with the most number of delays occurring between 16:00-19:00. The delays decline towards
# late night.
# Minimize the chance of flight delays by traveling in the morning and avoid flights that
# depart between 16:00-19:00.

# Do older planes suffer more delays than newer planes?
# We shall make use of the plane data file to aid in this analysis.
# Lets check the structure of the data before we begin   
glimpse(plane_data)

# We will be using the delay indicator formulated at the beginning of the study 
# to analyze how the age of a plane impact its performance.
plane_date <- dbGetQuery(conn,
  "SELECT carriers.Description, UniqueCarrier, plane_data.tailnum, flight_date, issue_date, ArrDel15
  FROM (plane_data LEFT JOIN flight_data ON plane_data.tailnum = flight_data.TailNum) S 
  JOIN carriers ON S.UniqueCarrier = carriers.Code
  WHERE ArrDel15 >= 0") # We added an additional filter here to improve the run time

glimpse(plane_date)

# The age of a plane depends on multiple factors beside its chronological age. We will not
# be going into the details in this analysis and with reference, take 11 years - the average
# age of U.S. commercial aircraft - as our standard for old planes.
# Create an age indicator; 0 = old planes, 1 = new planes.  

# Coerce issue_date to date class
plane_date <- plane_date %>% 
  mutate(flight_date = flight_date %>% as.Date("%d/%m/%Y"),
         issue_date = issue_date %>% as.Date("%m/%d/%Y")) # The data was recorded in an awkward format

plane_date <- plane_date %>% 
  mutate(neg_age = ifelse(lubridate::time_length(difftime(plane_date$flight_date, 
                                                        plane_date$issue_date), "years") < 0, NA, 1),
         age = ifelse(lubridate::time_length(difftime(plane_date$flight_date, 
                                                      plane_date$issue_date), "years") <= 11, 1, 0)) %>% 
  filter(!is.na(neg_age))

# Flight performance (Delayed) indicator; 0 = On-time, 1 = Delayed
obsvtn <- plane_date %>% 
  select(UniqueCarrier, ArrDel15 , age) %>% 
  mutate(age = factor(plane_date$age, c(0, 1), labels = c("old", "new")),
         ArrDel15 = factor(plane_date$ArrDel15, c(0:1), labels = c("On-time", "Delayed")),
         UniqueCarrier = factor(plane_date$UniqueCarrier)
  ) 

# Our interest: if older planes suffer more delays than newer planes. Using the observed
# frequencies create a contingency table.
xtbl_count <- table(obsvtn$age, obsvtn$ArrDel15)

# Row percentages
xtbl_per <- round(prop.table(xtbl_count, 1)*100, 2)

# We display the information on a clustered bar chart, with percentages of flight on-time
# and delayed within each group.
xtbl_per %>%
  as.data.frame() %>%
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Freq),
            vjust = -1, size = 4, position = position_dodge(width = 0.9)) +
  labs(title = "Percentage of Planes by Flight Performance grouped by Aircraft Age",
       x = "Flight Perfomance",
       y = "Percentage of Planes") +
  theme_grey() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  guides(fill = guide_legend(title = "Plane"))

# There seems to be no association between the planes' age and their flight performance.
# We will follow up with a chi-square test of association to check for any statistical 
# significance of difference between the planes' age and flight performance. 

# H0: There is no association between the planes' age and flight performance
# H1: There is such an association
chisq.test(xtbl_count)

# As the p-value = 0.28%, which is less than the 1% significance level. The test result is
# highly significant. The results indicate enough evidence to reject the null hypothesis and 
# conclude an association between the planes' age and flight performance.

# Although we were able to conclude that the difference in on-time performance and planes' age
# is statistically significant, the graphical representation of the performance for both the
# age groups look similar with a 0.14% difference.

# We conclude a small, consistent association between the planes' age and their performance
# due to the statistical significance, but biologically insignificant. Therefore, it is 
# unlikely that older planes do suffer from more delays.


# How does the number of people flying between different locations change over time?
# Since we do not have the information on number of passengers abroad on each plane we shall
# use of the number of flights as our indication of popularity.

# Lets check the structure of the data before we begin  
glimpse(flight_data) 

fd_routes <- dbGetQuery(conn,
                        "SELECT Year, Month, Origin, Dest, season
                        FROM flight_data")

fd_routes <- fd_routes %>% 
  mutate(route = with(fd_routes, paste(Origin, Dest, sep = "-")))

# # Check for number of unique routes
# fd_routes %>% 
#   group_by(route) %>% 
#   summarise(count = n()) %>% 
#   arrange(desc(count))
  
# We will be using the airport with the most number of outbound flights as our sample to
# this analysis. Lets check the airport with the most number of outbound flights.
fd_routes %>%
  group_by(Year, Origin) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

# ORD - Chicago O'Hare International has the most number of outgoing flights yearly.
# We will use fight data from the ORD airport to continue our analysis.

# Multiple Time Series Chart - Number of Outbound Flights from ORD airport over Time by 
# Destination
fd_routes %>% 
  mutate(date = with(fd_routes, paste(Year, Month, sep = "-")) %>% zoo::as.yearmon()) %>% 
  filter(Origin == "ORD") %>% 
  group_by(date, route) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  labs(title = "Number of Outbound Flights from ORD airport by Destination",
       x = "Date",
       y = "Number of Flights") +
  theme_grey() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~route, ncol = 13)

# From the graphs, we are able to observe the trend of outgoing flights from ORD (Chicago) 
# over the years from 1995 to 2000 (left to right). Majority of the routes showed consistent
# trends. 

# Flights from Chicago to Minneapolis (ORD-MSP) has been consistently decreasing over the years
# and flights from Chicago to Philadelphia (ORD_PHL) has been increasing. Meanwhile, flights
# to Seattle (ORD-SEA) are low at the beginning and end of each year and highest mid-year.

# It is important to note that this trend is exclusively for flights outbound from ORD and does
# not make a good representation of all the airports. However, the same analysis can be 
# performed to uncover the patterns using the origin airport of interest.

# Can you detect cascading failures as delays in one airport create delays in others?

# In this study, we shall analyze for cascading delays between the top 10 airports with 
# the highest traffic in the USA by number of flights. 
# Lets check which 10 states carries the most flights.

bz_airports <- dbGetQuery(conn, 
                          "SELECT Origin, city as city_o, COUNT(*) AS count
                          FROM flight_data, airports
                          WHERE airports.iata = flight_data.Origin
                          GROUP BY Origin, city_o
                          ORDER BY count DESC
                          Limit 10")

airport_del <- dbGetQuery(conn,
                          "SELECT CRSDepTime, SUM(DepDel15) AS delay_count, Origin, Dest, 
                          O.city AS city_o, O.lat AS lat_o, O.long AS long_o,
                          D.city AS city_d, D.lat AS lat_d, D.long AS long_d
                          FROM airports AS O, airports AS D, flight_data
                          WHERE O.iata = flight_data.Origin
                          AND D.iata = flight_data.Dest
                          AND DepDel15 != 0
                          GROUP BY CRSDepTime, Origin, Dest")

# airport_del <- airport_del %>% 
#   mutate(route = with(fd_routes, paste(Origin, Dest, sep = "-")))

# Stratified by time frame
# recall: departure time from 1600-1900 has the most number of delayed flights
delay1 <- airport_del %>% 
  filter(Origin == "ORD",
         Dest %in% bz_airports$Origin,
         CRSDepTime > 1400 & CRSDepTime <= 1700)

state <- map_data("state") 

ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group = group),
               fill = "darkgrey",
               color = "white") + 
  geom_curve(data = delay1, aes(x = long_o, y = lat_o, xend = long_d, yend = lat_d, 
                                 color = delay_count),
             arrow = arrow(length = unit(0.4, "cm")), 
             size = 0.8) +
  geom_label(data = delay1, nudge_x = 0, nudge_y = -1,
             aes(x = long_d, y = lat_d, label = Dest)) +
  geom_label(data = delay1, nudge_x = 1, nudge_y = -0, color = 'red',
             aes(x = long_o, y = lat_o, label = Origin)) +
  labs(title = paste("Visualizing Cascading Delays in Flights between the",
                     "Top 10 Busiest Airports in the US"),
       subtitle = "Timeframe: 1600 - 1900") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  theme(panel.background=element_blank(),
        axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) + 
  coord_fixed(1.3) 

# Flights leaving from ORD to MSP and LAX has more delays indicated by the darker
# shade of blue

delay2 <- airport_del %>% 
  filter(Origin %in% bz_airports$Origin,
         Dest %in% bz_airports$Origin,
         CRSDepTime > 1700 & CRSDepTime <= 2000,
         delay_count > 100)
  
  
ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, group = group),
               fill = "darkgrey",
               color = "white") + 
  geom_curve(data = delay2, aes(x = long_o, y = lat_o, xend = long_d, yend = lat_d, 
                                 color = delay_count),
             arrow = arrow(length = unit(0.4, "cm")), 
             size = 0.8) +
  geom_label(data = delay2, nudge_x = 0, nudge_y = -1,
             aes(x = long_d, y = lat_d, label = Dest)) +
  labs(title = paste("Visualizing Cascading Delays in Flights between the",
                     "Top 10 Busiest Airports in the US"),
       subtitle = "Timeframe: 1900 - 2200") +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  theme(panel.background=element_blank(),
        axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) + 
  coord_fixed(1.3) 

  # The shade of each arrow shows the number of delays at the particular time frame; 1700-2100.
# We were able to detect cascading failures for flights leaving MSP and LAX to the other 
# states. There is also noticeable increase in delays for flights leaving ORD 
----------------------------------------------------------------------------------------------
  
  
  lagged_delays <- fd_comp %>% 
  filter(CRSDepTime > 1900 & CRSDepTime <= 2200) %>% # Timeframe: 1900 - 2200
  select(DepTime, DepDelay, Origin, Dest) %>% 
  arrange(Origin, DepTime) %>%
  group_by(Origin) %>%
  mutate(dep_delay_lag = lag(DepDelay)) %>%
  filter(!is.na(DepDelay), !is.na(dep_delay_lag))

# We plot the relationship between the average delay time of a flight against the time 
# delayed for all the previous flight. 

lagged_delays %>%
  filter(Origin %in% bz_airports) %>% 
  group_by(dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(DepDelay)) %>% 
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  labs(y = "Departure Delay", x = "Previous Departure Delay") +
  scale_x_continuous(breaks = seq(0, 1500, by = 120))

# The result shows a positive relationship between the previous delay and the subsequent 
# flights' departure delay. The increase in variability after the 480 (mins) mark, 
# indicate the strength of the relationship cooling off after about 8-hours. This could
# suggest flights with shorter delays having a stronger effect on the subsequent flights 
# disability to depart on-time while flights with longer delays have poorer effect. 
# This makes sense as long-delayed flights can be interspersed with flights leaving on-time.

lagged_delays %>%
  filter(Origin %in% bz_airports) %>% 
  group_by(Origin, dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(DepDelay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  labs(y = "Departure Delay", x = "Previous Departure Delay") +
  scale_x_continuous(breaks = seq(0, 1500, by = 120)) +
  facet_wrap(~ Origin, ncol=2)