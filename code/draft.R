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

# Create a new column combining the dates
# flight_data_raw$Date <- with(flight_data_raw, paste(Year, Month, DayofMonth, sep = "-")) %>% 
#  as.Date("%Y-%m-%d")

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
         -WeatherDelay, -NASDelay, -SecurityDelay, -LateAircraftDelay)

-----------------------------------------------------------------------------------------------

# Check the structure of our data and the number of records  
glimpse(fd_comp)

# Check for the number of unique Tail Numbers
fd_comp %>% 
  group_by(TailNum) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))

# Let's look at the planes (identified by their tail number) that have the highest average delays
tail_delay <- fd_comp %>% 
  group_by(TailNum) %>% 
  summarise(AvgArrDelay = mean(ArrDelay)) %>% 
  arrange(desc(AvgArrDelay))

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

# Using CRSDepTime (Scheduled Departure Time) with every hour binned as a group, we calculate
# for the number of arrival delays with a 15-minute "grace period" given. A flight would be
# considered delayed only if the arrival delay exceed 15-minutes so as to speak. Lets create
# a delay indicator.
fd_comp <- fd_comp %>% 
  mutate(DepDel15 = ifelse(DepDelay >= 15, 1, 0),
         ArrDel15 = ifelse(ArrDelay >= 15, 1, 0),
         Delayed = ifelse(DepDel15 == 1 | ArrDel15 == 1, 1, 0)) %>% 
  select(-Cancelled, -Diverted)  



fd <- fd_comp %>% mutate(Season = ifelse(Month %in% 3:5, "Spring",
                                         ifelse(Month %in% 6:8, "Summer",
                                                ifelse(Month %in% 9:11, "Autumn",
                                                       "Winter"))))


# Lets visualize the best Month to travel
depdel_mth <- fd_comp %>% 
  group_by(Month) %>% 
  select(Month, ArrDel15) %>% 
  summarise(delay_count = sum(ArrDel15),
            per_del = mean(ArrDel15))

depdel_mth %>% 
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

# The best Month to travel are in April, May and September with chance of 18.05%, 18.82% 
# and 16.54% of flights delayed on arrival respectively.

# Now let us look at which day in the week is the best to travel.
depdel_wk <- fd_comp %>% 
  group_by(DayOfWeek) %>% 
  select(DayOfWeek, ArrDel15) %>% 
  summarise(delay_count = sum(ArrDel15),
            per_del = mean(ArrDel15))

depdel_wk %>% 
  ggplot(aes(DayOfWeek, per_del)) +
  geom_bar(stat = "identity", aes(fill = per_del)) +
  geom_text(aes(label = paste(round(per_del, 4)*100, "%")),
            vjust = -1) +
  scale_fill_gradientn(colors = rev(colorspace::heat_hcl(4))) +
  theme_light() +
  labs(title = "Percentage of Arrival Delay by Day of Week",
       x = "Day of Week",
       y = "Percentage of Arrival Delay") +
  theme_get() +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(title = "Percentage"))

# It best to fly on the weekends. Most notably, only 18.95% of the flights on Saturdays are
# prone to arrive late.

------------------------------------------------------------------------------------------------
  
depdel_tm <- fd_comp %>% 
  mutate(CRSDepTime = sprintf("%04d", fd_comp$CRSDepTime) %>% 
           asTime(format = "%H:00:00")) %>% 
  filter(ArrDelay >= 15) %>% 
  group_by(CRSDepTime) %>% 
  summarise(delay_count = n()) 

# Lets create a visualization - Number of Arrival Delays versus CRSDepTime
depdel_tm %>% 
  ggplot(aes(x = CRSDepTime, y = delay_count)) +
  geom_line(color="#69b3a2", size = 1.5) +
  labs(title = "Delay versus Scheduled Departure Time",
    x = "Schedule Departure Time",
    y = "Number of Arrival Delays") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_time(labels = scales::label_time(format = "%H:%M"), 
               breaks = hms(hours = seq(0, 23, 1)))
  
# According to the chart, flight delays were low in the early morning and picked up after 06:00
# with the most number of delays occurring between 16:00-19:00. The delays decline towards
# late night.
# Minimize chances of flight delay by avoiding flights that depart between 16:00-19:00.


# Since we have looked at the number of arrival delays by departure time, let us now study 
# the average arrival delay time.   
depdel_tm2 <- fd_comp %>% 
  mutate(CRSDepTime = sprintf("%04d", fd_comp$CRSDepTime) %>% 
           asTime(format = "%H:%M:00")) %>% 
  filter(ArrDelay >= 15) %>%
  group_by(CRSDepTime) %>%
  summarise(avg_del = mean(ArrDelay))
  
# Alternate visualization - Average Arrival Delayed Time (min) versus CRSDepTime
depdel_tm2 %>% 
  ggplot(aes(CRSDepTime, avg_del)) +
  geom_point(color = "#69b3a2", size = 1.2) +
  geom_smooth(method = "loess", col = "#CC6666", size = 1.5, se = FALSE) +
  geom_hline(yintercept = 15) +
  labs(title = "Average delay (minutes) versus Scheduled Departure Time",
       x = "Schedule Departure Time",
       y = "Average Arrival Delayed Time (minutes)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_x_time(labels = scales::label_time(format = "%H:%M"), 
               breaks = hms(hours = seq(0, 23, 1))) +
  scale_y_continuous(breaks = sort(c(seq(min(depdel_tm2$avg_del), 
                                         max(depdel_tm2$avg_del), 
                                         length.out=5), 15)))
  
# Once again, we were able to observe from the graph that the average arrival delayed time 
# is the highest for flights that are scheduled between 16:00-19:00.
# In general, expected arrival delay is between 30-45 minutes. 
  
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------  





















































# Do older planes suffer more delays than newer planes?
# Lets check the structure of the data before we begin   
glimpse(plane_data)

# As the original record of the years are of type chr, we have to cast it to numerical where
# arithmetic calculations can be applied
plane_data$year <- plane_data$year %>% as.numeric()

# Create a table with plane manufactured year identified by tail number
plane_reldate <- plane_data %>% 
  select(TailNum = tailnum, year)



fd_reldate <- merge(fd_new, plane_reldate, by = "TailNum", all.x = TRUE, all.y = FALSE) %>%
  group_by(TailNum, UniqueCarrier, year) %>% 
  summarise(total_delays = sum(Delayed))

# Check for possible duplication of Tail numbers  
fd_reldate %>% 
  group_by(TailNum) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))

# After reviewing the results, there are records with duplicate Tail numbers.
# This could suggest possible entry errors in the original data.

# Check for missing data
summary(fd_reldate)

# We have records on an estimated 3634 planes (by tail number).
# Unfortunately, we do not have the manufactured data for 2431 of those planes.
# Since we are interested in the age of the planes on delay, the incommplete records
# will be removed.
fd_reldate <- fd_reldate %>% 
  filter(!is.na(year))


fd_reldate <- merge(fd_reldate, carriers, by.x = "UniqueCarrier", by.y = "Code",
                       all.x = TRUE, all.y = FALSE) %>% 
  select(TailNum, UniqueCarrier, carrier = Description, everything()) %>% 
  rename_all(tolower)

# From the plane data, we calculate the average manufacturing year as a reference, where;
# before average = older planes,
# after average = newer planes.
plane_data %>%
  filter(!is.na(year), year !=  0) %>% 
  select(year) %>%
  arrange(year) %>% 
  summarise(avg_year = round(mean(year), 0))

# For each year, we calculate the number of plane delays and create an 'age group' indicator
test <- fd_reldate %>% 
  group_by(year) %>% 
  summarise(total_delays = sum(total_delays)) %>% 
  mutate(age = ifelse(year >= 1997, "new", "old")) 

# We conduct a hypothesis test to compare equality of the two variance
res.ftest <- var.test(total_delays ~ age, data = test)

res.ftest
# As the p-value = 6.468e-8%, which is less than the 1% significance level.
# There is strong evidence to reject the null hypothesis and conclude that the true
# variances are not equal

# Conduct a unpaired left-tailed t-test
# Null hypothesis: average new planes delay - average old planes delay >= 0
# Alt hypothesis: average new planes delay - average old planes delay < 0
res <- t.test(total_delays ~ age, data = test, var.equal = FALSE, alternative = "less")

res
# As the p-value = 0.01266%, which is less than the 1% significance level.
# There is strong evidence to reject the null hypothesis and conclude that older planes
# do suffer from more delays
  
bx_data <- fd_reldate %>% 
  group_by(year, uniquecarrier) %>% 
  summarise(total_delays = sum(total_delays)) %>%
  mutate(age = ifelse(year >= 1997, "new", "old"))
 
bx_data %>% 
  ggplot(aes(year, total_delays, fill = uniquecarrier)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 1997, color = "red", linetype = "dashed", size = 0.3) +
  geom_text(aes(x=1997, label="\nnew planes", y=35000), colour="#69b3a2", angle=90, size = 3.2) +
  geom_text(aes(x=1997, label="old planes\n", y=35000), colour="#CC6666", angle=90, size = 3.2) +
  guides(fill=guide_legend(title="Unique Carrier")) +
  facet_wrap(~uniquecarrier, ncol = 3) +
  labs(
    title = "Multiple Bar Charts - Total Delays by Manufactured Year by Carrier",
    x = "Manufactured Year",
    y = "Delay Count"
  )

# The majority of planes delay by manufactured year for each carrier is below the "1997" reference
# mark. However, this graphical representation is not sufficient enough to conclude older planes
# do indeed result in more delays, and should be interpreted together with a hypothesis test result.

# There seems to be a pattern in plane delays by manufactured year. More specifically, planes 
# that were manufactured in the year 1989-1991 appears to have contributed to the majority of 
# delays.

------------------------------------------------------------------------------------------------

# For each carrier, we calculate the average delays (in minutes)
fd_new %>% 
  group_by(UniqueCarrier) %>% 
  summarise(
    AvgDepDelay = mean(DepDelay),
    AvgArrDelay = mean(ArrDelay)
    ) %>% 
  arrange(desc(AvgDepDelay))
  
# Percentage of delays within each carrier
fd_comp %>% 
  group_by(UniqueCarrier) %>% 
  select(UniqueCarrier, Delayed) %>% 
  summarise(
    Total_Delays = sum(Delayed),
    PerDelays = mean(Delayed))

# Percentage of delays between the carrier
donut <- fd_new %>% 
  group_by(UniqueCarrier) %>% 
  summarise(
    TotalFlight = n(),
    n = sum(Delayed)) %>% 
  ungroup() %>% 
  summarise(
    UniqueCarrier,
    PerDelays = n/ sum(n)
  ) %>% 
  arrange(PerDelays)


donut$ymax = cumsum(donut$PerDelays)
donut$ymin = c(0, head(donut$ymax, n=-1))
donut$labelPosition <- (donut$ymax + donut$ymin)/ 2
donut$label <- paste0(donut$UniqueCarrier, "\n", round(donut$PerDelays, 4)*100, " %")


donut %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=UniqueCarrier)) +
  geom_rect() +
  geom_text(x = 2.5, aes(y = labelPosition, label = label, color = UniqueCarrier), size = 3) +
  scale_fill_brewer(palette = "BrBG") +
  scale_color_brewer(palette = "BrBG") +
  coord_polar(theta="y") +
  xlim(c(0, 5)) +
  theme_void() +
  labs(
    title = "Donut Plot - Percentage of Delays by Carrier"
  )
 
-----------------------------------------------------------------------------------------------

# How does the number of people flying between different locations change over time?
# Lets check the structure of the data before we begin  
glimpse(fd_new) 
   
fd_new$route <- with(fd_new, paste(Origin, Dest, sep = "-"))  
  
fd_routes <- fd_new %>% 
  select(-contains("Time"), -contains("Del"), -contains("Num"), -Distance)
  
head(fd_routes)
  
# Check for number of unique routes
fd_routes %>% 
  group_by(route) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


# For each month, we calculate the change in flights by carrier
fd_routes %>% 
  group_by(Year, Month, route, UniqueCarrier) %>% 
  summarise(count = n())
  
monthly_rep <- fd_routes %>% 
  group_by(Month, route) %>% 
  summarise(count = n()) %>% 
  arrange(route) %>% 
  group_by(route) %>% 
  mutate(
    MoM = round((count - lag(count, n = 1, default = NA))*100/ lag(count, n = 1, default = NA), 2)
  ) 

route_count <- fd_routes %>% 
  group_by(Month, route) %>% 
  summarise(count = n()) %>% 
  spread(key = route, value = count)

perlag <- monthly_rep %>%
  mutate(Month  = factor(Month, levels = c(1:12))) %>% 
  select(Month, route, MoM) %>% 
  spread(key = route, value = MoM)


report <- mapply(rbind, route_count, perlag) 

report <- t(report[,-1])

col_lab1 <- as.data.frame(month.abb)
col_lab2 <- rep("MoM (%)", 12) %>% 
  as.data.frame()

report_label <- mapply(rbind, col_lab1, col_lab2)

colnames(report) <- as.character(report_label)


----------------------------------------------------------------------------

# Check popularity of destination by number of flight arrivals by month
fd_routes %>% 
  group_by(Dest, Month) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Multiple Barplot for number of flights by season for each unique destinations
fd_routes %>% 
  group_by(Month, Dest) %>% 
  summarise(count = n()) %>%
  mutate(Month = month.abb[Month] %>% factor(levels = month.abb)) %>% 
  ggplot(aes(x = Month, y = count)) +
  geom_bar(stat = "identity", fill = "pink") +
  facet_wrap(~Dest, ncol = 13) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Multiple Barplot for number of flights by season for each outgoing flight from ORD (Chicago)
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


# Multiple line chart for number of flights by time for each unique destination 
# Removed records with less than 100 flights to destination
fd_routes %>% 
  mutate(date = with(fd_routes, paste(Year, Month, sep = "-")) %>% as.yearmon()) %>% 
  group_by(date, Dest) %>% 
  summarise(count = n()) %>%
  filter(count > 100) %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  facet_wrap(~Dest, ncol = 20) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# We sample 1 airport, we check the number of flights by routes by month
# Lets check which airport has the most number of outgoing flights in a year
fd_routes %>%
  group_by(Year, Origin) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

# ORD - Chicago O'Hare International has the most number of outgoing flights yearly.
# We shall use fight data from the ORD airport as our sample.

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



























# Create a new data frame storing only information on flight delays
flight_data_delays <- flight_data_raw[, c("CRSDepTime" ,"CRSArrTime", "DepTime", "ArrTime", "DepDelay", "ArrDelay")]

# A smaller version of the raw data frame, removing variables that might not be useful
flight_data <- flight_data_raw %>% transmute(UniqueCarrier, Origin, Dest, CRSDepTime, 
                                             DepTime, DepDelay, CRSArrTime, ArrTime, 
                                             ArrDelay, CRSElapsedTime, ActualElapsedTime)



flight_data <- flight_data_raw %>% mutate(Season = ifelse(Month %in% 3:5, "Spring",
                                                             ifelse(Month %in% 6:8, "Summer",
                                                                    ifelse(Month %in% 9:11, "Autumn",
                                                                           "Winter"))))

# Check for the number of missing data
skim(flight_data, ArrDelay, DepDelay)
# flight_data %>% count(is.na(DepDelay))

# For a better accuracy of the results, incomplete observations will be removed
# All cancelled and diverted flights are removed
flight_data <- flight_data[complete.cases(flight_data),]

# Check for negative flight times
# flight_data %>% count(ArrTime > DepTime)

# Quick check for records with Duplicate Tail Numbers
# flight_data %>% count(length(unique(TailNum)))



>> filter ArrDelays more than 15 mins
>> create a heatmap 

>> find the average arrival delay time
>> % of the arrival delay flight over total flights


flight_data <- flight_data %>% mutate(DepDel = ifelse(DepDelay > 0 & DepDelay < 15, 0, DepDelay),
                                      DepDel15 = ifelse(DepDelay >= 15, 1, 0))

flight_data %>% count(DepDel == 0)
flight_data %>% count(DepDel15 == 1)


DepDel <- flight_data %>% transmute(DepDel = ifelse(DepDelay > 0 & DepDelay < 15, 0, NA))
DepDel15 <- flight_data %>% transmute(DepDel15 = ifelse(DepDelay >= 15, 1, NA))


DepDel_long <- melt(data = DepDel,
                        variable.name = "DepDelay",
                        value.name = "Value")

DepDel15_long <- melt(data = DepDel15,
                        variable.name = "DepDelay",
                        value.name = "Value")

DepDelay <- rbind(DepDel_long, DepDel15_long)
DepDelay <- DepDelay[complete.cases(DepDelay),]

# library(ggplot2)
# pie <- ggplot(DepDelay, aes(x = "", y = Value, fill = DepDelay)) +
#  geom_bar(width = 1, stat = "identity") +
#  coord_polar("y", start = 0)


flight_data %>% 
  dplyr::group_by(UniqueCarrier) %>% 
  skim(ArrDelay, DepDelay)

# Calculates the average arrival delays across the carriers
carrier_performance <- flight_data %>% 
                        group_by(UniqueCarrier) %>% 
                        summarise(Avg_ArrDelay = mean(ArrDelay)) %>% 
                        arrange(Avg_ArrDelay)



arrDelay <- filter(otd95, ArrDelay > 0)
str(arrDelay)

depDelay <- filter(otd95, DepDelay > 0)
str(depDelay)




head(otd95 %>% select(DepDelay, DepDelayMinutes, DepDel15), 10)

otd95 %>% count(DepDel15 == 1)




str(otd95)



cancel_delay_analysis <- otd95 %>% 
  group_by(Year, Month, DayofMonth, UniqueCarrier) %>% 
  summarise(
    monthly_flight = n(),
    avg_ArrDelay = mean(ArrDelay[ArrDelay > 0], na.rm=TRUE),
    avg_DepDelay = mean(DepDelay[DepDelay > 0], na.rm=TRUE),
    sum_Cancelled = sum(is.na(DepDelay))
  )

ggplot(data = cancel_delay_analysis) +
  geom_point(aes(x=avg_ArrDelay, y=sum_Cancelled)) +
  geom_smooth(mapping = aes(x=avg_ArrDelay, y=sum_Cancelled)) +
  facet_wrap(~UniqueCarrier, nrow = 3)











airlinecount <- data.frame(dplyr::count(delayed_otd95, UniqueCarrier))
airlinecount <- airlinecount[order(airlinecount$n, decreasing = TRUE), ]

ggplot(airlinecount, aes(x = reorder(UniqueCarrier, n), y = n)) + 
  geom_bar(colour="white", fill="red", stat="identity") + 
  geom_text(aes(label=scales::comma(n)), vjust=-0.1, hjust=0.45, color='black') + 
  labs(title = "Number of Flight Delays by Airline (Based on Arrival Time)", x = "Airline", y = "Number of Delays") + 
  scale_y_continuous(labels=,) + 
  coord_flip() + theme(plot.title = element_text(hjust = 0.5))

weekdays_df <- delayed_otd95 %>%
  select(UniqueCarrier, DayOfWeek) %>%
  group_by(UniqueCarrier, DayOfWeek) %>%
  dplyr::summarise(n = length(DayOfWeek), .groups = 'keep') %>%
  data.frame()

