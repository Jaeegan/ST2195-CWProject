# -*- coding: utf-8 -*-
"""
Created on Wed Mar  2 18:01:54 2022

@author: Joshua
"""

#### **Set up, import modules and data**

import os
import sqlite3
import calendar
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import calplot
from scipy.stats import chi2_contingency

# Set up settings
import warnings
warnings.filterwarnings('ignore')

from matplotlib.axes._axes import _log as matplotlib_axes_logger
matplotlib_axes_logger.setLevel('ERROR')

pd.set_option("display.max_columns", None)

sns.set(rc={"figure.dpi":300, 'savefig.dpi':300})
sns.set_context('notebook')
sns.set_style("ticks")

os.chdir("C:/Users/Joseph/Downloads/Joshua/SIM Year 2/Programing for Data Science/r/coursework/data")

# Set up function to retrieve Week of Month

def get_week_of_month(date):
    cal = calendar.Calendar(6) # Week starts Sunday
    weeks = cal.monthdayscalendar(date.year, date.month) 
    for x in range(len(weeks)):
        if date.day in weeks[x]:
            return x + 1

# Set up and open connection to the datebase

try:
    os.remove("flights2.db")
except OSError:
    pass

conn = sqlite3.connect("flights2.db")
c = conn.cursor()

# Load and write data to database

carriers = pd.read_csv("carriers.csv")
airports = pd.read_csv("airports.csv")
plane_data = pd.read_csv("plane-data.csv")

carriers.to_sql("carriers", con = conn, index = False)
airports.to_sql("airports", con = conn, index = False)
plane_data.to_sql("plane_data", con = conn, index = False)

# Load main data for preparation from CSV

flight_data = pd.read_csv("1995.csv")
for i in range(1996, 2001):
    data = pd.read_csv(str(i)+".csv")
    flight_data = pd.concat([flight_data, data], ignore_index = True)

flight_data.head(10)

### **Handele Missing Values**

# Before proceeding with the analysis:
    
# Check for the number of missing values in each column

mv = flight_data.isna().sum()
missing_values = mv.loc[(mv != 0)].sort_values()

print(missing_values)

# Visualize the missing values on a bar chart and remove any variables with, None = total number of observations

total_obs = flight_data.shape[0]

missing_values.where(missing_values < total_obs).dropna().plot.bar(
    rot = 30,
    fontsize = 8,
    title = "Bar Chart of Missing Values",
    figsize = (10,4))

plt.xlabel("Variable", fontsize = 10)
plt.ylabel("Number of Missing Values", fontsize = 10);

# Check the number of canceled or diverted flights

flight_data["Cancelled"].value_counts()
          
((flight_data["Cancelled"]) | (flight_data["Diverted"])).value_counts()

# Hence, remove the records with missing values

cancel_divr = flight_data[(flight_data.Cancelled == 1) | (flight_data.Diverted == 1)]
fd_mod = flight_data.drop(cancel_divr.index)

### **Clean and Enrich data**

# Drop columns that provide little to no information for further analysis

fd_mod = fd_mod.drop(
    columns = ["Cancelled", "Diverted", "CancellationCode", "CarrierDelay",
               "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay"])

# Make modifications to data where necessary:
# "DepDel15": binary indicator* for flights with departure delay time >= 15 mins
# "ArrDel15": binary indicator* for flights with arrival delay time >= 15 mins
# *(0 = not delayed, 1 = delayed)

fd_mod["flight_date"] = fd_mod.Year.astype(str) + "/" + fd_mod.Month.astype(str) + "/" \
    + fd_mod.DayofMonth.astype(str) 

fd_mod["season"] = fd_mod["Month"].apply(lambda x : "Spring" if x in range(3, 6) \
                                          else ("Summer" if x in range(6, 9) \
                                                else ("Autumn" if x in range(9, 12) \
                                                      else "Winter")))

fd_mod["depdel15"] = [1 if x >= 15 else 0 for x in fd_mod.DepDelay]
fd_mod["arrdel15"] = [1 if x >= 15 else 0 for x in fd_mod.ArrDelay]

fd_mod.head()

# Write main data to the database

fd_mod.to_sql("flight_data", con = conn, if_exists = "replace", index = False)

# Write data to csv file (Will be used in Q5)
fd_mod.to_csv("flight_data.csv", index = False)

### **When is it best to fly to minimize delays?**

# The columns DepDel15 and ArrDel15 are binary indicators for departure and arrival delays.

#### **General overview of arrival delays**

arrdel_htmap_query = c.execute("""
                               SELECT Year, Month, DayofWeek, flight_date, arrdel15
                               FROM flight_data
                               WHERE arrdel15 == 1
                               """).fetchall()
                               
arrdel_htmap = pd.DataFrame(arrdel_htmap_query)                               
arrdel_htmap.columns = ["year", "month", "dayofweek", "flight_date", "arrdel15"]

arrdel_htmap["flight_date"] = pd.to_datetime(arrdel_htmap["flight_date"])
arrdel_htmap["month"] = arrdel_htmap["flight_date"].dt.month_name().str[: 3]
arrdel_htmap["dayofweek"] = arrdel_htmap["flight_date"].dt.day_name().str[: 3]
arrdel_htmap.set_index("flight_date", inplace = True)

calplot.calplot(arrdel_htmap.arrdel15, cmap = "Reds", colorbar = True)
plt.suptitle("Frequency of Arrival Delays Overview", x = 0.45, y = 1.02 , fontsize = 20);

#### **Best Season to travel**

arrdel_dom_query = c.execute("""
                             SELECT DayofMonth, season, AVG(arrdel15)
                             FROM flight_data
                             GROUP BY DayofMonth, season
                             """).fetchall()
                             
# Lets visualize the best season to travel
arrdel_dom_query = c.execute("""
                             SELECT DayofMonth, season, AVG(arrdel15)
                             FROM flight_data
                             GROUP BY DayofMonth, season
                             """).fetchall()

arrdel_dom = pd.DataFrame(arrdel_dom_query)
arrdel_dom.columns = ["dayofmonth", "season", "per_del"]

arrdel_dom.pivot(index = "dayofmonth", columns =  "season", values = "per_del").plot.line(
    xticks = range(0,32),
    title = "Percentage of Arrival Delay by Season",
    xlabel = "Day of Month",
    ylabel = "Percentage of Arrival Delay",
    figsize = (10, 4));

#### **Best Month to travel**

arrdel_mth_query = c.execute("""
                             SELECT Month, AVG(arrdel15)
                             FROM flight_data
                             GROUP BY Month
                             """).fetchall()
                             
arrdel_mth = pd.DataFrame(arrdel_mth_query)
arrdel_mth.columns = ["month", "per_del"]

plt.figure(figsize = (10, 4))
ax = sns.barplot(data = arrdel_mth, x = "month", y = "per_del",
                 dodge = False,
                 hue = "per_del",
                 palette = "YlOrRd")

ax.get_legend().remove()

ax.set(title = "Percentage of Arrival Delay by Month",
       xlabel = "Month",
       ylabel = "Percentage of Arrival Delay",
       xticks = range(0, 12),
       xticklabels = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]);

for bar in ax.patches:
    anot = round(bar.get_height()*100, 2).astype("str")
    ax.annotate(anot + "%",
                (bar.get_x() + bar.get_width()/ 2, bar.get_height()),
                ha = 'center', va = 'center', 
                size = 8, xytext = (0, 6), 
                textcoords = 'offset points')

#### **Best Week to travel**

arrdel_wk_query = c.execute("""
                            SELECT Month, flight_date, arrdel15
                            FROM flight_data
                            """).fetchall()
                            
arrdel_wk = pd.DataFrame(arrdel_wk_query)
arrdel_wk.columns = ["month", "flight_date", "arrdel15"]   

arrdel_wk["flight_date"] = pd.to_datetime(arrdel_wk["flight_date"])
arrdel_wk["month"] = arrdel_wk["flight_date"].dt.month_name().str[: 3]
arrdel_wk["weekofmonth"] = [get_week_of_month(x) for x in arrdel_wk.flight_date]

arrdel_wk = arrdel_wk.groupby(["month", "weekofmonth"])["arrdel15"].mean().reset_index()
arrdel_wk.rename(columns = {"arrdel15" : "per_del"}, inplace = True)

sort_order = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

arrdel_wk.index = pd.CategoricalIndex(arrdel_wk["month"], categories = sort_order, ordered = True)
arrdel_wk = arrdel_wk.sort_index().reset_index(drop = True)

g = sns.FacetGrid(arrdel_wk, col = "month", col_wrap = 4, margin_titles = True)
g.map(sns.barplot, "weekofmonth", "per_del", dodge = False)
g.set_titles(col_template = "{col_name}")
g.fig.subplots_adjust(top = 0.8)
g.fig.suptitle("Percentage of Arrival Delay by Week by Month")
g.set(xlabel = "Week of Month",
      ylabel = "Arrival Delay %")
g.fig.set_size_inches(10, 5);

for ax in g.axes.ravel():
    for bar in ax.patches:
        anot = round(bar.get_height()*100, 2).astype("str")
        ax.annotate(anot + "%",
                    (bar.get_x() + bar.get_width()/ 2, bar.get_height()),
                    ha = 'center', va = 'center', 
                    size = 7, xytext = (0, 6), 
                    textcoords = 'offset points')

#### **Best Day of week to travel**

arrdel_dy_query = c.execute("""
                      SELECT DayOfWeek, season, AVG(arrdel15)
                      FROM flight_data
                      GROUP BY DayOfWeek, season
                      """).fetchall()

arrdel_dy = pd.DataFrame(arrdel_dy_query)
arrdel_dy.columns = ["dayofweek", "season", "per_del"]

g = sns.FacetGrid(arrdel_dy, col = "season", col_wrap = 2, margin_titles = True)
g.map(sns.barplot, "dayofweek", "per_del", dodge = False)
g.fig.subplots_adjust(top = 0.85)
g.fig.suptitle("Percentage of Arrival Delay by Day of Week")
g.set_titles(col_template = "{col_name}")
g.set(xlabel = "Day of Week",
      ylabel = "Arrival Delay (%)",
      xticklabels = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"])
g.fig.set_size_inches(10, 4);

for ax in g.axes.ravel():
    for bar in ax.patches:
        anot = round(bar.get_height()*100, 2).astype("str")
        ax.annotate(anot + "%",
                    (bar.get_x() + bar.get_width()/ 2, bar.get_height()),
                    ha = 'center', va = 'center', 
                    size = 7, xytext = (0, 4),
                    textcoords = 'offset points')

arrdel_dy[arrdel_dy.dayofweek == 6].reset_index(drop = True)

# Summary

#### **Best Time to travel**

arrdel_tm_query = c.execute("""
                            SELECT CRSDepTime, arrdel15
                            FROM flight_data
                            WHERE arrdel15 == 1
                            """).fetchall()
                            
arrdel_tm = pd.DataFrame(arrdel_tm_query)
arrdel_tm.columns = ["crsdeptime", "arrdel15"]

# Pad leading zeros
arrdel_tm["crsdeptime"] = arrdel_tm["crsdeptime"].apply("{0:-04d}".format)

# Create categorical variable, "time_bin"
time_bin = []
for x in arrdel_tm["crsdeptime"]:
    hour = x[:2]
    time_bin.append(hour)
    
time_bin = pd.DataFrame(time_bin)

arrdel_tm = arrdel_tm.merge(time_bin, left_index = True, right_index = True)
arrdel_tm.rename(columns = {0 : "time_bin"}, inplace = True)

arrdel_tm.groupby("time_bin")["arrdel15"].sum().plot.line(
    x = "time_bin",
    xticks = range(0, 24),
    title ="Arrival Delays versus Scheduled Departure Time",
    xlabel = "Schedule Departure Time",
    ylabel = "Number of Arrival Delays",
    figsize = (10,4));

### **Do older planes suffer more delays than newer planes?**

# "neg_age": proxy column for filtering invalid plane ages
# "age": dummy variable for age of the planes (0 = old planes, 1 = new planes.)

plane_date_query = c.execute("""
                             SELECT flight_date, issue_date, arrdel15
                             FROM (plane_data LEFT JOIN flight_data 
                                   ON plane_data.tailnum = flight_data.TailNum)
                             WHERE arrdel15 >= 0
                             """).fetchall()

plane_date = pd.DataFrame(plane_date_query)
plane_date.columns = ["flight_date", "issue_date", "arrdel15"]

# Coerce dates to datetime class
plane_date["flight_date"] = pd.to_datetime(plane_date["flight_date"])
plane_date["issue_date"] = pd.to_datetime(plane_date["issue_date"], 
                                          format = "%m/%d/%Y", 
                                          errors = "coerce")

time_diff = (plane_date["flight_date"] - plane_date["issue_date"])
  
plane_date["neg_age"] = [None if x < 0 else 1 for x in (time_diff / np.timedelta64(1, "Y"))]
plane_date["age"] = [1 if x <= 11 else 0 for x in (time_diff / np.timedelta64(1, "Y"))]

plane_date.dropna(inplace = True)

print(plane_date)

#### Re: *Do older planes suffer more delays than newer plane?*

# Create a contingency table

obsvtn = plane_date[["arrdel15", "age"]].reset_index(drop = True)

obsvtn["age"] = ["old" if x == 0 else "new" for x in obsvtn.age]
obsvtn["status"] = ["On-time" if x == 0 else "Delayed" for x in obsvtn.arrdel15]

xtbl_count = pd.crosstab(obsvtn["status"], obsvtn["age"], margins = True)

print(xtbl_count)

# Column percentages
xtbl_count.rename(index = {"All": "coltotal"}, inplace = True)
xtbl_per = (xtbl_count/xtbl_count.loc["coltotal"]*100).round(2)

xtbl_per.drop(columns = "All", index = "coltotal", inplace = True)

# Visualize the information on a side-by-side bar chart

ax = xtbl_per.plot.bar(rot = 0, 
                       title = "Percentage of Planes by Flight Performance grouped by Aircraft Age",
                       xlabel = "Flight Performance",
                       ylabel = "Percentage of Planes",
                       figsize = (10, 4));

for bar in ax.patches:
    anot = round(bar.get_height(), 2).astype("str")
    ax.annotate(anot + "%",
                (bar.get_x() + bar.get_width()/ 2, bar.get_height()),
                ha = 'center', va = 'center', 
                size = 8, xytext = (0, 6), 
                textcoords = 'offset points')

#### **Chi-square test for association**

# H0: There is no association between the planes' age and flight performance
# H1: There is such an association
  
# Define the data
xtbl_count.drop(columns = "All", index = "coltotal", inplace = True)
stat, p, dof, expected = chi2_contingency(xtbl_count)
  
# Interpreting the p-value
a = 0.05
print("p-value is " + str(p))
if p <= a:
    print('Reject H0')
else:
    print('H0 is not rejected')

### **How does the number of people flying between different locations change over time?**

# Check the airport with the most number of outbound flights

fd_routes_query = c.execute("""
                            SELECT Year, Month, season, Origin, Dest
                            FROM flight_data
                            """).fetchall()

fd_routes = pd.DataFrame(fd_routes_query)
fd_routes.columns = ["year", "month", "season", "origin", "dest"]

fd_routes["route"] = fd_routes.origin + "-" + fd_routes.dest

fd_routes.groupby("year")["origin"]\
    .value_counts()\
    .sort_values(ascending = False)\
    .head(10)

#### **Multiple Time Series Chart - Number of Outbound Flights from ORD airport over Time by Destination**

fd_routes["date"] = fd_routes.year.astype(str) + "-" + fd_routes.month.astype(str)

fd_routes = fd_routes[fd_routes.origin == "ORD"]\
    .groupby("date")["route"]\
    .value_counts()\
    .reset_index(name = "counts")

g = sns.FacetGrid(fd_routes, col = "route", col_wrap = 13, margin_titles = True)
g.map(sns.lineplot, "date", "counts")
g.fig.subplots_adjust(top = 0.93)
g.fig.suptitle("Number of Outbound Flights from ORD airport by Destination", fontsize = 20.0)
g.set_titles(col_template = "{col_name}")
g.set(xlabel = "Date", 
      ylabel = "Number of Flights")
g.fig.set_size_inches(15, 15);

## **Can you detect cascading failures as delays in one airport create delays in others?**

# Analyze for cascading delays between the top 10 busiest airports in the USA in terms of flight frequency.

# Flight frequency for the top 10 busiest airports/ cities

bz_airports_query = c.execute("""
                              SELECT Origin, city, COUNT(*) as count
                              FROM flight_data, airports
                              WHERE airports.iata = flight_data.Origin
                              GROUP BY Origin, city
                              ORDER BY count DESC
                              Limit 10
                              """).fetchall()

bz_airports = pd.DataFrame(bz_airports_query)
bz_airports.columns = ["iata", "airport_city", "counts"]

print(bz_airports)

# "delay_lag": lag 1-period departure delay time

delay_lag_query = c.execute("""
                      SELECT Origin, Dest, CRSDepTime, DepTime, DepDelay
                      FROM flight_data
                      WHERE CRSDepTime > 1900
                      AND CRSDepTime <= 2200
                      ORDER BY Origin, DepTime
                      """).fetchall()

delay_lag = pd.DataFrame(delay_lag_query)
delay_lag.columns = ["origin", "dest", "crsdeptime", "deptime", "depdelay"] 

delay_lag["depdelay_lag"] = delay_lag.groupby("origin")["depdelay"].shift(1)
delay_lag.dropna(inplace = True)
delay_lag.reset_index(drop = True, inplace = True)
    
print(delay_lag)

### **Relationship between the average delay against time delayed for all the previous flight**

delay_lag_ORD = delay_lag[delay_lag.origin == "ORD"].reset_index(drop = True)
delay_lag_ORD = delay_lag_ORD.groupby("depdelay_lag")["depdelay"].mean()\
    .reset_index(name = "depdelay_mean")

delay_lag_ORD.plot.scatter(s = 0.8,
                           x = "depdelay_lag", 
                           y = "depdelay_mean",
                           xticks = range(0, 1500, 120),
                           xlabel = "Previous Departure Delay",
                           ylabel = "Depparture Delay (mins)",
                           figsize = (10, 4));

#### **Cascading delays for top 10 busiest airports**

delay_lag["bz_indicator"] = delay_lag.origin.isin(bz_airports.iata)
delay_lag_bz = delay_lag[delay_lag.bz_indicator == True]    

delay_lag_bz = delay_lag_bz.groupby(["origin", "depdelay_lag"])["depdelay"].mean()\
    .reset_index(name = "depdelay_mean")

g = sns.FacetGrid(delay_lag_bz, col = "origin", col_wrap = 2, margin_titles = True)
g.map(sns.scatterplot, "depdelay_lag", "depdelay_mean", s = 7.5)
g.set_titles(col_template = "{col_name}")
g.set(xticks = range(-750, 1500, 120),
      xlabel = "Previous Departure Delay",
      ylabel = "Departure Delay (mins)")
g.fig.set_size_inches(17, 10);

conn.close()