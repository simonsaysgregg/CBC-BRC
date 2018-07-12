##### RE-Analysis post-1st review
## Analysis of BRC data from calvary baptist church
## monitoring period August 2017-November 2017
## data location in CBC_BRC.DEL.csv
## Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
require("grid")
## Statistical analysis
require("stats")        # Lots of stats stuff
## Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
require("TTR")
#require("xlsx")        # creates errors # Reads and writes to xlsx file
require("purrr")
require("tidyr")
require("fBasics")
require("pls")
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Read data file
# Data file has previous manipulations
BRC <- read.csv("./Working/CBC_BRC.DEL.csv")
## View to confirm proper read
#View(BRC)

## rename columns
colnames(BRC) <- c("date.time", 
                     "rainfall", 
                     "intensity",
                     "Air.temp", 
                     "In.temp", 
                     "In.depth",
                    "Shal.temp", 
                     "Shal.depth",
                   "Deep.temp", 
                   "Deep.depth",
                     "Out.temp", 
                     "Out.depth", 
                     "event")
# Confirm
# View(BRC)

## Set date time fomat
BRC$date.time <- mdy_hm(BRC$date.time, tz = "est")
# Confirm class
#class(BRC[,1])

## Need to convert units to metric
BRC.m <- mutate(BRC, rainfall = (rainfall * 25.4), 
                  intensity = (intensity * 25.4), 
                  Air.temp = (Air.temp - 32)/1.8, 
                  In.temp = (In.temp - 32)/1.8, 
                  In.depth = (In.depth * 30.48),
                  Shal.temp = (Shal.temp - 32)/1.8, 
                  Shal.depth = (Shal.depth * 30.48),
                  Deep.temp = (Deep.temp - 32)/1.8, 
                  Deep.depth = (Deep.depth * 30.48),
                  Out.temp = (Out.temp - 32)/1.8, 
                  Out.depth = (Out.depth * 30.48))
#View(BRC.m)

## 9/5 Event
## Plot depth and rainfall
# Plot 1
plot951 <- (BRC.m) %>%
  select(date.time,
         In.depth,
         Shal.depth,
         Deep.depth,
         Out.depth) 
colnames(plot951) <- c("date.time",
                       "Inlet",
                       "Shallow Well",
                       "Deep Well",
                       "Outlet")
# Plot2
plot952 <- (BRC.m) %>%
  select(date.time,
         Air.temp,
         In.temp,
         Shal.temp,
         Deep.temp,
         Out.temp) 
colnames(plot952) <- c("date.time",
                       "Air",
                       "Inlet",
                       "Shallow Well",
                       "Deep Well",
                       "Outlet")
# Prep plotting dataset1
plot951 <- (plot951) %>%
  subset(date.time >= as.POSIXct("2017-09-05 00:00:00") & date.time <= as.POSIXct("2017-09-10 00:00:00")) %>%
  melt(id = "date.time")
# View(plot951)
# Prep plotting dataset2
plot952 <- (plot952) %>%
  subset(date.time >= as.POSIXct("2017-09-05 00:00:00") & date.time <= as.POSIXct("2017-09-10 00:00:00")) %>%
  melt(id = "date.time")
# View(plot952)
# plot1
plot1 <-ggplot(data = plot951)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Depth (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
# Plot2
plot2 <-ggplot(data = plot952)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))


## 9/11 Event
## Plot depth and rainfall
# Plot 1
plot9111 <- (BRC.m) %>%
  select(date.time,
         In.depth,
         Shal.depth,
         Deep.depth,
         Out.depth) 
colnames(plot9111) <- c("date.time",
                       "Inlet",
                       "Shallow Well",
                       "Deep Well",
                       "Outlet")
# Plot2
plot9112 <- (BRC.m) %>%
  select(date.time,
         Air.temp,
         In.temp,
         Shal.temp,
         Deep.temp,
         Out.temp) 
colnames(plot9112) <- c("date.time",
                       "Air",
                       "Inlet",
                       "Shallow Well",
                       "Deep Well",
                       "Outlet")
# Prep plotting dataset1
plot9111 <- (plot9111) %>%
  subset(date.time >= as.POSIXct("2017-09-11 00:00:00") & date.time <= as.POSIXct("2017-09-19 00:00:00")) %>%
  melt(id = "date.time")
# View(plot9111)
# Prep plotting dataset2
plot9112 <- (plot9112) %>%
  subset(date.time >= as.POSIXct("2017-09-11 00:00:00") & date.time <= as.POSIXct("2017-09-19 00:00:00")) %>%
  melt(id = "date.time")
# View(plot9112)
# plot3
plot3 <-ggplot(data = plot9111)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Depth (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
# Plot4
plot4 <-ggplot(data = plot9112)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot3), ggplotGrob(plot4), size = "last"))

## Additional statistical analysis
## Split into list of events
BRCevents <- split(BRC.m, BRC.m$event) 
# Returns a list of events 
# View(BRCevents)

## Calculates mean of Duration & Rainfall Accumulaiton 
# Returns a data frame of values same length as list
BRCsum <- BRCevents %>%
  map_df(function(df) {summarise(df, Date = min(date.time), 
                                 Duration = ((max(date.time)-min(date.time))/3600),
                                 Accumulation = sum(rainfall, na.rm = TRUE),
                                 max.intensity = max(intensity, na.rm = TRUE),
                                 medinT = median(In.temp, na.rm = TRUE), 
                                 maxinT = max(In.temp, na.rm = TRUE),
                                 medshalT = median(Shal.temp, na.rm = TRUE), 
                                 maxshalT = max(Shal.temp, na.rm = TRUE),
                                 meddeepT = median(Deep.temp, na.rm = TRUE), 
                                 maxdeepT = max(Deep.temp, na.rm = TRUE),
                                 medoutT = median(Out.temp, na.rm = TRUE), 
                                 maxoutT = max(Out.temp, na.rm = TRUE))})
# View(BRCsum)

## Breaking events into pre and post 
## subset to provide additional hydrology analsis
BRC_pre1012 <- (BRCsum[-c(1),]) %>%
  subset(Date <= "2017/10/12" & Accumulation >= 1.7) 
#View(BRC_pre1012)
## subset to provide additional hydrology analsis
BRC_post1012 <- (BRCsum[-c(1),]) %>%
  subset(Date >= "2017/10/12" & Accumulation >= 1.7) 
#View(BRC_post1012)

## Statistical Testing 
# Wilcoxon




## box plots of pre-1012
# median data
BRCpre1012med_box <- (BRC_pre1012) %>%
  select(medinT,
         medshalT,
         meddeepT,
         medoutT) %>%
  melt()
#View(BRCpre1012med_box)
# maximum data
BRCpre1012max_box <- (BRC_pre1012) %>%
  select(maxinT,
         maxshalT,
         maxdeepT,
         maxoutT) %>%
  melt()
#View(BRCpre1012max_box)

# plot median temps
ggplot(data = BRCpre1012med_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c( "In", "Shallow", "Deep", "Out"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())

# plot max temps
ggplot(data = BRCpre1012max_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c("In", "Shallow", "Deep", "Out"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())

# Scatter plot of all medians and maximums
tot.scat <- (BRCsum[-c(1:5),]) %>%
  select(Date,
         medinT,
         maxinT,
         medoutT,
         maxoutT) 
colnames(tot.scat) <- c("Date",
                        "Median In",
                        "Maximum In",
                        "Median Out",
                        "Maximum Out")
# View(tot.scat)
# Melt data set
tot.scat <- (tot.scat) %>%
  melt(id = "Date")
# Plot
ggplot(data = tot.scat, aes(x = Date))+
  geom_point(aes(y = value, shape = variable))+ 
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2017-10-12")), color = "Analysis Division"))+
  scale_shape_manual(values = c(0,1,15,16))+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_y_continuous(limits = c(0,40), expand = c(0,0))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "10 days")+
  labs(x = "Date", y = "Temperature (°C)")

## Probaility exceedance plot
## select variables from event summary table short
prob.plot <- (BRC_pre1012) %>%
  select(Date,
         medinT,
         maxinT,
         medoutT,
         maxoutT) 
# View(prob.plot)                
########### sort columns
prob.plot <- mutate(prob.plot, 
                    medinT = sort(medinT, decreasing = TRUE, na.last = TRUE),
                    maxinT = sort(maxinT, decreasing = TRUE, na.last = TRUE),
                    medoutT = sort(medoutT, decreasing = TRUE, na.last = TRUE),
                    maxoutT = sort(maxoutT, decreasing = TRUE, na.last = TRUE))
#View(prob.plot)
############ Rank 
prob.plot <- mutate(prob.plot, 
                    In.med.1 = rank(desc(medinT), na.last = TRUE),
                    In.max.1 = rank(desc(maxinT), na.last = TRUE),
                    Out.med.1 = rank(desc(medoutT), na.last = TRUE),
                    Out.max.1 = rank(desc(maxoutT), na.last = TRUE))
#View(prob.plot)
########### Calculate probabiltiy
prob.plot <- mutate(prob.plot, 
                    In.med.2 = (In.med.1 - 0.375) / (6 + 1 - (2 * 0.375)),
                    In.max.2 = (In.max.1 - 0.375) / (6 + 1 - (2 * 0.375)),
                    Out.med.2 = (Out.med.1 - 0.375) / (6 + 1 - (2 * 0.375)),
                    Out.max.2 = (Out.max.1 - 0.375) / (6 + 1 - (2 * 0.375)))
#View(prob.plot)

##### Inlet + Outlet 
ggplot(data = prob.plot)+
  geom_point(aes(x = medinT, y = In.med.2, shape = "Median Inlet"))+ 
  geom_point(aes(x = medoutT, y = Out.med.2, shape = "Median Outlet"))+
  geom_point(aes(x = maxinT, y = In.max.2, shape = "Maximum Inlet"))+ 
  geom_point(aes(x = maxoutT, y = Out.max.2, shape = "Maximum Outlet"))+
  geom_vline(aes(xintercept = 21, color = "Trout Threshold"))+
  scale_shape_manual(values = c(0,1,15,16))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(0.0,1.0), expand = c(0,0)) +
  scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Temperature (°C)", y = "Probability")
