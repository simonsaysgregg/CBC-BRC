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
require("cowplot")
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

##Begin user defined functions###########################################
## For flow calculation
# Inlet1 flow calculation function
flow.out <- function(out.depth) { 
  # comvert depth to meters
  out.depth <- out.depth * 0.01
  
  ifelse(out.depth <= 0.1143, ((796.7 * (out.depth^2.5)) / 1000), (((796.7 * (out.depth^2.5)) / 1000) + ((1.84 * 0.1905 * (out.depth^1.5)))))                      
  # IFELSE determination of which weir calculation to use; stage in meters                                             
  # TRUE V-notch flow
  # FALSE V-notch + retangular w/ supressed flow
}

# Runoff estimation function
runoff.in <- function(acc, CN, WA = 1.0378) {  # WA in acres
  # convert accumulation to inches
  acc.in <- acc * 3.94
  # surface storage
  S <- (1000/CN) - 10
  # runoff units: inches
  Q <- ((acc.in - (0.2 * S))^2)/(acc.in + (0.8 * S))
  # conversion to volume: cubic feet
  Q.vol <- Q * WA * (43560 / 12)
  # conversion to cubic meters
  Q.vol <- Q.vol * 0.0283
  return(Q.vol)
}

# Thermal Load function
therm <- function(Q, temp) {
 
  
  Q * (temp + 273.15) * 1000 * 4.18 

}
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
                  Out.depth = (Out.depth * 30.48),
                  Out.flow = flow.out(Out.depth))
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
# Plot 3
plot953 <- (BRC.m) %>%
  select(date.time,
         rainfall)
colnames(plot953) <- c("date.time",
                       "Rainfall")
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
# Prep plotting dataset3
plot953 <- (plot953) %>%
  subset(date.time >= as.POSIXct("2017-09-05 00:00:00") & date.time <= as.POSIXct("2017-09-10 00:00:00")) %>%
  melt(id = "date.time")
# View(plot953)
# plot1
plot1 <-ggplot(data = plot951)+
  geom_line(aes(x = date.time, y = value, color = variable, linetype = variable))+
  labs(x = "Date", y = "Depth (cm)")+
  theme_grey()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size = 18))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
# Plot2
plot2 <-ggplot(data = plot952)+
  geom_line(aes(x = date.time, y = value, color = variable, linetype = variable))+
  labs(x = "Date", y = "Temperature (°C)")+
  theme_grey()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size = 18))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
# Plot 3
plot3 <- ggplot(data = plot953)+
  geom_bar(aes(x = date.time, y = value, color = variable, linetype = variable), stat = "identity", size = 1)+
  labs(y = "Rainfall (mm/hr)", x = "Date")+
  scale_y_reverse()+
  theme_grey()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_blank())
gA <- ggplotGrob(plot3)
gB <- ggplotGrob(plot1)
gC <- ggplotGrob(plot2)
grid::grid.newpage()
plot_grid(gA, gB, gC, rel_heights = c(1.2,2.2,2.2), ncol = 1, align = "v")

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
# Plot 3
plot9113 <- (BRC.m) %>%
  select(date.time,
         rainfall)
colnames(plot9113) <- c("date.time",
                        "Rainfall")
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
# Prep plotting dataset3
plot9113 <- (plot9113) %>%
  subset(date.time >= as.POSIXct("2017-09-11 00:00:00") & date.time <= as.POSIXct("2017-09-19 00:00:00")) %>%
  melt(id = "date.time")
# View(plot9113)
# plot1
plot1 <-ggplot(data = plot9111)+
  geom_line(aes(x = date.time, y = value, color = variable, linetype = variable))+
  labs(x = "Date", y = "Depth (cm)")+
  theme_grey()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size = 18))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
# Plot2
plot2 <-ggplot(data = plot9112)+
  geom_line(aes(x = date.time, y = value, color = variable, linetype = variable))+
  labs(x = "Date", y = "Temperature (°C)")+
  theme_grey()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size = 18))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
# Plot 3
plot3 <- ggplot(data = plot9113)+
  geom_bar(aes(x = date.time, y = value, color = variable, linetype = variable), stat = "identity", size = 1)+
  labs(y = "Rainfall (mm)", x = "Date")+
  scale_y_reverse()+
  theme_grey()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_blank())
gA <- ggplotGrob(plot3)
gB <- ggplotGrob(plot1)
gC <- ggplotGrob(plot2)
grid::grid.newpage()
plot_grid(gA, gB, gC, rel_heights = c(1.2,2.2,2.2), ncol = 1, align = "v")



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
                                 maxoutT = max(Out.temp, na.rm = TRUE),
                                 Runoff.vol = runoff.in(Accumulation, CN = 88),
                                 Out.flow.vol = sum((Out.flow * 120), na.rm = TRUE))})
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
## Replace -Inf with NA
BRC_pre1012[1:2,11:12] <- NA

## Statistical Testing 
# Wilcoxon
# median in
wilcox.test(BRC_pre1012$medinT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max in
wilcox.test(BRC_pre1012$maxinT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# median out
wilcox.test(BRC_pre1012$medoutT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max out
wilcox.test(BRC_pre1012$maxoutT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# median shal
wilcox.test(BRC_pre1012$medshalT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max shal
wilcox.test(BRC_pre1012$maxshalT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# median deep
wilcox.test(BRC_pre1012$meddeepT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max deep
wilcox.test(BRC_pre1012$maxdeepT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
## Differenece between
# median in and out
wilcox.test(BRC_pre1012$medinT, BRC_pre1012$medoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# max in and out
wilcox.test(BRC_pre1012$maxinT, BRC_pre1012$maxoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# median in and shal
wilcox.test(BRC_pre1012$medinT, BRC_pre1012$medshalT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# max in and shal
wilcox.test(BRC_pre1012$maxinT, BRC_pre1012$maxshalT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# median shal and deep
wilcox.test(BRC_pre1012$medshalT, BRC_pre1012$meddeepT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# max shal and deep
wilcox.test(BRC_pre1012$maxshalT, BRC_pre1012$maxdeepT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# median deep and out
wilcox.test(BRC_pre1012$meddeepT, BRC_pre1012$medoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# max deep and out
wilcox.test(BRC_pre1012$maxdeepT, BRC_pre1012$maxoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
## post 1012 Differenece between
# median in and out
wilcox.test(BRC_post1012$medinT, BRC_post1012$medoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# max in and out
wilcox.test(BRC_post1012$maxinT, BRC_post1012$maxoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)



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
  scale_x_discrete(labels = c( "Inlet", "Shallow Well", "Deep Well", "Outlet"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size =24))

# plot max temps
ggplot(data = BRCpre1012max_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c("Inlet", "Shallow Well", "Deep Well", "Outlet"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size =24))

## box plots of post-1012
# median data
BRCpost1012med_box <- (BRC_post1012[-c(1),]) %>%
  select(medinT,
         medshalT,
         meddeepT,
         medoutT) %>%
  melt()
#View(BRCpost1012med_box)
# maximum data
BRCpost1012max_box <- (BRC_post1012[-c(1),]) %>%
  select(maxinT,
         maxshalT,
         maxdeepT,
         maxoutT) %>%
  melt()
#View(BRCpost1012max_box)

# plot median temps
ggplot(data = BRCpost1012med_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c( "Inlet", "Shallow Well", "Deep Well", "Outlet"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size =24))

# plot max temps
ggplot(data = BRCpost1012max_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c("Inlet", "Shallow Well", "Deep Well", "Outlet"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        text = element_text(size =24))

# Scatter plot of all medians and maximums
tot.scat <- (BRCsum[-c(1),]) %>%
  select(Date,
         Accumulation,
         medinT,
         maxinT,
         medoutT,
         maxoutT) %>%
  subset(Accumulation >= 1.7) %>%
  select(Date,
         medinT,
         maxinT,
         medoutT,
         maxoutT)
colnames(tot.scat) <- c("Date",
                        "Median Inlet",
                        "Maximum Inlet",
                        "Median Outlet",
                        "Maximum Outlet")
# View(tot.scat)
tot.scat[1:2,4:5] <- NA
# Melt data set
tot.scat <- (tot.scat) %>%
  melt(id = "Date")
# Plot
ggplot(data = tot.scat, aes(x = Date))+
  geom_point(aes(y = value, shape = variable))+ 
  geom_hline(aes(yintercept = 21, linetype = "Trout Threshold"))+
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2017-10-12")), linetype = "Analysis Division"))+
  scale_shape_manual(values = c(0,1,15,16))+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        tex = element_text(size = 18))+
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
                    In.med.2 = (In.med.1) / (6 + 1),
                    In.max.2 = (In.max.1 ) / (6 + 1 ),
                    Out.med.2 = (Out.med.1 ) / (6 + 1 ),
                    Out.max.2 = (Out.max.1 ) / (6 + 1 ))
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
  theme(legend.position = "bottom", legend.title = element_blank(),
        text = element_text(size = 18))+
  scale_y_continuous(limits = c(0.0,1.0), expand = c(0,0)) +
  scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Temperature (°C)", y = "Probability")

## 10/16 Event
## Plot depth and rainfall
# Plot 1
plot10161 <- (BRC.m) %>%
  select(date.time,
         In.depth,
         Shal.depth,
         Deep.depth,
         Out.depth) 
colnames(plot10161) <- c("date.time",
                        "Inlet",
                        "Shallow Well",
                        "Deep Well",
                        "Outlet")
# Plot2
plot10162 <- (BRC.m) %>%
  select(date.time,
         Air.temp,
         In.temp,
         Shal.temp,
         Deep.temp,
         Out.temp) 
colnames(plot10162) <- c("date.time",
                        "Air",
                        "Inlet",
                        "Shallow Well",
                        "Deep Well",
                        "Outlet")
# Prep plotting dataset1
plot10161 <- (plot10161) %>%
  subset(date.time >= as.POSIXct("2017-10-15 00:00:00") & date.time <= as.POSIXct("2017-10-17 00:00:00")) %>%
  melt(id = "date.time")
# View(plot10161)
# Prep plotting dataset2
plot10162 <- (plot10162) %>%
  subset(date.time >= as.POSIXct("2017-10-15 00:00:00") & date.time <= as.POSIXct("2017-10-17 00:00:00")) %>%
  melt(id = "date.time")
# View(plot10162)
# plot3
plot3 <-ggplot(data = plot10161)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Depth (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
# Plot4
plot4 <-ggplot(data = plot10162)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "1 days")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot3), ggplotGrob(plot4), size = "last"))

########## Temperature Duration Plot
### Event 6 & 7 
## September 11-18
##### Subset data
evnt.9.11 <- BRC.m %>% 
  select(date.time,
         In.temp, 
         Out.temp) %>%
  subset(date.time >= as.POSIXct("2017-09-11 9:00:00") & date.time <= as.POSIXct("2017-09-18 15:00:00")) 
#View(evnt.9.11)
##### filter in and out data
f.evnt.9.11 <- (evnt.9.11) %>%
  mutate(In.temp = sort(In.temp, decreasing = TRUE, na.last = TRUE),
         Out.temp = sort(Out.temp, decreasing = TRUE, na.last = TRUE), 
         in.rank = rank(desc(In.temp)),
         out.rank = rank(desc(Out.temp)),
         In.temp = signif(In.temp, digits = 3),
         Out.temp = signif(Out.temp, digits = 3))
#View(f.evnt.9.11)
### Add counter for in temps
f.evnt.9.11 <- f.evnt.9.11 %>%
  group_by(In.temp) %>%
  mutate(count.in = length(In.temp))
#View(f.evnt.9.11)
### Add counter for out temps
f.evnt.9.11 <- f.evnt.9.11 %>%
  group_by(Out.temp) %>%
  mutate(count.out = length(In.temp))
#View(f.evnt.9.11)
### Calculate time for in temps
f.evnt.9.11 <- f.evnt.9.11 %>%
  group_by(count.in) %>%
  mutate(time.in = mean(count.in)*2/60,
         time.in = signif(time.in, digits = 4))
#View(f.evnt.9.11)
### Calculation time for out temps
f.evnt.9.11 <- f.evnt.9.11 %>%
  group_by(count.out) %>%
  mutate(time.out = mean(count.out)*2/60,
         time.out = signif(time.out, digits = 4))
#View(f.evnt.9.11)
######## Select columns for in temperatures
in.temp <- (f.evnt.9.11) %>%
  ungroup() %>%
  select("In.temp", "time.in")
#View(in.temp)
######## Select columns for in temperatures
out.temp <- (f.evnt.9.11) %>%
  ungroup() %>%
  select("Out.temp", "time.out")
#View(out.temp)
##### Gather Distict observations
in.temp <- distinct(in.temp)
#View(in.temp)
out.temp <- distinct(out.temp)
#View(out.temp)
############## Plot in
ggplot()+
  geom_point(data = in.temp, aes(x = time.in, y = In.temp, shape = "Inlet"))+ 
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  geom_smooth(data = in.temp, aes(x = time.in, y = In.temp, color = "Inlet"), method = loess, se = FALSE)+ 
  geom_point(data = out.temp, aes(x = time.out, y = Out.temp, shape = "Outlet"))+
  geom_smooth(data = out.temp, aes(x = time.out, y = Out.temp, color = "Outlet"), method = loess, se = FALSE)+
  scale_shape_manual(values = c(1, 16))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size =18))+
  scale_y_continuous(limits = c(10,35), expand = c(0,0)) +
  scale_x_continuous(breaks = c(0:16)) +
  labs(x = "Duration (hrs)", y = "Temperature (°C)")

# Hydrology analysis
# Returns a data frame of values same length as list
BRCflow.ana <- BRCsum %>%
  mutate(frac.out = (Out.flow.vol/Runoff.vol) * 100,
         perc.red = ((Runoff.vol - Out.flow.vol) / Runoff.vol) * 100)
#View(BRCflow.ana)
# summary
BRCflow.anasumpre <- BRCflow.ana[-c(1:5),] %>%
  subset(Date <= "2017/10/12" & Accumulation >= 1.7) %>%
  summarise(med = median(perc.red),
            min = min(perc.red),
            max = max(perc.red),
            sumout = sum(Out.flow.vol),
            sumin = sum(Runoff.vol))
#View(BRCflow.anasumpre)
BRCflow.anasumpost <- BRCflow.ana %>%
  subset(Date >= "2017/10/12" & Accumulation >= 1.7) %>%
  summarise(med = median(perc.red),
            min = min(perc.red),
            max = max(perc.red),
            sumout = sum(Out.flow.vol),
            sumin = sum(Runoff.vol))
#View(BRCflow.anasumpost)

##Cummulative volume reduction
# pre period 
prepp <- BRCflow.anasumpre %>%
  summarise(cum = ((sumin - sumout) / sumin) * 100)
# View(prepp)
# post period 
postp <- BRCflow.anasumpost %>%
  summarise(cum = ((sumin - sumout) / sumin) * 100)
# View(postp)
cumvolreduction <- (((BRCflow.anasumpre$sumin + BRCflow.anasumpost$sumin) - (BRCflow.anasumpre$sumout + BRCflow.anasumpost$sumout)) / (BRCflow.anasumpre$sumin + BRCflow.anasumpost$sumin)) * 100
# cumvolreduction

## Thermal load reduction
## runoff volumes and outflow estimations
## median event temperatures pre/post 1012
thermal.pre <- (BRCsum) %>%
  select(Date,
         Accumulation,
         Runoff.vol,
         Out.flow.vol,
         medinT,
         medoutT) %>%
  subset(Date <= "2017/10/12" & Accumulation >= 1.7) %>%
  mutate(Runoff.therm = therm(Runoff.vol, medinT),
         Out.flow.therm = therm(Out.flow.vol, medoutT),
         therm.perc.red = (Runoff.therm - Out.flow.therm) / Runoff.therm)
#View(thermal.pre)
thermal.post <- (BRCsum) %>%
  select(Date,
         Accumulation,
         Runoff.vol,
         Out.flow.vol,
         medinT,
         medoutT) %>%
  subset(Date >= "2017/10/12" & Accumulation >= 1.7) %>%
  mutate(Runoff.therm = therm(Runoff.vol, medinT),
         Out.flow.therm = therm(Out.flow.vol, medoutT),
         therm.perc.red = (Runoff.therm - Out.flow.therm) / Runoff.therm)
#View(thermal.post)
# summary
thermpre.sum <- thermal.pre %>%
  summarise(med = median(therm.perc.red, na.rm = TRUE),
            min = min(therm.perc.red, na.rm = TRUE),
            max = max(therm.perc.red, na.rm = TRUE),
            sumout = sum(Out.flow.therm, na.rm = TRUE),
            sumin = sum(Runoff.therm, na.rm = TRUE),
            cum = (sumin - sumout) / sumin)
#View(thermpre.sum)
thermpost.sum <- thermal.post %>%
  summarise(med = median(therm.perc.red, na.rm = TRUE),
            min = min(therm.perc.red, na.rm = TRUE),
            max = max(therm.perc.red, na.rm = TRUE),
            sumout = sum(Out.flow.therm, na.rm = TRUE),
            sumin = sum(Runoff.therm, na.rm = TRUE),
            cum = (sumin - sumout) / sumin)
#View(thermpost.sum)
## cummulative load
cumthermred <- (((thermpre.sum$sumin + thermpost.sum$sumin) - (thermpre.sum$sumout + thermpost.sum$sumout)) / (thermpre.sum$sumin + thermpost.sum$sumin)) * 100
# cumthermred

## Temp-duration plot to add to Brittain Creek Temp-Dur plot
## Create temperature-duration plot
## Entire monitoring period 
## excluding data collection times
## Use BRC Outlet temp
## Select temperature variable: sort, rank, & round
BRC.tempdur <- (BRC.m) %>%
  select(date.time,
         Out.temp,
         Out.flow) %>%
  subset(Out.flow > 0) %>%
  mutate(Temp = sort(Out.temp, decreasing = TRUE, na.last = TRUE),
         T.rank = rank(desc(Temp)),
         Temp = signif(Temp, digits = 2))
## View(BRC.tempdur)

## Add counter to data fram for in temp obersvations 
## grouped_by temperature
BRC.tempdur <- BRC.tempdur %>%
  group_by(Temp) %>%
  mutate(count = length(Temp))
## View(BRC.tempdur)

## Calculate duration (at temperature hrs) of temp observations
## 1 observation = 2-min duration
BRC.tempdur <- BRC.tempdur %>%
  group_by(count) %>%
  mutate(time = mean(count)*2/60,  ## Conversion to hours
         time = signif(time, digits = 3))
## View(BRC.tempdur)

## Select temp and duration variables
BRC.tempdur1 <- (BRC.tempdur) %>%
  ungroup() %>%
  select("Temp", "time")
#View(BRC.tempdur1)

## Gather distict observations
## Should result in a single value per temperature
BRC.tempdur2 <- distinct(BRC.tempdur1)
#View(BRC.tempdur2)

## Sum time to create cummulative duration exceedance of observation temperature
BRC.tempdur2 <- (BRC.tempdur2) %>%
  mutate(cumdur = cumsum(time))
## View(BRC.tempdur2)

# Write data to file
write.csv(BRC.tempdur2, file = "./Working/BRC.temp.duration.csv")

## Plot Brittain Creek Temperature-Durations
ggplot()+
  geom_point(data = BRC.tempdur2, aes(x = cumdur, y = Temp, shape = "BRC Outflow Temperature"))+ 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_y_continuous(limits = c(5,35), 
                     expand = c(0,0)) +
  labs(x = "Duration (hrs)", y = "Temperature (°C)")



