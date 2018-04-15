##### Alternative analysis following LSVFS
#Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
#Statistical analysis
require("stats")        # Lots of stats stuff
#Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
require("xlsx")         # Reads and writes to xlsx file
#Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

CBC_BRC.1 <- read.csv("CBC_BRC.DEL.csv")
## View to confirm proper read
#View(CBC_BRC.1)
## Set date time fomat
CBC_BRC.1$date.time <- as.POSIXct(CBC_BRC.1$date.time, format = "%m/%d/%y %H:%M", tz = "est")
## Confirm
class(CBC_BRC.1[,1])
##View
#View(CBC_BRC.1)


### rename columns
colnames(CBC_BRC.1) <- c("Date", 
                         "Rainfall", 
                         "Intensity", 
                         "Air.temp", 
                         "In1.temp", 
                         "In2.depth", 
                         "Shallow1.temp", 
                         "Shallow2.depth", 
                         "Deep1.temp", 
                         "Deep2.depth", 
                         "Out1.temp", 
                         "Out2.depth", 
                         "Event")
## Convert Units
CBC_BRC.1.1 <- mutate(CBC_BRC.1, 
                      Rainfall = (Rainfall * 25.4), 
                      Intensity = (Intensity * 25.4),
                      RainCum = "[<-"(Rainfall, !is.na(Rainfall), cumsum(na.omit(Rainfall))),
                      Air.temp = (Air.temp - 32)/1.8, 
                      In1.temp = (In1.temp - 32)/1.8,
                      In2.depth = (In2.depth * 30.48),
                      Shallow1.temp = (Shallow1.temp - 32)/1.8,
                      Shallow2.depth = (Shallow2.depth * 30.48),
                      Deep1.temp = (Deep1.temp - 32)/1.8,
                      Deep2.depth = (Deep2.depth * 30.48),
                      Out1.temp = (Out1.temp - 32)/1.8,
                      Out2.depth = (Out2.depth * 30.48))
## Replace NAs with 0
#CBC_BRC.1.1[is.na(CBC_BRC.1.1)] <- 0
##### melting data set
CBC_BRC.1m <- melt(CBC_BRC.1.1, id = c("Date", 
                                    "Rainfall", 
                                    "Intensity", 
                                    "Event", 
                                    "RainCum",
                                    "In2.depth", 
                                    "Shallow2.depth", 
                                    "Deep2.depth", 
                                    "Out2.depth"))
## Confirm
#View(CBC_BRC.1m)
### rename columns
colnames(CBC_BRC.1m) <- c("Date", 
                         "Rainfall", 
                         "Intensity", 
                         "Event", 
                         "RainCum",
                         "In2.depth", 
                         "Shallow2.depth", 
                         "Deep2.depth", 
                         "Out2.depth", 
                         "Temperature_Location",
                         "Temperature")
##Confirm
#View(CBC_BRC.1m)
## Spliting text
BRCsplit <- unlist(strsplit(as.character(CBC_BRC.1m$Temperature_Location), "\\."))
## Reorganizing split
dim(BRCsplit) = c(2, nrow(CBC_BRC.1m))
BRCsplit <- data.frame(t(BRCsplit))
## Confirm
#View(BRCsplit)
#### Adding it back
CBC_BRC.1m$Temperature_Location <- BRCsplit[,1]
#TFC_RWH.m$temp <- RWHsplit[,2]
## Confirm
#View(CBC_BRC.1m)
###### Second Melt
##### melting data set
CBC_BRC.2m <- melt(CBC_BRC.1m, id = c("Date", 
                                      "Rainfall", 
                                      "Intensity", 
                                      "Event", 
                                      "RainCum",
                                      "Temperature_Location", 
                                      "Temperature"))
## Confirm
#View(CBC_BRC.2m)
### rename columns
colnames(CBC_BRC.2m) <- c("Date", 
                          "Rainfall", 
                          "Intensity", 
                          "Event", 
                          "RainCum",
                          "Temperature_Location", 
                          "Temperature", 
                          "Depth_Location", 
                          "Depth")
##Confirm
#View(CBC_BRC.2m)
## Spliting text
BRCsplit.1 <- unlist(strsplit(as.character(CBC_BRC.2m$Depth_Location), "\\."))
## Reorganizing split
dim(BRCsplit.1) = c(2, nrow(CBC_BRC.2m))
BRCsplit.1 <- data.frame(t(BRCsplit.1))
## Confirm
#View(BRCsplit.1)
#### Adding it back
CBC_BRC.2m$Depth_Location <- BRCsplit.1[,1]
#TFC_RWH.m$temp <- RWHsplit[,2]
## Confirm
#View(CBC_BRC.2m)
##Write to file
write.csv(CBC_BRC.2m, file = "CBC_BRC.2m.csv")



################## Secondary functions****************
###### Reproducing event summary
###### melted and unmelted
#View(CBC_BRC.1)
## Need to convert units
BRC.1 <- mutate(CBC_BRC.1, Rainfall = (Rainfall * 25.4), 
                Intensity = (Intensity * 25.4),
                Air.temp = (Air.temp - 32)/1.8, 
                In1.temp = (In1.temp - 32)/1.8, 
                In2.depth = (In2.depth * 30.48), 
                Shallow1.temp = (Shallow1.temp - 32)/1.8, 
                Shallow2.depth = (Shallow2.depth * 30.48), 
                Deep1.temp = (Deep1.temp - 32)/1.8, 
                Deep2.depth = (Deep2.depth * 30.48), 
                Out1.temp = (Out1.temp - 32)/1.8, 
                Out2.depth = (Out2.depth * 30.48))
#View(BRC.1)
## Replace NAs with 0
#BRC.1[is.na(BRC.1)] <- 0
### rename columns
colnames(BRC.1) <- c("Date", 
                     "Rainfall", 
                     "Intensity", 
                     "Air.Temperature", 
                     "In1.Temperature", 
                     "In2.Depth", 
                     "Shal1.Temperature", 
                     "Shal2.Depth", 
                     "Deep1.Temperature", 
                     "Deep2.Depth", 
                     "Out1.Temperature", 
                     "Out2.Depth", 
                     "Event")
##Confirm
View(BRC.1)

### Correlating Scatter plots 
ggplot()+
  geom_point(data = BRC.1, aes(x = In2.Depth, y = Out2.Depth))+ 
  geom_abline()+
  #geom_point(data = BRC.1 , aes(x = Rainfall.Accumulation, y = Maximum_Temperature, shape = Temperature_Location))+
  #scale_shape_manual(values = c(0,1,15,16), labels = c("Med In", "Max In", "Med Out", "Max Out"))+
  #ggtitle("Median & Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  #scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Rainfall", y = "Depth (cm)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))

#### Summary of Events
BRC.sum <- ddply(BRC.1, .(Event), summarise,
                     Start = min(Date),
                     End = max(Date),
                     Duration = max(Date) - min(Date),
                     Rainfall.Accumulation = sum(Rainfall, na.rm = TRUE),
                     Maximum.Intensity = max(Intensity),
                     Air.med = median(Air.Temperature), 
                     Air.max = max(Air.Temperature),
                     Air.var = var(Air.Temperature),
                     In.med = median(In1.Temperature), 
                     In.max = max(In1.Temperature),
                     In.var = var(In1.Temperature),
                     Shal.med = median(Shal1.Temperature), 
                     Shal.max = max(Shal1.Temperature),
                     Shal.var = var(Shal1.Temperature),
                     Deep.med = median(Deep1.Temperature), 
                     Deep.max = max(Deep1.Temperature),
                     Deep.var = var(In1.Temperature),
                     Out.med = median(Out1.Temperature), 
                     Out.max = max(Out1.Temperature),
                     Out.var = var(Out1.Temperature))
#### Confirm 
#View(BRC.sum)
### Remove Event 0
BRC.sum.1 <- subset(BRC.sum, Event != 0)
#### Confirm 
#View(BRC.sum.1)
### Remove small Events < 1.73mm
BRC.sum.2 <- subset(BRC.sum.1, Rainfall.Accumulation >= 1.73)
length(BRC.sum.2)
#### 13 Events



#### Summary of Events************ This calaculates summar stats incorrectly
event.sum.1 <- ddply(CBC_BRC.2m, .(Event, Temperature_Location), summarise,
                     Start = min(Date),
                     End = max(Date),
                     Duration = max(Date) - min(Date),
                     Rainfall.Accumulation = sum(Rainfall, na.rm = TRUE),
                     Maximum.Intensity = max(Intensity),
                     Average.Temperature = mean(Temperature),
                     Median.Temperature = median(Temperature),
                     Mininum.Temperature = min(Temperature),
                     Maximum.Temperature = max(Temperature),
                     Temperature.Variance = var(Temperature))
#### Confirm 
View(event.sum.1)
### Write .csv export data table
write.csv(event.sum.1, "Event.Sum.total.csv")

### Remove Event 0
event.sum.2 <- subset(event.sum.1, Event != 0)
### confirm
#View(event.sum.2)
### Write .csv export data table
write.csv(event.sum.2, "Event.Sum.short.csv")
### Remove Locaiton Air
event.sum.3 <- subset(event.sum.2, Temperature_Location != "Air")
### confirm
#View(event.sum.3)
### Remove Rainfall < 1.73mm (4) ******** Becafefull
event.sum.4 <- subset(event.sum.3, Rainfall.Accumulation >= 6.93)
View(event.sum.4)


#### Finding events of interest
##### max precip
event.sum.1[which.max(event.sum.1$Rainfall.Accumulation),]
##### 

### Event == 19
max.precip.event <- subset(CBC_BRC.2m, Event == 19)
## confirm
#View(max.precip.event)
#### Plot event
ggplot(data = max.precip.event)+
  geom_line(aes(x = Date, y = Temperature, colour = Temperature_Location))
### Rainfall
ggplot(data = max.precip.event)+
  geom_line(aes(x = Date, y = Rainfall), na.rm = T)


##### find temperature events of interest ######
BRC.sum.2[which.max(BRC.sum.2$In.med), ]
# Maximum Median inlet temp Event == 7
BRC.sum.2[which.max(BRC.sum.2$In.max), ]
# Maxiumum Max in temperature Event == 4

##### Total event plot attempt 
ggplot()+
  ggtitle("Event Thermogrpah")+
  geom_line(data = CBC_BRC.2m, aes(x= Date, y = Temperature, color = Temperature_Location))+
  geom_line(data = CBC_BRC.2m, aes(x= Date, y = Depth/2.5, color = Depth_Location))+
  #geom_line(data = evnt.7.1, aes(x= Date, y = RainCum/2.5))+
  #scale_linetype_manual(values = c("dotted", "dashed", "twodash", "longdash", "dotdash"), labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  scale_y_continuous(sec.axis = sec_axis(~./2.5, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

#### subset event Event == 7
##### Event 7 subset and plotting
evnt.7 <- subset(CBC_BRC.2m, Event == 7)
evnt.7.1 <- subset(CBC_BRC.2m, Date >= as.POSIXct("2017-09-10 00:00") & Date <= as.POSIXct("2017-09-18 15:00"))
##### Plot temperature
#ggplot(data = evnt.1, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  #geom_line()
###### Plot Depth
#ggplot(data = evnt.1.1, aes(x = Date, y = Depth, colour = Depth_Location))+
 # geom_line()
##### Advance plot attempt Event1
ggplot()+
  ggtitle("September 11th Thermogrpah")+
  geom_line(data = evnt.7.1, aes(x= Date, y = Temperature, color = Temperature_Location))+
  geom_line(data = evnt.7.1, aes(x= Date, y = Depth/2.5, color = Depth_Location))+
  #geom_line(data = evnt.7.1, aes(x= Date, y = RainCum/2.5))+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999"), labels = c("Air Temp", "Deep Temp", "Deep Depth", "In Temp", "In Depth", "Out Temp", "Out Depth", "Shallow Temp", "Shallow Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  scale_y_continuous(sec.axis = sec_axis(~./2.5, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

#### subset event Event == 4
##### Event 7 subset and plotting
evnt.4 <- subset(CBC_BRC.2m, Event == 4)
evnt.4.1 <- subset(CBC_BRC.2m, Date >= as.POSIXct("2017-09-5 10:00") & Date <= as.POSIXct("2017-09-10 00:00"))
##### Plot temperature
#ggplot(data = evnt.1, aes(x = Date, y = Temperature, colour = Temperature_Location))+
#geom_line()
###### Plot Depth
#ggplot(data = evnt.1.1, aes(x = Date, y = Depth, colour = Depth_Location))+
# geom_line()
##### Advance plot attempt Event1
ggplot()+
  ggtitle("September 5th Thermogrpah")+
  geom_line(data = evnt.4.1, aes(x= Date, y = Temperature, color = Temperature_Location))+
  geom_line(data = evnt.4.1, aes(x= Date, y = Depth/2.5, color = Depth_Location))+
  #geom_line(data = evnt.7.1, aes(x= Date, y = RainCum/2.5))+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999"), labels = c("Air Temp", "Deep Temp", "Deep Depth", "In Temp", "In Depth", "Out Temp", "Out Depth", "Shallow Temp", "Shallow Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  scale_y_continuous(sec.axis = sec_axis(~./2.5, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

######## Filter only in and out temperatures for plot
event.sum.5 <- filter(event.sum.4, Temperature_Location == "In1" | Temperature_Location == "Out1")
event.sum.5.1 <- filter(event.sum.4, Temperature_Location == "In1" | Temperature_Location == "Out1")
event.sum.3.1 <- filter(event.sum.3, Temperature_Location == "In1" | Temperature_Location == "Out1")
event.sum.4.1 <- subset(event.sum.4, Temperature_Location != "Air")
####### Data sets from ^^^ have duplicate names scatter plot below is faulty******
###### Scatter Plot 
ggplot()+
  geom_point(data = event.sum.5, aes(x = Start, y = Median.Temperature, shape = Temperature_Location))+ 
  geom_point(data = event.sum.5.1, aes(x = Start, y = Maximum.Temperature, shape = Temperature_Location))+
  scale_shape_manual(values = c(0,1,15,16), labels = c( "Max In","Med In", "Max Out", "Med Out"))+
  ggtitle("Median & Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Date", y = "Temperature (°C)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))

##### Boxplot Median Event Temperature 
### All events in & out
ggplot(data = event.sum.3.1)+
  geom_boxplot(aes(x = Temperature_Location, y = Median.Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
##### Boxplot Maximum Event Temperature 
### All events in & out
ggplot(data = event.sum.3.1)+
  geom_boxplot(aes(x = Temperature_Location, y = Maximum.Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")

##### Boxplot Median Event Temperature 
### Runoff events in & out
ggplot(data = event.sum.5)+
  geom_boxplot(aes(x = Temperature_Location, y = Median.Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
##### Boxplot Maximum Event Temperature 
### runoff events in & out
ggplot(data = event.sum.5)+
  geom_boxplot(aes(x = Temperature_Location, y = Maximum.Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")

##### Boxplot Median Temperature in out shal deep 
### Runoff events all locations
ggplot(data = event.sum.4.1)+
  geom_boxplot(aes(x = Temperature_Location, y = Median.Temperature))+
  scale_x_discrete(labels = c("Deep", "In", "Out", "Shallow"))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
##### Boxplot Maximum Temperature in out shal deep 
### runoff events all locations
ggplot(data = event.sum.4.1)+
  geom_boxplot(aes(x = Temperature_Location, y = Maximum.Temperature))+
  scale_x_discrete(labels = c("Deep", "In", "Out", "Shallow"))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")

#### Event temperature density exploration
evnt.temp.den <- subset(CBC_BRC.2m, Event != 0)
#### Group by temperature location
evnt.temp.den.in <- filter(evnt.temp.den, Temperature_Location == "In1")

######### Temp density plot************
ggplot(evnt.temp.den.in, aes())+
  geom_histogram(aes(x = Temperature), binwidth = 5 )+
  ggtitle("In Temp Histogram")+
  #labs( x = "Rainfall Accumulation (mm)", y = "Event Count")+
  theme(plot.title = element_text(hjust = 0.5))



####### Statistical Testing Reduced dataset
###### Wilcoxon
### Wilcoxon RankSumming
#### BRC.sum.2
##### Test of significance Median Inlet
wilcox.test(x = BRC.sum.2$In.med, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 0.02
##CI 15.2 - 20.3
### psuedomed 17.7
##### Test of significance Maximum Inlet
wilcox.test(x = BRC.sum.2$In.max, mu = 21, conf.int = TRUE, conf.level = 0.95)
### not sig different p-val = 0.4548
##CI 16.5 - 23.3
### psuedomed 18.9
### Wilcoxon RankSumming
##### Test of significance Median Outlet
wilcox.test(x = BRC.sum.2$Out.med, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 0.002
##CI 14.3 - 18.3
### psuedomed 16.7
##### Test of significance Maximum Outlet let
wilcox.test(x = BRC.sum.2$Out.max, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 0.005
##CI 15.6 - 19.8
### psuedomed 17.8

##### Differecne of medians
##### Test of significance Median Inlet & Outlet
wilcox.test(x = BRC.sum.2$Out.med, y = BRC.sum.2$In.med, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.4648
# 95% CI  -1.2500000 - 0.9944444
## psudeo-median -0.2895
##### Test of significance Maximum Inlet & Outlet
wilcox.test(x = BRC.sum.2$Out.max, y = BRC.sum.2$In.max, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.4171
# 95% CI  -5.938935  1.902832 
## psudeo-median   -1.81945 

#### signifiacne test of in/out variance
##### Differecne of medians
##### Test of significance Median variance Inlet & Outlet
wilcox.test(x = BRC.sum.2$In.var, y = BRC.sum.2$Out.var, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.3652
#View(summary.3)

################ Recomputing monthly stats for confirmation
## Perform monthly stats
##### Can us for wheater summary
BRC_monthly.stats <- group_by(BRC.sum.2, month=floor_date(BRC.sum.2$Start, "month")) %>% 
                     summarise(min.lenght = min(duration), 
                               max.lenght = max(duration), 
                               med.length = median(duration), 
                               min.rain = min(rainfall_acc, na.rm = TRUE), 
                               max.rain = max(rainfall_acc, na.rm = TRUE), 
                               med.rain = median(rainfall_acc, na.rm = TRUE), 
                               rainfall_sum = sum(rainfall_acc, na.rm = TRUE), 
                               max.intensity = max(intensity.peak, na.rm = TRUE), 
                               avg.air = mean(avg.air, na.rm = TRUE),
                               min.air = min(min.air, na.rm = TRUE), 
                               max.air = max(max.air, na.rm =TRUE), 
                               med.air = median(med.air, na.rm = TRUE))
## Confirm
View(BRC_monthly.stats)
## Write .csv
write.csv(BRC_monthly.stats, file = "CBC_BRC.mon.stats.csv")
####### Monthly runoff summary
##### Summary of temperatures at all brc locations
BRC_runoff.monthly.stats <- group_by(BRC.sum.2, month=floor_date(BRC.sum.2$Start, "month")) %>%
                            summarise(med.in = median(In.med, na.rm = TRUE), 
                                      max.in = max(In.max, na.rm = TRUE), 
                                      med.shal = median(Shal.med, na.rm = TRUE), 
                                      max.shal = max(Shal.max, na.rm = TRUE), 
                                      med.deep = median(Deep.med, na.rm = TRUE), 
                                      max.deep = max(Deep.max, na.rm = TRUE), 
                                      med.out = median(Out.med, na.rm = TRUE), 
                                      max.out = max(Out.max, na.rm = TRUE))
## Confirm
View(BRC_runoff.monthly.stats)
## Write .csv
write.csv(BRC_runoff.monthly.stats, file = "CBC_BRC.runff.mon.stats.csv")


####### Statistical Testing of monthly runooff values
###### Wilcoxon
### Wilcoxon RankSumming
#### BRC.sum.2
##### Test of significance Median Inlet
wilcox.test(x = BRC_runoff.monthly.stats$med.in, mu = 21, conf.int = TRUE, conf.level = 0.95)
### Not sig dif
##### Test of significance Maximum Inlet
wilcox.test(x = BRC_runoff.monthly.stats$max.in, mu = 21, conf.int = TRUE, conf.level = 0.95)
### not sig different
### Wilcoxon RankSumming
##### Test of significance Median Outlet
wilcox.test(x = BRC_runoff.monthly.stats$med.out, mu = 21, conf.int = TRUE, conf.level = 0.95)
### Not sig dif 
##### Test of significance Maximum Outlet let
wilcox.test(x = BRC_runoff.monthly.stats$max.out, mu = 21, conf.int = TRUE, conf.level = 0.95)
### Not sig dif 

####### Statistical Testing of whole event dataset runoff values
###### Wilcoxon
### Wilcoxon RankSumming
#### BRC.sum.1
##### Test of significance Median Inlet
wilcox.test(x = BRC.sum.1$In.med, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 0.0022
##CI 15.3 - 19.1
### psuedomed 17.2
##### Test of significance Maximum Inlet
wilcox.test(x = BRC.sum.1$In.max, mu = 21, conf.int = TRUE, conf.level = 0.95)
### not sig different p-val = 0.5
##CI 19.0 - 24.7
### psuedomed 21.9
### Wilcoxon RankSumming
##### Test of significance Median Outlet
wilcox.test(x = BRC.sum.1$Out.med, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 3.312e-06
##CI 11.3 - 16.6
### psuedomed 14.7
##### Test of significance Maximum Outlet let
wilcox.test(x = BRC.sum.1$Out.max, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 0.002
## CI 12.9 - 18.7
### psuedomed 16.4
##### Differecne of medians whole dataset
##### Test of significance Median Inlet & Outlet
wilcox.test(x = BRC.sum.1$Out.med, y = BRC.sum.1$In.med, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.1336
# 95% CI (-1.74) - (0.29)
## True diff pseudomedian -0.678
##### Test of significance Maximum Inlet & Outlet
wilcox.test(x = BRC.sum.1$Out.max, y = BRC.sum.1$In.max, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
###significantly different pval = 0.02252
# 95% CI (-7.15) - (-0.48)
## True difference -3.9


####### Statistical Testing Reduced dataset/ shal/deep
###### Wilcoxon
### Wilcoxon RankSumming
#### BRC.sum.2
##### Test of significance Median shallow
wilcox.test(x = BRC.sum.2$Shal.med, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 0.0266
##CI 15.1 - 20.7
### psuedomed 17.8
##### Test of significance Maximum shallow
wilcox.test(x = BRC.sum.2$Shal.max, mu = 21, conf.int = TRUE, conf.level = 0.95)
### not sig different p-val = 0.0934
##CI 15.5 - 21.6
### psuedomed 18.4
### Wilcoxon RankSumming
##### Test of significance Median deep
wilcox.test(x = BRC.sum.2$Deep.med, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 0.0175
##CI 15.0 - 20.5
### psuedomed 17.7
##### Test of significance Maximum deep
wilcox.test(x = BRC.sum.2$Deep.max, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 0.142
##CI 16.1 - 21.5
### psuedomed 18.4

##### Differecne of medians between in and shallow and deep
##### Test of significance Median Inlet & shal
wilcox.test(x = BRC.sum.2$Shal.med, y = BRC.sum.2$In.med, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.8926
# 95% CI   -0.6222222  1.2919444
## psudeo-median  0.09583333
##### Differecne of medians between shallow and deep
##### Test of significance Median shallow and deep
wilcox.test(x = BRC.sum.2$Deep.med, y = BRC.sum.2$Shal.med, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.5756
# 95% CI   -0.2860578  0.2885569
## psudeo-median 0.04997648
##### Test of significance medians shallow and outlet
wilcox.test(x = BRC.sum.2$Out.med, y = BRC.sum.2$Shal.med, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.494
# 95% CI  -4.951111  2.100556 
## psudeo-median -1.333889 
##### Test of significance medians deep and outlet
wilcox.test(x = BRC.sum.2$Out.med, y = BRC.sum.2$Deep.med, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.5428
# 95% CI  -4.208954  2.107803
## psudeo-median  -1.046177 

##### Differecne of MAXIMUM between in and shallow and deep and out
##### Test of significance Max In and shallow
wilcox.test(x = BRC.sum.2$Shal.max, y = BRC.sum.2$In.max, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.5382
# 95% CI  -4.863335  2.862735 
## psudeo-median -1.237209 
##### Test of significance max deep & shallow
wilcox.test(x = BRC.sum.2$Deep.max, y = BRC.sum.2$Shal.max, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval =  0.1261
# 95% CI  -0.1916243  0.7153554
## psudeo-median 0.287457 
##### Test of significance maximum shallow and outlet
wilcox.test(x = BRC.sum.2$Out.max, y = BRC.sum.2$Shal.max, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval =  0.5428
# 95% CI  -4.481151  2.698942
## psudeo-median -0.9517396 
##### Test of significance maximum deep and outlet
wilcox.test(x = BRC.sum.2$Out.max, y = BRC.sum.2$Deep.max, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.3243
# 95% CI  -4.188895  2.408390 
## psudeo-median  -1.0522 


########### Explore linear model of inlet temps v outlet temps 
########## with possible addition of shal and deep temps as additional predictors
##### Median temperatures
scatter.smooth(x=BRC.sum.2$In.med, y=BRC.sum.2$Out.med, main="Out.med ~ In.med")  # scatterpl
##### Maximum temperatures
scatter.smooth(x=BRC.sum.2$In.max, y=BRC.sum.2$Out.max, main="Out.max ~ In.max")  # scatterpl
#### Corelation between variables
#### Median temperautres
cor(BRC.sum.2$Out.med, BRC.sum.2$In.med)
## == -0.0009834274
#### Maximum temperautres
cor(BRC.sum.2$Out.max, BRC.sum.2$In.max)
## == -0.3898169

### Linear model
BRC.lm <- lm(Out.med ~ In.med, data = BRC.sum.2)
summary(BRC.lm)



############# Probaility exceedance plot
#### select variables from event summary table short
BRC.ex <- (BRC.sum.2[ ,c(10,11,19,20)]) 
#View(BRC.exceed)                
########### sort columns
BRC.ex <- mutate(BRC.ex, 
                     In.med = sort(In.med, decreasing = TRUE, na.last = TRUE),
                     In.max = sort(In.max, decreasing = TRUE, na.last = TRUE),
                     Out.med = sort(Out.med, decreasing = TRUE, na.last = TRUE),
                     Out.max = sort(Out.max, decreasing = TRUE, na.last = TRUE))
View(BRC.ex)
############ Rank 
BRC.ex <- mutate(BRC.ex, 
                           In.med.1 = rank(desc(In.med), na.last = TRUE),
                           In.max.1 = rank(desc(In.max), na.last = TRUE),
                           Out.med.1 = rank(desc(Out.med), na.last = TRUE),
                           Out.max.1 = rank(desc(Out.max), na.last = TRUE))
View(BRC.ex)
########### Calculate probabiltiy
BRC.ex <- mutate(BRC.ex, 
                    In.med.2 = (In.med.1 - 0.375) / (13 + 1 - (2 * 0.375)),
                    In.max.2 = (In.max.1 - 0.375) / (13 + 1 - (2 * 0.375)),
                    Out.med.2 = (Out.med.1 - 0.375) / (13 + 1 - (2 * 0.375)),
                    Out.max.2 = (Out.max.1 - 0.375) / (13 + 1 - (2 * 0.375)))
View(BRC.ex)
############# Exceedance probability plots
##### Inlet
ggplot(data = BRC.ex)+
  geom_point(aes(x = In.med, y = In.med.2, shape = "Inlet"))+ 
  geom_point(aes(x = Out.med, y = Out.med.2, shape = "Outlet"))+
  scale_shape_manual(values = c(15,16))+
  ggtitle("Median Temperature Probability Exceedance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(0.0,1.0), expand = c(0,0)) +
  scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Temperature (°C)", y = "Probability")
##### Outlet
ggplot(data = BRC.ex)+
  geom_point(aes(x = In.max, y = In.max.2, shape = "Inlet"))+ 
  geom_point(aes(x = Out.max, y = Out.max.2, shape = "Outlet"))+
  scale_shape_manual(values = c(15,16))+
  ggtitle("Maximum Temperature Probability Exceedance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(0.0,1.0), expand = c(0,0)) +
  scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Temperature (°C)", y = "Probability")
##### Inlet + Outlet 
ggplot(data = BRC.ex)+
  geom_point(aes(x = In.med, y = In.med.2, shape = "Median Inlet"))+ 
  geom_point(aes(x = Out.med, y = Out.med.2, shape = "Median Outlet"))+
  geom_point(aes(x = In.max, y = In.max.2, shape = "Maximum Inlet"))+ 
  geom_point(aes(x = Out.max, y = Out.max.2, shape = "Maximum Outlet"))+
  scale_shape_manual(values = c(0,1,15,16))+
  ggtitle("Inlet & Outlet Temperature Probability Exceedance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(0.0,1.0), expand = c(0,0)) +
  scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Temperature (°C)", y = "Probability")


########## Temperature Duration Plot
### Event 6 & 7 
## September 11-18
##### Subset data
evnt.9.11 <- subset(BRC.1, Date >= as.POSIXct("2017-09-11 9:00") & Date <= as.POSIXct("2017-09-18 15:00")) %>%
             select("In1.Temperature", "Out1.Temperature")
#View(evnt.9.11)
##### filter in and out data
f.evnt.9.11 <- (evnt.9.11) %>%
                mutate(In1.Temperature = sort(In1.Temperature, decreasing = TRUE, na.last = TRUE),
                       Out1.Temperature = sort(Out1.Temperature, decreasing = TRUE, na.last = TRUE), 
                       in.rank = rank(desc(In1.Temperature)),
                       out.rank = rank(desc(Out1.Temperature)),
                       In1.Temperature = signif(In1.Temperature, digits = 3),
                       Out1.Temperature = signif(Out1.Temperature, digits = 3))
#View(f.evnt.9.11)
### Add counter for in temps
f.evnt.9.11 <- f.evnt.9.11 %>%
                    group_by(In1.Temperature) %>%
                    mutate(count.in = length(In1.Temperature))
#View(f.evnt.9.11)
### Add counter for out temps
f.evnt.9.11 <- f.evnt.9.11 %>%
                    group_by(Out1.Temperature) %>%
                    mutate(count.out = length(In1.Temperature))
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
              select("In1.Temperature", "time.in")
#View(in.temp)
######## Select columns for in temperatures
out.temp <- (f.evnt.9.11) %>%
              ungroup() %>%
              select("Out1.Temperature", "time.out")
#View(out.temp)
##### Gather Distict observations
in.temp <- distinct(in.temp)
#View(in.temp)
out.temp <- distinct(out.temp)
#View(out.temp)
############## Plot in
ggplot()+
  geom_point(data = in.temp, aes(x = time.in, y = In1.Temperature, shape = "In"))+ 
  #geom_point(data = out.temp, aes(x = time.out, y = Out1.Temperature, shape = "Out"))+
  #scale_shape_manual(values = c(0, 15))+
  ggtitle("Temperature-Duration Plot")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(5,35), expand = c(0,0)) +
  #scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Duration (hrs)", y = "Temperature (°C)")
############## Plot out
ggplot()+
  #geom_point(data = in.temp, aes(x = time.in, y = In1.Temperature, shape = "In"))+ 
  geom_point(data = out.temp, aes(x = time.out, y = Out1.Temperature, shape = "Out"))+
  scale_shape_manual(values = c(1))+
  ggtitle("Temperature-Duration Plot")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(5,35), expand = c(0,0)) +
  #scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Duration (hrs)", y = "Temperature (°C)")


###### Some monthyl stats
BRC_air.monthly.stats <- group_by(BRC.1, month=floor_date(BRC.1$Date, "month")) %>% summarise(Average.Temperature = mean(Air.Temperature, na.rm = TRUE), 
                                                                                              Maximum.Temperature = max(Air.Temperature, na.rm = TRUE),
                                                                                              rainfall.acc = sum(Rainfall, na.rm = TRUE))
##### Confirm
View(BRC_air.monthly.stats)


