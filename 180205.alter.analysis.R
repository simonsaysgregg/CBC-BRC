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
View(CBC_BRC.1)
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
BRC.1 <- mutate(CBC_BRC.1, rainfall = (rainfall * 25.4), 
                intensity = (intensity * 25.4),
                air.temp = (air.temp - 32)/1.8, 
                in.temp = (in.temp - 32)/1.8, 
                in.depth = (in.depth * 30.48), 
                shal.temp = (shal.temp - 32)/1.8, 
                shal.depth = (shal.depth * 30.48), 
                deep.temp = (deep.temp - 32)/1.8, 
                deep.depth = (deep.depth * 30.48), 
                out.temp = (out.temp - 32)/1.8, 
                out.depth = (out.depth * 30.48))
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




##### Boxplot Temperature
ggplot(data = CBC_BRC.2)+
  geom_boxplot(aes(x = Temperature_Location, y = Temperature))

### histogram temperature
## needs stat_bin() info
#ggplot(data = TFC_RWH.2)+
# geom_histogram(aes(x = Temperature_Location, y = Temperature, colour = Temperature_Location))

### Line plot
ggplot(data = CBC_BRC.2)+
  #theme_classic()+
  geom_line(aes(x = Date, y = Temperature, colour = Temperature_Location))



#### Summary of Events
event.sum.1 <- ddply(CBC_BRC.2, .(Event, Temperature_Location), summarise,
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
### Remove Event 0
event.sum.3 <- subset(event.sum.2, Event != "Air")
### confirm
#View(event.sum.3)
### Remove Rainfall < 1.7mm
event.sum.4 <- subset(event.sum.3, Rainfall.Accumulation > 1.7)
View(event.sum.4)


#### Finding events of interest
##### max precip
event.sum.1[which.max(event.sum.1$Rainfall.Accumulation),]
##### 

### Event == 19
max.precip.event <- subset(CBC_BRC.2, Event == 19)
## confirm
View(max.precip.event)
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
event.sum.5.1 <- filter(event.sum.4, Temperature_Location == "in" | Temperature_Location == "out")
event.sum.3.1 <- filter(event.sum.3, Temperature_Location == "in" | Temperature_Location == "out")
event.sum.4.1 <- subset(event.sum.4, Temperature_Location != "air")

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
### All events
ggplot(data = event.sum.5)+
  geom_boxplot(aes(x = Temperature_Location, y = Median.Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")


##### Boxplot Maximum Event Temperature 
### All events
ggplot(data = event.sum.3.1)+
  geom_boxplot(aes(x = Temperature_Location, y = Maximum.Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")

##### Boxplot Median Event Temperature 
### Runoff events
ggplot(data = event.sum.3.1)+
  geom_boxplot(aes(x = Temperature_Location, y = Median.Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")


##### Boxplot Maximum Event Temperature 
### runoff events
ggplot(data = event.sum.5)+
  geom_boxplot(aes(x = Temperature_Location, y = Maximum.Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")

##### Boxplot Median Temperature in out shal deep 
### Runoff events
ggplot(data = event.sum.4.1)+
  geom_boxplot(aes(x = Temperature_Location, y = Median.Temperature))+
  scale_x_discrete(labels = c("Deep", "In", "Out", "Shallow"))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")


##### Boxplot Maximum Temperature in out shal deep 
### runoff events
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
##CI 9.0 - 18.3
### psuedomed 15.5
##### Test of significance Maximum Outlet let
wilcox.test(x = BRC.sum.2$Out.max, mu = 21, conf.int = TRUE, conf.level = 0.95)
### sig cooler p-val = 0.003
##CI 9.5 - 19.3
### psuedomed 16.7

##### Differecne of medians
##### Test of significance Median Inlet & Outlet
wilcox.test(x = BRC.sum.2$Out.med, y = BRC.sum.2$In.med, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.1465
# 95% CI  -10.4033333 - 0.3805556
## psudeo-median -0.7
##### Test of significance Maximum Inlet & Outlet
wilcox.test(x = BRC.sum.2$Out.max, y = BRC.sum.2$In.max, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.166
# 95% CI  -8.796024 - 1.236642 
## psudeo-median -3.5

#### signifiacne test of in/out variance
##### Differecne of medians
##### Test of significance Median variance Inlet & Outlet
wilcox.test(x = BRC.sum.2$In.var, y = BRC.sum.2$Out.var, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.99
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
### not significantly different pval = 0.01476
# 95% CI (-3.8) - (-0.2)
## True diff pseudomedian -1.4
##### Test of significance Maximum Inlet & Outlet
wilcox.test(x = BRC.sum.1$Out.max, y = BRC.sum.1$In.max, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
###significantly different pval = 0.0032
# 95% CI (-9.9) - (-2.0)
## True difference -5.7


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
### not significantly different pval = 0.1997
# 95% CI  -6.872749  1.452205 
## psudeo-median -2.570057 
##### Test of significance medians deep and outlet
wilcox.test(x = BRC.sum.2$Out.med, y = BRC.sum.2$Deep.med, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.2279
# 95% CI  -6.384972  1.335549 
## psudeo-median -2.283922

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
### not significantly different pval = 0.2279
# 95% CI  -6.766104  1.809912 
## psudeo-median -2.188332 
##### Test of significance maximum deep and outlet
wilcox.test(x = BRC.sum.2$Out.max, y = BRC.sum.2$Deep.max, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.1237
# 95% CI  -6.860589  1.236076 
## psudeo-median -2.285539


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






