## Script for event deliniation and precip analysis

## Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
## Statistical analysis
require("stats")        # Lots of stats stuff
## Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
require("purrr")
require("tidyr")
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Requires called data file in local directory
## Read data from CSV
data.tot <- read.csv("CBC_BRC.DEL.csv")
## View to confirm proper read
#View(data.tot)

## Set date time fomat
data.tot$date.time <- as.POSIXct(data.tot$date.time, 
                                 format = "%m/%d/%y %H:%M", 
                                 tz = "est")
## Confirm
#class(data.tot[,1])
## View
#View(data.tot)

## Rename columns
colnames(data.tot) <- c("Date", 
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
data.metric <- mutate(CBC_BRC.1, 
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