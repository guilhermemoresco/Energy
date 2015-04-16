####################################################
##                                                ##
##       India Energy Reliability Analysis        ##
##                                                ##
####################################################


##                                                ##
###    Part 1 - Load and treat Weather Data      ###
##                                                ##

##For the first analysis we will use the weather data from the biggest city in each state as representative to state-wise weather


#Set working directory
setwd("C:/Users/Guilherme/github/Energy/NRLDC")

#load weatheR library
require(weatheR)
library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)
library(lubridate)
library(stringr)


# #Definition of input parameters to the getInterpolatedDataByCity function
# station.list <- allStations()
 cities.list <- c("Chandigarh, India", "Faridabad, Haryana, India", "Dehradun, Uttarakhand, India","Srinagar, Jammu and Rashmir, India", "Shimla, HP, India", "lahore, Pujab, India", "Delhi, India", "Rajasthan, Jaipur, India", "Kanpur, UP, India")
# #add state names
# start <- 2008
# end <- 2015
# k <- 4
# distance <- 150 #km
# hourly_interval <- 24 
# tolerance <- 0.05 #it is in % how many missed datas we tolarate

# #Get interpolated data and save it into a data frame called weather.data
# Questions:
### Can you run a list of cities with more than seven elements?
### Even if you set the hourly interval to be 24 hours it is giving hourly data. Why?



# data <- getInterpolatedDataByCity(cities.list, station.list, k, start, end, distance, hourly_interval, tolerance)
# save(weather.data, file="weatherdata.rsav")
# save(data,file="complete.data.gz")

load("complete.data.rsav")

# Load weather data from original file
weather.data <- data$station_data

#Status of chosen stations
station.status <- data$dl_status
#save(station.status,file="stationstatus.rsav")

station.interpolated <- data$interpolated
#save(station.interpolated,file="stationinterpolated.rsav")

station.chosen <- data$station_names_final
#save(station.chosen,file="stationchosen.rsav")

##Plot location of stations
# plotStations(cities.list, station.list, k)

#
## Treatment of weather data
#

# Run a DDPLY to average the hourly data in daily. Here we chose to use the interpolated data instead of the filtered
# this because the interpolated gives no NA values between data and the mean of the interpolated should be equal to the
# mean of actual data.

#weather.dailyo <- aggregate(weather.data$TEMP ~ weather.data$city + weather.data$YR + weather.data$M + weather.data$D, FUN=mean)
weather.daily <- ddply(weather.data, .(city,YR,M,D), summarize, temperature=mean(TEMP, na.rm=TRUE), dew_point = mean(DEW.POINT, na.rm=TRUE))

# Create a column containing DATE information
weather.daily$Date <- as.Date(paste(weather.daily$YR, weather.daily$M, weather.daily$D, sep="-"),format = "%Y-%m-%d")

# Create week information
weather.daily$week <- week(weather.daily$Date)

## Vector with the corresponding state for each city
city.state <- data.frame(city=c("Chandigarh","Faribadad","Dehradun","Srinagar","Shimla","lahore","Delhi","Jaipur","Kanpur"),state=c("Chandigarh","Haryana","Uttarakhand","J&R","HP","Punjab","Delhi","Rajasthan","UP"))

# Create the state column
weather.daily$state <- city.state$state[match(weather.daily$city,city.state$city)]

# Create the Class column
weather.daily$class <- c(rep("Weather",nrow(weather.daily)))

# Reorganize and rename the data frame header
weather.df <- weather.daily[,c(9,7,2,3,4,8,10,5,6)]
names(weather.df) <- c("state","date","yr","m","d","week","class","temperature degC","dewpoint degC")

# melt the data frame
weather.df <- melt(weather.df, id.vars=c("state","date","yr","m","d","week","class"))
weather.df <- weather.df[order(weather.df$state),]
rownames(weather.df) <- NULL

## Weather data frame is ready to be used



##                                                ##
###     Part 2 - Load and treat Demand Data      ###
##                                                ##

#load tables 1 to 4 and save them with their own name
load("NRLDC.Table1.rsav")
table1 <- data

load("NRLDC.Table2.rsav")
table2 <- dat

load("NRLDC.Table3.rsav")
table3 <- dat

load("NRLDC.Table4.rsav")
table4 <- dat

### Treat table 2 - Demand Class

# Create a class vector for the data frame
table2$class <- c(rep("Demand (GWh)",nrow(table2)))

# Select only desired columns
demand.df <- table2[,c(2,1,12,13,14,15,16,10,11)]

# Rename header
names(demand.df) <- c("state","date","yr","m","d","week","class","Consumption","Shortage")

# Melt demand class data frame
demand.df <- melt(demand.df, id.vars=c("state","date","yr","m","d","week","class"))
demand.df <- demand.df[order(demand.df$state),]
rownames(demand.df) <- NULL


### Treat table 3 - Shortage Class

table3$year <- strftime(table3$Date,format="%Y")
table3$month <- strftime(table3$Date,format="%m")
table3$day <- strftime(table3$Date,format="%d")

# Create a class vector for the data frame
table3$class <- c(rep("Shortage (MW)",nrow(table3)))

# Select only desired columns
shortage.df <- table3[,c(2,1,13,14,15,12,16,4,8)]

# Rename header
names(shortage.df) <- c("state","date","yr","m","d","week","class","peak shortage","off peak shortage")

# Melt shortage class data frame
shortage.df <- melt(shortage.df, id.vars=c("state","date","yr","m","d","week","class"))
shortage.df <- shortage.df[order(shortage.df$state),]
rownames(shortage.df) <- NULL


##                                      ##
###     Part 3 - Load Supply Data      ###
##                                      ##


#load coal, water and fuel (gas) stock data
load("Coal_Stock.rdata")
load("Gas_Stock.rdata")
load("Hydro_Stock.rdata")

gas <- gas[order(gas$firstname),]


## Treat coal data frame
coalstate <- read.csv("coal.state.csv")[,c(1,2)]
Coal$state <- coalstate$state[match(Coal$firstname,coalstate$station)]
Coal$state <- city.state$state[match(Coal$state,toupper(city.state$state))]

Coal <- na.omit(Coal)

# Assume EIA numbers ~ 1.904MWh per ton of coal.
# MT means one thousend metric tons
Coal$Coal_stock_GWh <- Coal$Act_Stock_MT/1.904
Coal$year <- year(Coal$Date)
Coal$month <- month(Coal$Date)
Coal$day <- day(Coal$Date)
Coal$week <- week(Coal$Date)
Coal$class <- c(rep("Stock (GWh)",nrow(Coal)))
Coal$variable <- c(rep("coal",nrow(Coal)))

coal.df <- Coal[,c(11,4,13,14,15,16,17,18,12)]
names(coal.df) <- c("state","date","yr","m","d","week","class","variable","value")
coal.df <- ddply(coal.df, .(state,date,yr,m,d,week,class,variable),summarize,value=sum(value))

## Treat hydro data frame
hydro <- hydro_monthly
month <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

hydro$month <- str_sub(hydro$uniqueMon,1,3)
hydro$year <- paste("20",str_sub(hydro$uniqueMon,4,5),sep="")
hydro$month <- match(hydro$month,month)
hydro$day <- c(rep("15",nrow(hydro)))
hydro$Date <- as.Date(paste(hydro$year, hydro$month, hydro$day, sep="-"),format = "%Y-%m-%d")
hydro$week <- week(hydro$Date)

hydro$state <- city.state$state[match(hydro$State,toupper(city.state$state))]
hydro <- na.omit(hydro)
hydro$hydro_stock_GWh <- hydro$PRL.MU

hydro$class <- c(rep("Stock (GWh)",nrow(hydro)))
hydro$variable <- c(rep("hydro",nrow(hydro)))

hydro.df <- hydro[,c(23,21,19,18,20,22,25,26,24)]

names(hydro.df) <- c("state","date","yr","m","d","week","class","variable","value")
hydro.df <- ddply(hydro.df, .(state,date,yr,m,d,week,class,variable),summarize,value=sum(value))

stock.df <- rbind(coal.df,hydro.df)


## Actual energy supplied (energy demand)
month.g <- c("jan","feb","mar","april","may","jun","jul","aug","sep","oct","nov","dec")
gas$month <- match(gas$month,month.g)
gas$day <- c(rep("15",nrow(gas)))
gas$shortage <- 







# Condense everything in one single table

plot.utt <- rbind(weather.df,demand.df,shortage.df,stock.df)
plot.utt$value <- as.factor(plot.utt$value)

plot.utt <- subset(plot.utt,  state=="Delhi") #date>"2011-01-01" & date<"2013-12-31" &

ggplot(plot.utt, aes(x=date,y=value,color=variable,linetype=variable))+geom_line()+facet_wrap(~class, scale="free_y", ncol=1)+theme_classic()


