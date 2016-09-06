# |*****************************************************************************
# | Dwayne Macadangdang 9/6/2016
# | Coursera: Exploratory Data Analysis
# | Week 1 Programming Assignment: Plot 4
# |*****************************************************************************

setwd("/Users/mistermaxx/Documents/work/personal/Coursera/Exp_Data_Anal/Week_1")
library(data.table)
library(dplyr)
library(lubridate)

plot4 <- function()
{
  #read in data from the file
  energy.file.data <- read.table("household_power_consumption.txt",header=T,sep=";",colClasses=c("character","character","double","double","double","double","double","double","numeric"),na.strings="?")
  
  # blow away the NA's
  energy.subset.data <- na.omit(energy.file.data)
  
  # coerce the Data column to a date
  energy.subset.data$Date <- as.Date(energy.subset.data$Date, format="%d/%m/%Y")
  
  # filter on data on our date range
  energy.subset.data <- filter(energy.subset.data, between(energy.subset.data$Date, as.Date("2007-02-01"), as.Date("2007-02-02")))
  
  # create a new column named DateTimeStamp that combines the Date and Time columns
  energy.subset.data <- transform(energy.subset.data, DateTimeStamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
  
  # reduce the data set down to only required columns for plotting
  energy.plot.data <- select(energy.subset.data, DateTimeStamp, Global_active_power, Global_reactive_power, Voltage, Global_intensity, Sub_metering_1, Sub_metering_2, Sub_metering_3)
  
  # four plots, 2 rows, 2 columns
  par(mfrow=c(2,2))
  
  # first plot
  plot(energy.plot.data$DateTimeStamp, energy.plot.data$Global_active_power, type="l", xlab="", ylab="Global Active Power", col = "blue")
  
  # second plot
  plot(energy.plot.data$DateTimeStamp, energy.plot.data$Voltage, type="l", xlab="Date/Time", ylab="Voltage", col = "red")
  
  # third plot
  plot(energy.plot.data$DateTimeStamp, energy.plot.data$Sub_metering_1, type="l", xlab="", ylab="Energy Sub Metering", col = "orange")
  lines(energy.plot.data$DateTimeStamp, energy.plot.data$Sub_metering_2, col = "green")
  lines(energy.plot.data$DateTimeStamp, energy.plot.data$Sub_metering_3, col = "purple")
  legend("topright", col=c("orange","green","purple"), c("Sub Metering 1  ","Sub Metering 2  ", "Sub Metering 3  "),lty=c(1,1), bty="n", cex=.5)
  
  # fourth plot
  plot(energy.plot.data$DateTimeStamp, energy.plot.data$Global_reactive_power, type="l", xlab="Date/Time", ylab="Global Reactive Power")
  
  dev.copy(png, file="plot4.png", width=480, height=480)
  dev.off()
  
}

