# |*****************************************************************************
# | Dwayne Macadangdang 9/5/2016
# | Coursera: Exploratory Data Analysis
# | Week 1 Programming Assignment: Plot 2
# |*****************************************************************************

setwd("/Users/mistermaxx/Documents/work/personal/Coursera/Exp_Data_Anal/Week_1")
library(data.table)
library(dplyr)
library(lubridate)

plot2 <- function()
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
  
  plot(energy.plot.data$DateTimeStamp, energy.plot.data$Global_active_power, type="l", xlab="", ylab="Global Active Power (in kilowatts)")
  dev.copy(png, file="plot2.png", width=480, height=480)
  dev.off()
}

