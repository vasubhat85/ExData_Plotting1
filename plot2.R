plot2 <- function(){
  ## Constructing plot1
  ## Read the txt file "household_power_consumption.txt
  ## Subset of data considered for plot generation is from 2 days: 01/02/2007 and 02/02/2007
  ## Generate a plot to compare global active power vs. time
  
  ## Read data
  powerdata <- read.table("./household_power_consumption.txt", stringsAsFactors = FALSE, header = TRUE, sep =";"  )
  
  ## Create column in table with date and time merged together
  FullTimeDate <- strptime(paste(powerdata$Date, powerdata$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
  powerdata <- cbind(powerdata, FullTimeDate)
  
  ##change all column class to a correct class
  powerdata$Date <- as.Date(powerdata$Date, format="%d/%m/%Y")
  powerdata$Time <- format(powerdata$Time, format="%H:%M:%S")
  powerdata$Global_active_power <- as.numeric(powerdata$Global_active_power)
  powerdata$Global_reactive_power <- as.numeric(powerdata$Global_reactive_power)
  powerdata$Voltage <- as.numeric(powerdata$Voltage)
  powerdata$Global_intensity <- as.numeric(powerdata$Global_intensity)
  powerdata$Sub_metering_1 <- as.numeric(powerdata$Sub_metering_1)
  powerdata$Sub_metering_2 <- as.numeric(powerdata$Sub_metering_2)
  powerdata$Sub_metering_3 <- as.numeric(powerdata$Sub_metering_3)
  
  ## Subset data considered from 01/02/2007 to 02/02/2007
  subsetdata <- subset(powerdata, Date == "2007-02-01" | Date =="2007-02-02")
  
  ## plot global active power vs. time for the selected days
  with(subsetdata, plot(FullTimeDate, Global_active_power, type="l", xlab="Day", ylab="Global Active Power (kilowatts)"))
  
  ## Save the plot in png format with a specified dimensions
  png("plot2.png", width=480, height=480)
  dev.off()
  
}