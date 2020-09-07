plot1 <- function(){
  ## Constructing plot1
  ## Read the txt file "household_power_consumption.txt
  ## Subset of data considered for plot generation is from 2 days: 01/02/2007 and 02/02/2007
  ## Generate a histogram observe the global active power in kilowatts
  
  ## Read data
  powerdata <- read.table("./household_power_consumption.txt", stringsAsFactors = FALSE, header = TRUE, sep =";"  )
  
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
  
  ## subset data considered from 01/02/2007 to 02/02/2007
  subsetdata <- subset(powerdata, Date == "2007-02-01" | Date =="2007-02-02")
  
  ## plot histogram for global active power in kilowatts for the selected days
  hist(subsetdata$Global_active_power, col="red", border="black", main ="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency")
  
  ## Save the histogram in png format with a specified dimensions
  png("plot1.png", width=480, height=480)
  dev.off()
}

