plot1 <- function()
{
	##	Get the file path and file name for the text file provided
	filePath <- ("C:/Users/DrJekyll325/Documents/R/Exploratory Data Analysis/Course Project 1/")
	fileName <- paste(filePath, "household_power_consumption.txt", sep = "")


	##	Use the data.table library to read the power consumption data from the
	##	text file in a data table named dtConsumption
	library(data.table)
	dtConsumption <- fread(input = fileName, sep = ";", colClasses = "character",
							stringsAsFactors = FALSE)


	##	Filter the data table to only observations from the 1st and 2nd of
	##	February, 2007.  Note that the dates in the file are in d/m/yyyy format.
	dfConsumption <- data.frame(dtConsumption[Date %like% "^[12]/2/2007"])
	rm(dtConsumption)


	##	Update data types and create new column named DateTime based on Date
	##	and Time columns
	dfConsumption[, "Date"] <- as.Date(dfConsumption[, "Date"], "%d/%m/%Y")
	dfConsumption[, "Global_active_power"] <- as.numeric(dfConsumption[, "Global_active_power"])
	dfConsumption[, "Global_reactive_power"] <- as.numeric(dfConsumption[, "Global_reactive_power"])
	dfConsumption[, "Voltage"] <- as.numeric(dfConsumption[, "Voltage"])
	dfConsumption[, "Global_intensity"] <- as.numeric(dfConsumption[, "Global_intensity"])
	dfConsumption[, "Sub_metering_1"] <- as.numeric(dfConsumption[, "Sub_metering_1"])
	dfConsumption[, "Sub_metering_2"] <- as.numeric(dfConsumption[, "Sub_metering_2"])
	dfConsumption[, "Sub_metering_3"] <- as.numeric(dfConsumption[, "Sub_metering_3"])
	dfConsumption[, "DateTime"] <- paste(dfConsumption[, "Date"], dfConsumption[, "Time"], sep = " ")
	dfConsumption[, "DateTime"] <- as.POSIXct(dfConsumption[, "DateTime"])


	##	Create a histogram
	hist(as.numeric(dfConsumption$Global_active_power), col = "red", breaks = 15, ylim = c(0, 1200),
		main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
}
