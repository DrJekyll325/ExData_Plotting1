plot1 <- function()
{
	##	Get the file path and file name for the text file provided
	filePath <- ("C:/Users/DrJekyll325/Documents/R/Exploratory Data Analysis/Course Project 1")
	fileName <- "household_power_consumption.txt"
	setwd(filePath)


	##	Use the data.table library to read the power consumption data from the
	##	text file in a data table named dtConsumption
	library(data.table)
	dtConsumption <- fread(input = fileName, sep = ";", colClasses = "character",
							stringsAsFactors = FALSE)


	##	Filter the data table to only observations from the 1st and 2nd of
	##	February, 2007.  Note that the dates in the file are in d/m/yyyy format.
	dfConsumption <- data.frame(dtConsumption[Date %like% "^[12]/2/2007"])
	rm(dtConsumption)


	##	Update the data type for the column used in this plot
	dfConsumption[, "Global_active_power"] <- as.numeric(dfConsumption[, "Global_active_power"])


	##	Create a histogram and write it to plot1.png
	png("plot1.png", width = 480, height = 480,units = "px")
	hist(as.numeric(dfConsumption$Global_active_power), col = "red", breaks = 15, ylim = c(0, 1200),
		main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
	dev.off()
}
