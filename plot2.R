plot2 <- function()
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


	##	Update the data types for the columns used in this plot and create new
	##	column named DateTime based on Date and Time columns
	dfConsumption[, "Date"] <- as.Date(dfConsumption[, "Date"], "%d/%m/%Y")
	dfConsumption[, "Global_active_power"] <- as.numeric(dfConsumption[, "Global_active_power"])
	dfConsumption[, "DateTime"] <- paste(dfConsumption[, "Date"], dfConsumption[, "Time"], sep = " ")
	dfConsumption[, "DateTime"] <- as.POSIXct(dfConsumption[, "DateTime"])


	##	Determine the range for the x- and y-axes
	xrange <- range(dfConsumption[, "DateTime"])
	yrange <- range(dfConsumption[, "Global_active_power"])


	##	Open the destination file and create a plot with no data, only axes
	png("plot2.png", width = 480, height = 480,units = "px")
	plot(xrange, yrange, type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")


	##	Add the line for Global Active Power over the Date/Time range and close the file
	lines(dfConsumption[, "DateTime"], dfConsumption[, "Global_active_power"])
	dev.off()
}
