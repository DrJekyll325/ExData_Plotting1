plot3 <- function()
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
	dfConsumption[, "Sub_metering_1"] <- as.numeric(dfConsumption[, "Sub_metering_1"])
	dfConsumption[, "Sub_metering_2"] <- as.numeric(dfConsumption[, "Sub_metering_2"])
	dfConsumption[, "Sub_metering_3"] <- as.numeric(dfConsumption[, "Sub_metering_3"])
	dfConsumption[, "DateTime"] <- paste(dfConsumption[, "Date"], dfConsumption[, "Time"], sep = " ")
	dfConsumption[, "DateTime"] <- as.POSIXct(dfConsumption[, "DateTime"])


	##	Determine the range for the x- and y-axes
	xrange <- range(dfConsumption[, "DateTime"])
	yrange <- range(c(dfConsumption[, "Sub_metering_1"], dfConsumption[, "Sub_metering_2"], dfConsumption[, "Sub_metering_3"]))


	##	Open the destination file and create a plot with no data, only axes and the legend
	png("plot3.png", width = 480, height = 480,units = "px")
	plot(xrange, yrange, type = "n", xlab = "", ylab = "Energy sub metering")
	legend('topright', lty = 1, c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), cex = 0.95)


	##	Add the three lines for Energy Sub Metering over the Date/Time range and close the file
	lines(dfConsumption[, "DateTime"], dfConsumption[, "Sub_metering_1"], col = "black")
	lines(dfConsumption[, "DateTime"], dfConsumption[, "Sub_metering_2"], col = "red")
	lines(dfConsumption[, "DateTime"], dfConsumption[, "Sub_metering_3"], col = "blue")
	dev.off()
}
