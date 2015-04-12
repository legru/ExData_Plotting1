
## -------- SETTING ---------------------------

# The start date of the date to analyze
date.start <- strptime("2007-02-01","%Y-%m-%d")

# The end date of the date to analyze 
date.end <- strptime("2007-02-02","%Y-%m-%d")

#filename
file.name <- "household_power_consumption.txt" 


## -------- Load data --------------------------
#read the ddata
data.raw <- read.csv(file = file.name, sep = ";", na.strings="?", skipNul = TRUE) 
 
 
## --------- CLEAN DATA -----------------------

# clean date field to make transform it in date format
clean.date <- function(data) {
  data.date <- data$Date
  data.date.clean <- strptime(data.date,"%d/%m/%Y")
  data$Date <- data.date.clean
  data
} 




# extract the relevant subset
# ideally the following should work, but it does not.  Can some of you tell me why?
# > data.all <- subset(data.clean_date, (Date >= date.start & Date >= date.end))

# Alternative
relevant.data <- function(data, start, end) {
  # uniform treatment of date
  data.clean_date <- clean.date(data)
  # extract data dated before end
  data.before_end <- subset(data.clean_date, Date <= end)
  # remove data before start  
  data.relevant <- subset(data.before.end, Date >= start)
  # return relevant data
}

# merge date and time so that all points are correctly distributed in time
date_time.data <- function(data) {
  # join data and time
  date_time <- paste(data.all$Date,data$Time)
  # format date and time correctly
  date.combined <- strptime(date_time,"%Y-%m-%d %H:%M:%S")
  # merge back in the data frame
  data$date.combined <- date.combined
  # return resulting data frame
  data
}

# data.all stores all data that is needed for the homework
data.all <- relevant.data(data.raw,date.start,date.end)

## ------- Plotting Functions -----------------

# generic plotting function for the homework
# data.plot: the data to plot
# var.x: the x variable to plot
# var.y: the y variable to plot
# x.lab="": the xlab to use
# y.lab="": the ylab to use
#
plot.draw <- function(data.plot, var.x, var.y, x.lab="", y.lab="") {
  plot(data.plot[[var.x]],
       data.plot[[var.y]],
       type="l",
       ylab = y.lab,
       xlab = x.lab)
}


# plot to a png file device
# file.name: the name of the file to print
# data.plot: the data to plot
# var.x: the x variable to plot
# var.y: the y variable to plot
# x.lab="": the xlab to use
# y.lab="": the ylab to use
#
plot.to.file <- function (file.name, data.plot, var.x, var.y, x.lab="", y.lab="", plot.fun="plot.draw") {
  # open the device
  png(filename = file.name,width = 480,height = 480)
  # instantiate the plotting function
  fun.plot <- match.fun(plot.fun)
  # apply the plotting function: plot the graph on the device
  fun.plot(data.plot, var.x, var.y, x.lab, y.lab)
  # cloose the device
  dev.off()
} 

## ------- PLOT2 ------------------------------

plot2 <- function () {
  plot.to.file("plot2.png", 
               date_time.data(data.all), 
               "date.combined", "Global_active_power",
               y.lab="Global Active Power (kilowatt)")
}

plot2()


plot3.draw <- function() {
  date_time <- paste(data.all$Date,data.all$Time)
  date.combined <- strptime(date_time,"%Y-%m-%d %H:%M:%S")
  data.all$date.combined <- date.combined
  plot(data.all$date.combined,
       data.all$Sub_metering_1,
       type="l",
       ylab = "Energy sub metering",
       xlab="")
  lines(data.all$date.combined,
       data.all$Sub_metering_2,
       col= "red")
  lines(data.all$date.combined,
       data.all$Sub_metering_3,
       col="blue")
}

plot3 <- function () {
  # open the device
  png(filename = "Plot3.png")
  # plot the graph on the device
  plot3.draw()
  # cloose the device
  dev.off()
}

#plot3()


plot4.draw <- function() {
  date_time <- paste(data.all$Date,data.all$Time)
  date.combined <- strptime(date_time,"%Y-%m-%d %H:%M:%S")
  data.all$date.combined <- date.combined
  par(mfrow=c(2,2))
  plot2.draw()
  plot(data.all$date.combined,
       data.all$Voltage,
       type="l",
       ylab = "Voltage",
       xlab="")
  plot3.draw()
  plot(data.all$date.combined,
       data.all$Global_reactive_power,
       type="l",
       ylab = "Global reactive power",
       xlab="")  
}

plot4 <- function () {
  # open the device
  png(filename = "Plot4.png")
  # plot the graph on the device
  plot4.draw()
  # cloose the device
  dev.off()
}

#plot4()