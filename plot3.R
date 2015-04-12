
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
  data.relevant <- subset(data.before_end, Date >= start)
  # return relevant data
  data.relevant
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

# generalize plot.draw  to the case of multiple lines
# use the same arguments of plot.draw with an additional one
# more.vars_col contains a matrix data-colum color to print that data colum
multi_plot.draw <- function(data.plot, var.x, var.y, x.lab="", y.lab="",more.vars_col=NULL) {
  # base plot
  plot.draw(data.plot, var.x, var.y, x.lab, y.lab)
  # plot remaing columns....
  # add_line is a line that plots a colum with a given color
  add_line <- function(pars) {
    # extract the colums
    data.label <- pars[1]
    data.col <- pars[2]
    # plot the line with the required color
    lines(data.plot[[var.x]],
          data.plot[[data.label]],
          col=data.col)
  }
  # if no matrix is passed,, than ignore the rest
  if (!is.null(more.vars_col)) {
    # a matrix is passed,, use the apply to loop through all lines
    apply(more.vars_col,1,add_line)
    # add the legend as required 
    legend("topright", lty=c(1,1,1),
           col=c("black","red","blue"),
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  }
}


# plot to a png file device
# file.name: the name of the file to print
# data.plot: the data to plot
# var.x: the x variable to plot
# var.y: the y variable to plot
# x.lab="": the xlab to use
# y.lab="": the ylab to use
#
plot.to.file <- function (file.name, data.plot, var.x, var.y, x.lab="", y.lab="", plot.fun="plot.draw",more.vars_col=NULL) {
  # open the device
  png(filename = file.name,width = 480,height = 480)
  # instantiate the plotting function
  fun.plot <- match.fun(plot.fun)
  # apply the plotting function: plot the graph on the device
  fun.plot(data.plot, var.x, var.y, x.lab, y.lab,more.vars_col)
  # cloose the device
  dev.off()
} 

## ------- PLOT3 ------------------------------

plot3 <- function () {
  # extra graphs
  more <- matrix(c("Sub_metering_2", "Sub_metering_3", 
                   "red", "blue"), 
                 nrow=2, nco=2)
  # draw with the extra lines
  plot.to.file("plot3.png", 
               date_time.data(data.all), 
               "date.combined", "Sub_metering_1",
               y.lab="Global Active Power (kilowatt)",
               plot.fun="multi_plot.draw",
               more.vars_col= more)
}

plot3()



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

