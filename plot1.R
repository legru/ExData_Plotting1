
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

# data.all stores all data that is needed for the homework
data.all <- relevant.data(data.raw,date.start,date.end)

## ------- PLOT1 ------------------------------

# draw the plot on a screen device
plot1.draw <- function () {
  hist(data.all$Global_active_power,
       freq=TRUE, 
       col="red",
       main = "Global Active Power",
       xlab = "Global Active Power (kilowatt)")
}

# Plot1 creates a PNG file with Plot1 as requested by the homework
plot1 <- function() {
  # open the device
  png(filename = "plot1.png",width = 480,height = 480)
  # plot the graph on the device
  plot1.draw()
  # cloose the device
  dev.off()
}

plot1()
