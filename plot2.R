library(data.table)

## ...........................................................
# Load the data
file.path= "C:/Users/Massimo/OneDrive/Documents/GitHub/Coursera/DataScience/Exploratory Analysis/Project1"
data.dir= "exdata-data-household_power_consumption"
file.name= "household_power_consumption.txt"
data.raw= fread(input = paste(file.path,data.dir,file.name,sep="/"),
                header = TRUE, 
                na.strings=c("NA","N/A","","?") )
setkey(dt.raw,Date)
data= data.raw[Date %in% c('1/2/2007', '2/2/2007')]  # select relevant data
# remove data.raw which is  not needed
data.raw=  NULL

## ...........................................................
#  Prepare the  data table for  plotting

#  define a timestamp for all data points
# NOTE: The following is a  hack  to transform POSIXlt to POSIXct
#  I  am sure it  can be done better than this,  but  I have no time to look  for  a solution now
ts= data.table(timestamp= strptime(paste(data[,Date], data[,Time]),"%d/%m/%Y %H:%M:%S"))
data[,timestamp:= ts[,timestamp]]
# fix the rype of the columns
data$Date= as.Date(Date)
data$Global_active_power=    as.numeric(data$Global_active_power)
data$Global_reactive_power=  as.numeric(data$Global_reactive_power)
data$Voltage=                as.numeric(data$Voltage)
data$Sub_metering_1=         as.numeric(data$Sub_metering_1)
data$Sub_metering_2=         as.numeric(data$Sub_metering_2)
data$Sub_metering_3=         as.numeric(data$Sub_metering_3)

## ...........................................................
# PLOT2

# open PNG device
png(paste(file.path,"Plot2.png",sep="/"),width = 480,height = 480)
#  Plot the graph
plot(data$Global_active_power~data$timestamp,type="l",
     xlab = "",
     ylab = "Global Active Power (kilowatts)")
# Close the device
dev.off()
