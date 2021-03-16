setwd("C:/Users/Mafer/Documents/R-curso4")
#Datos cargados
data = read.table("household_power_consumption.txt", header = TRUE, sep= ";",
                  na.strings = "?")



#Formato a la columna Date
data$Date = as.Date(data$Date, "%d/%m/%Y")

#trabajar solo con 1 y 2 de febrero de 2007
conjunto = subset(data, Date  >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

#Combinar Date y time
date_time = paste(conjunto$Date, conjunto$Time)

#Darle nombre a la variable
date_time = setNames(date_time, "DateTime")

## eliminando las columnas de Date and Time 
conjunto = conjunto[ ,!(names(conjunto) %in% c("Date","Time"))]

#Agregando dateTime
conjunto = cbind(date_time, conjunto)

#formato
conjunto$date_time = as.POSIXct(date_time)

#PLOT4
par(mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0))
with(conjunto, {
  plot(Global_active_power~date_time, type= "l",
                     xlab="",ylab = "Global Active Power (kilowatts)")
  plot(Voltage~date_time, type="l", xlab = "datetime", 
       ylab = "Voltage")
  
  plot(Sub_metering_1~date_time, type= "l", xlab="", 
                       ylab = "Energy sub metering")
  lines(Sub_metering_2 ~date_time, col= "red")
  lines(Sub_metering_3~date_time, col ="blue")
  legend("topright",col = c("black", "red", "blue"), lty=1, lwd=1, bty="n", cex = 0.6,
  legend=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~date_time, type="l", ylab="Global_reactive_power",
       xlab="datetime")
  })


dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()


