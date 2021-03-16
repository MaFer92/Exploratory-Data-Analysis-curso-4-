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

#PLOT3
with(conjunto, {plot(conjunto$Sub_metering_1~conjunto$date_time, type= "l", xlab="", 
     ylab = "Energy sub metering")
lines(conjunto$Sub_metering_2 ~conjunto$date_time, col= "red")
lines(conjunto$Sub_metering_3~conjunto$date_time, col ="blue")
legend("topright",col = c("black", "red", "blue"), lwd =c(1,1,1),
       legend=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"))})


#Guardar en png
dev.copy(png, file = "plot3.png",  width = 480, height = 480)
dev.off()
