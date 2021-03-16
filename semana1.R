setwd("C:/Users/Mafer/Documents/R-curso4")

###############BASE PLOTTING
library(datasets)
data(cars)
with(cars, plot(speed, dist))

##################LATTICE PLOT
library(lattice)
state = data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

##################GGPLOT2
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

################################################################################
#BASE PLOTTING
library(datasets)

#histograma
hist(airquality$Ozone)

#diagrama de dispersión
with(airquality, plot(Wind, Ozone))

#Boxplot
airquality = transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

#par funcion
#es usada para especificar parametros graficos globales

par("lty")
par("col")
par("pch")
par("bg")
par("mar")
par("mfrow")

#BASE PLOT WITH ANNOTATION
library(datasets)
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York") ##Add a title

#OPCION 2
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

#Opcion 3
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York", type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

#BASE PLOT WITH REGRESSION LINE
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York", pch = 20))
model = lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

###MULTIPLE BASE PLOTS
#OPCION 1
par(mfrow = c(1, 2))
with(airquality,{plot(Wind, Ozone, main = "Ozone and Wind ") 
     plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")})

#OPCION 2
par(mfrow = c(1, 3), mar = c(4,4,2,1), oma = c(0,0,2,0))
with(airquality,{plot(Wind, Ozone, main = "Ozone and Wind ") 
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temperature")
  mtext("Ozone and Weather in New York City", outer = TRUE)})

#Base Plotting Demostration
x = rnorm(100)
hist(x)
y = rnorm(100)
z = rnorm(100)
plot(x, z)
plot(x,y)
par(mar = c(4,4,2,2))
plot(x, y)
plot(x, y, pch = 20)
plot(x, y, pch = 19)
#example(points) #para saber que tipos de simbolos hay para graficar. 
title("Scatterplot")
text(-2, -2, "label")
legend("topleft", legend = "Data")
legend("topleft", legend = "Data", pch = 20)
fit = lm(y  ~ x)
abline(fit)
abline(fit, lwd = 3)
abline(fit, lwd = 3, col = "blue")
plot(x, y , lwd = 3, xlab = "Weight", ylab = "Height", 
       main = "Scatterplot", pch = 20)
legend("topright", legend = "Data", pch = 20)
fit = lm(y  ~ x)
abline(fit, lwd = 3, col = "red")
z = rpois(100, 2)
par(mfrow = c(2,1))
plot(x, y, pch = 20)
plot(x, z, pch = 19)
par("mar")

par(mar = c(2,2,1,1))
plot(x, y, pch = 20)
plot(x, z, pch = 20)

par(mfrow = c(1,2))
par(mar = c(4,4,2,2))
plot(x, y, pch = 20)
plot(x, z, pch = 20)

par(mfrow = c(2,2))
plot(x, y, pch = 20)
plot(x, z, pch = 20)
plot(z, x, pch = 20)
plot(y, x, pch = 20)

par(mfcol = c(2,2))
plot(x, y, pch = 20)
plot(x, z, pch = 20)
plot(z, x, pch = 20)
plot(y, x, pch = 20)

par(mfrow = c(1,1))
x = rnorm(100)
y = x + rnorm(100)
g = gl(2, 50)
g = gl(2, 50, labels = c("Male", "Female"))
plot(x, y, type = "n")
points(x[g == "Male"], y[g == "Male"], col = "green")
points(x[g == "Female"], y[g == "Female"], col = "blue", pch = 19)

#####################################################################
#Graphics Devices in R
library(datasets)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")

#PDF
pdf(file = "myplot.pdf") #crea un archivo pdf
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.off() #cierra el archivo pdf y ya con el plot

#####Copying Plots
library(datasets)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.copy(png, file = "geyserplot.png") #copia la grafica en un archivo png
dev.off() #cerrar el dispositivo png

