################################# SEMANA 2 #####################################
setwd("C:/Users/Mafer/Documents/R-curso4")

###lattice plotting
library(lattice)

################## funcion xyplot
library(datasets)

xyplot(Ozone ~ Wind, data =airquality)

##Simple Lattice Plot
airquality = transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data =airquality, layout=c(5,1))

#
p = xyplot(Ozone ~ Wind, data =airquality)
print(p) #aparece la grafica 

xyplot(Ozone ~ Wind, data =airquality)

#Funcion panel
#opcion 1
set.seed(10)
x = rnorm(100)
f = rep(0:1, each = 50)
y = x + f - f*x +rnorm(100, sd = 0.5)
f = factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2,1)) #grafica dos paneles

#opcion 2
xyplot(y ~ x | f, panel = function(x,y, ...){
  panel.xyplot(x, y, ...) 
  panel.abline(h = median(y), lty = 2) 
  })

#opciona 3
xyplot(y ~ x | f, panel = function(x,y, ...){
  panel.xyplot(x, y, ...) 
  panel.lmline(x, y, col=2) 
})

### ggplot2
library(ggplot2)
library(ggplot)
str(mpg)

#grafica simple
qplot(displ, hwy, data = mpg)

#color mapeado por la variable drv
qplot(displ, hwy, data = mpg, color = drv)

#agregando un geom
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

#histogramas
qplot(hwy, data = mpg, fill= drv)

#Facets (ventanas)
#columnas
qplot(displ, hwy, data = mpg, facets = .~ drv)
#filas
qplot(hwy, data = mpg, facets = drv~., binwidth = 2)

#cargar base de datos maacs
load("maacs.Rda")
maacs = na.omit(maacs)
str(maacs)
#histograma general
qplot(log(eno), data = maacs)
#histograma por grupo
qplot(log(eno), data = maacs, fill= mopos)

#Density Smooth (densidad suave)
qplot(log(eno), data = maacs, geom = "density")

qplot(log(eno), data = maacs, geom = "density", color=mopos)

#graficas de dispersion (Scatterplots)
#ENO VS. 25PM
#opcion 1
qplot(log(pm25), log(eno), data = maacs)
#opcion 2
qplot(log(pm25), log(eno), data = maacs, shape = mopos)
#opcion 3
qplot(log(pm25), log(eno), data = maacs, color=mopos)

#en un mismo panel
qplot(log(pm25), log(eno), data = maacs, color=mopos) + geom_smooth(method = "lm")
#separado en paneles
qplot(log(pm25), log(eno), data = maacs, facets =.~mopos) + geom_smooth(method = "lm")

#basic plot
qplot(logpm25, NocturnalSympt, data =maacs, facets = . ~ bmicat, 
      geom = c("point", "smooth"), method = "lm")

###con ggplot
g = ggplot(maacs, aes(log(pm25), log(eno)))
g + geom_point() +geom_smooth()

g + geom_point() + geom_smooth(method = "lm")

#agregando mas capas: mas de dos graficas en un mismo panel
g + geom_point() + facet_grid(.~mopos) + geom_smooth(method = "lm")

#modificando la estetica (color de los puntos)
#con alpha los puntos pueden ser transparentes
#asignar color con un valor constante
g + geom_point(color= "steelblue", size=4, alpha=1/2)
#asignar color con el valor de una variable
g + geom_point(aes(color= mopos), size =4, alpha=1/2)

#modificando etiquetas
g + geom_point(aes(color= mopos), size =4, alpha=1/2) + labs(title = "MAACS") +
  labs(x = expression("log"*PM[2.5]), y = "log eno")

#modificando la suavidad
g + geom_point(aes(color= mopos), size =2, alpha=1/2) + geom_smooth(size=4, 
                        linetype= 3, method = "lm", se=FALSE)
#modificando el tema
g + geom_point(aes(color= mopos), size =2, alpha=1/2) + theme_bw(base_family = "Times")

# una nota sobre los ejes limites
testdata = data.frame(x=1:100, y=rnorm(100))
testdata[50, 2] = 100 #Outlier!
plot(testdata$x, testdata$y, type="l", ylim = c(-3,3))

#
g = ggplot(testdata, aes(x = x,y=y))
g + geom_line()

#limite de los ejes
#outlier no tomado en cuenta
g + geom_line() + ylim(-3, 3)
#outlier incluido
g + geom_line() + coord_cartesian(ylim=c(-3,3))

#multiples boxplot en un mismo plot
qplot(drv,hwy,data=mpg,geom="boxplot", color= manufacturer)

#g + geom_point() + facet_grid(drv~cyl, margins = TRUE)

#qplot(price,data=diamonds,binwidth=18497/30,fill=cut)

#qplot(carat, price, data=diamonds, color=cut, facets = .~cut) + 
#geom_smooth(method = "lm")
