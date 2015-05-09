require(xlsx)
require(tidyr)
require(dplyr)
require(ggplot2)
require(knitr)

setwd("C:/Github/MScDataMiningUBA2015/AID/data")

gorriones<-read.xlsx("Gorriones.xlsx",1)

####1
#a
nrow(gorriones)
ncol(gorriones)

#b
colMeans(gorriones[,2:ncol(gorriones)])
cov(gorriones[,2:ncol(gorriones)])
cor(gorriones[,2:ncol(gorriones)])

#c - La varianza del largo total

#d - La covarianza del largo total y el largo del pico y cabeza.

#e - La correlación consigo mismo de la extensión alar y 


#2
recepcionistas<-read.xlsx("recepcionistas.xlsx",1)

#a-Promedios de calificaciones para cada atributo y cada juez
colMeans(recepcionistas[,2:7])

#b
cov(recepcionistas[,2:4])
cor(recepcionistas[,2:4])

cov(recepcionistas[,5:7])
cor(recepcionistas[,5:7])

cov(recepcionistas[,2:7])
cor(recepcionistas[,2:7])

#c - Faltan las relaciones entre variables de distintos subgrupos.

#d - Las trazas si son iguales porque son las covarianzas con respecto a una misma variable.





#