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

#c - La varianza del largo total/La covarianza del largo total y el largo del pico y cabeza.

#d - La correlación consigo mismo de la extensión alar y la covarianza del largo total y el largo del pico y cabeza. 

#e - m12=m21/sqr(m11)*sqr(m22)

#f
gorriones2<-gorriones
gorriones2$diflargos<-gorriones[[2]]-gorriones[[5]]

#g - La media de la nueva variable dif largos es la diferencia entre las medias del largo total y el largo del humero.
# En el vector de medias vemos la escala de las distintas variables, quiza tendriamos que normalizar.
colMeans(gorriones2[,2:ncol(gorriones)])

#h - Ambas trazas se aumentan, la de correlciones en 1 x tener una variable mas, la de covarianzas en la varianza de la nueva variable.
sum(diag(cov(gorriones[,2:ncol(gorriones)])))
sum(diag(cov(gorriones2[,2:ncol(gorriones2)])))

sum(diag(cor(gorriones[,2:ncol(gorriones)])))
sum(diag(cor(gorriones2[,2:ncol(gorriones2)])))




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


