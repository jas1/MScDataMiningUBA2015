#AID - Trabajo Práctico Nro. I

setwd("C:/Github/MScDataMiningUBA2015/AID")

require(xlsx)
require(tidyr)
require(dplyr)
require(ggplot2)
require(knitr)
require(GGally)

#Ejercicio 1: Transformaciones de datos

#Trabajo previo sobre el file recepcionistas: Escribir un nombre a ultimo campo, sino tira error.

recepcionistas<-read.xlsx("recepcionistas.xlsx",1)

df <- recepcionistas %>%
  rename(cord.juez1=cord.juez.1, cord.juez2=cord.juez.2) %>%
  gather(atributo, puntaje, -candidatos) %>%
  separate(atributo, into = c("atributo", "juez"), sep = "\\.")

kable(df)


#a) Calcule el promedio por juez de cada una de las aspirantes

a<- df %>%
  group_by(candidatos,juez)%>%
  summarise(promedio=mean(puntaje))
kable(a)


#b) Calcule el promedio de cada una de las aspirantes tomando en cuenta todos los rubros y ambos jueces


b<- df %>%
  group_by(candidatos) %>%
  summarise(promedio=mean(puntaje))
kable(b)


#c) Transformar las puntuaciones observadas de modo tal que cada una seis variables tenga media 0 y dispersión 1.

#Esto permite observar variación entre las candidatas dentro de un mismo atributo y un mismo juez.

c<- df %>%
  group_by(juez,atributo) %>%
  mutate(est=(puntaje-mean(puntaje))/sd(puntaje)) %>%
  ungroup()
kable(c)


#d) Transformar las puntuaciones de modo tal que cada candidata tenga para cada juez media 0 y dispersión 1.

#Esto permite observar la variación entre los atributos de una misma candidata y un mismo juez


d<-df %>%
  group_by(juez,candidatos) %>%
  mutate(est=(puntaje-mean(puntaje))/sd(puntaje))%>%
  ungroup()
kable(d)


#e) Grafique los perfiles multivariados de cada una de las candidatas para ambas transformaciones.

#1) Variación entre las candidatas dentro de un mismo juez y un mismo atributo. Probamos con dos visualizaciones, la primera permite comparar mejor las diferencias entre jueces, la segunda, permite ver el perfil de atributos de cada candidata.


e1<-ggplot(c, aes(x=juez, y=est, group=candidatos, color=candidatos))+
  geom_point(size=5)+
  geom_line()+
  facet_grid(.~atributo)

print(e1)



e2<-ggplot(c, aes(x=atributo, y=est, group=candidatos, color=candidatos))+
  geom_point(size=5)+
  geom_line()+
  facet_grid(.~juez)

print(e2)


# Conclusiones: Si intentamos hacer un ranking de candidatas para cada atributo: En el atributo coordinación, ambos jueces establecen a Carla como la mejor y Alejandra como la más floja. 
# En idiomas hay diferencias entre ambos jueces, ambos consideran a Mariana como la mejor, Daniela se mantiene en último lugar, con Sabrina desde cerca. 
# En presencia, Daniela está en el último lugar para ambos jueces, Maia y Carla en el primero.  
# Si hay alguna preferencia de los jueces por alguna candidata, podemos decir que el juez dos prefiere más a Alejandra que el juez 1. Mientras que el juez uno prefiere más a Mariana que el juez 2. 


#2) Variación entre los atributos de una misma candidata y un mismo juez. La primera visualización permite ver las diferencias entre ambos jueces, la segunda permite ver cómo evaluló cada juez en general con respecto a cada atributo.

e3<-ggplot(d, aes(x=juez, y=est, group=atributo, color=atributo))+
  geom_line()+
  geom_point(size=5)+
  facet_grid(.~candidatos)

print(e3)


e4<-ggplot(d, aes(x=candidatos, y=est, group=atributo, color=atributo))+
  geom_line()+
  geom_point(size=5)+
  facet_grid(.~juez)

print(e4)


#En los únicos casos que se observa una disidencia notable entre ambos jueces en cuanto a las aptitudes de una misma candidata es en Alejandra y Mariana, el juez 2 en ambos casos ve mejor el idioma que el juez 1 pero peor la coordinación. Este patrón se replica en casi todos los casos.
#El juez 1 tiene a ser más exigente en idiomas que en coordinación, y el juez 2 suele puntuar mejor en presencia que en los otros atributos.


#Ejercicio 2: Conjunto de datos

internet<-read.xlsx("Internet2013.xlsx",1)

#a) Clasificar las variables de la base.

internet2<- internet %>% transform(ID=as.factor(ID),
                                   Nacionalidad=as.factor(Nacionalidad),
                                   Sexo=as.factor(Sexo),
                                   Sitio=as.factor(Sitio))

#b) Construir la tabla de frecuencia para la variable sexo. Hay algún valor que llame la atención? ¿Qué tipo de error considera que es?

table(internet2$Sexo)

#Hay un valor igual a 0, puede ser faltante o no contesta.

internet2$Sexo[!internet2$Sexo %in% c(1,2)]<-NA

internet2$Sexo<-droplevels(internet2$Sexo)

levels(internet$Sexo)<-c("masculino","femenino")

#Error de tipeo.

#c) Ordenar los datos por la variable Edad. Encontró algún valor extraño?. Qué tipo de error puede ser?

internet2<-internet2 %>% arrange(Edad)

sort(unique(internet2$Edad))

internet2$Edad[internet2$Edad<5 | internet2$Edad>100]<-NA

#Error de tipeo.

#d,f) Construir la tabla de frecuencias de la variable Sitio. Encuentra algún valor que le llame la atención. ¿Qué tipo de error puede ser?

table(internet2$Sitio, exclude=NULL)

internet2$Sitio[!internet2$Sitio %in% seq(1,8)]<-NA

internet2$Sitio<-droplevels(internet2$Sitio)

#e,f) Proceda de forma similar para las variables Temperatura, Autos y Cigarrillos.

table(internet2$Temperatura)

#Temperaturas mayores de 50 error.
internet2$Temperatura[internet2$Temperatura>50]<-NA

table(internet2$Cigarrillos)

#Outlier 2680 autos
table(internet2$Autos)

#g) Para cuales de las variables tiene sentido calcular la media? Y la mediana?
# Para las numéricas.

#h) En el caso de los autos, por tener un outlier muy severo, 
# cualquier valor de alfa que elimine el outlier seria suficiente.

mean(internet2$Autos)
mean(internet2$Autos,trim=0.05, na.rm=TRUE)
mean(internet2$Autos,trim=0.1, na.rm=TRUE)

#Pasar del 0.05 al 0.1 no varía casi nada la media.

#i) ¿Cuáles de las variables le parecen simétricas a partir de estos resúmenes?. Confirme estas observaciones mediante un boxplot.

boxplot(internet2$Autos)
boxplot(internet2$Autos[internet2$Autos<1000])

boxplot(internet2$Cigarrillos)
boxplot(internet2$Cigarrillos[internet2$Cigarrillos<50])

boxplot(internet2$Temperatura)

#j) Calcular la desviación intercuartil y detectar presencia de valores salvajes moderados y severos.

iqroutliers<- 
function(data,variable){
  primcuartil<-quantile(data[[variable]],0.25)[[1]]
  tercuartil<-quantile(data[[variable]],0.75)[[1]]
  iqr<-tercuartil-primcuartil
  
  #Outliers Moderados
  outmod1<-data[data[[variable]]<primcuartil-1.5*iqr & data[[variable]]>=(primcuartil-3*iqr),]
  outmod2<-data[data[[variable]]>(tercuartil+1.5*iqr) & data[[variable]]<=(tercuartil+3*iqr),]
  
  #Outliers Severos
  outsev<-data[data[[variable]]>(tercuartilcig+3*iqrcig),]
  
  return(list(outmod1,outmod2,outsev))
}

l<-iqroutliers(internet2,"Cigarrillos")

#Ejercicio 3

setwd("C:/Github/MScDataMiningUBA2015/AID/data")
gorriones<-read.xlsx("Gorriones.xlsx",1)

#a) Indicar en cada caso de que tipo de variable se trata.

#Todas numéricas menos sobrevida vivo

gorriones$sobrevive<-as.factor(gorriones$sobrevive)

#b) Realizar una tabla de frecuencias y un resumen* para cada una de ellas

sum(is.na(gorriones[[2]]))
summary(gorriones[[2]])
sd(gorriones[[2]])

sum(is.na(gorriones[[3]]))
summary(gorriones[[3]])
sd(gorriones[[3]])
        
#c) Realizar en el caso que corresponda un histograma. Ensayar el número de intervalos que conviene en cada variable.
hist(gorriones[[2]],breaks=5,freq=TRUE)
hist(gorriones[[3]],breaks=5,freq=TRUE)
hist(gorriones[[4]],breaks=5,freq=TRUE)
hist(gorriones[[5]],breaks=5,freq=TRUE)
hist(gorriones[[6]],breaks=5,freq=TRUE)

#d) Realizar un boxplot comparativo para cada una de estas variables particionando por el grupo definido por la supervivencia. Le parece que alguna de estas variables está relacionada con la supervivencia, es decir que toma valores muy distintos en ambos grupos. Analizar en todos los casos la presencia de outliers.
boxplot(gorriones[[2]]~gorriones[[7]])
boxplot(gorriones[[3]]~gorriones[[7]])
boxplot(gorriones[[4]]~gorriones[[7]])
boxplot(gorriones[[5]]~gorriones[[7]])
boxplot(gorriones[[6]]~gorriones[[7]])

#e) Construir gráficos bivariados para las todas las variables, particionando por el grupo de supervivencia. (un color para cada grupo). Observa alguna regularidad que pueda explicar la supervivencia.
#f) Calcular la matriz de varianzas y covarianzas muestrales y la matriz de correlación muestral: para el grupo general y para los subgrupos definidos por la sobrevida. Comparar estas matrices con la del grupo general y entre sí.

pairs(gorriones[,2:7],col=gorriones[,7])
ggpairs(gorriones[,2:7],colour=colnames(gorriones)[7])

#Todas las variables muestran mayor variabilidad para el subset que no sobrevivio. También muestran mayor correlación positiva.
#Esto puede ser causado porque estar fuera de la norma, muy grandes y muy chicos puede ser perjudicial.

