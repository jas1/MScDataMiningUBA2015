setwd("C:/Users/OLX - Franco/Dropbox/Data Science/Maestria/AID/clase_AID1104")

require(xlsx)
require(tidyr)
require(dplyr)
require(ggplot2)

recepcionistas<-read.xlsx("recepcionistas.xls",1)

df <- recepcionistas %>%
  rename(cord.juez1=cord.juez.1, cord.juez2=cord.juez.2) %>%
  gather(atributo, puntaje, -candidatos) %>%
  separate(atributo, into = c("atributo", "juez"), sep = "\\.") 


#Calcule el promedio por juez de cada una de las aspirantes
ej1<- df %>%
  group_by(candidatos,juez)%>%
  summarise(promedio=mean(puntaje))

#Calcule el promedio de cada una de las aspirantes tomando en cuenta todos los rubros y ambos jueces
ej2<- df %>%
  group_by(candidatos) %>%
  summarise(promedio=mean(puntaje))

#Transformar las puntuaciones observadas de modo tal que cada una seis variables tenga media 0 y dispersión 1. 
#Esto permite observar variación entre las candidatas dentro de un mismo atributo y un mismo juez
ej3<- df %>%
  group_by(juez,atributo) %>%
  mutate(est=scale(puntaje,center=TRUE,scale=TRUE))


#Transformar las puntuaciones de modo tal que cada candidata tenga para cada juez media 0 y dispersión 1.
#Esto permite observar la variación entre los atributos de una misma candidata y un mismo juez
ej4<- df %>%
  group_by(juez,candidatos) %>%
  mutate(est=scale(puntaje,center=TRUE,scale=TRUE))

#Grafique los perfiles multivariados de cada una de las candidatas para ambas transformaciones.
#

ej5<-ggplot(ej3, aes(x=juez, y=est, group=candidatos, color=candidatos))+
  geom_point(size=5)+
  geom_line()+
  facet_grid(.~atributo)

print(ej5)

ej6<-ggplot(ej4, aes(x=juez, y=est, group=atributo, color=atributo))+
  geom_line()+
  geom_point(size=5)+
  facet_grid(.~candidatos)

print(ej6)
