require(xlsx)
require(tidyr)
require(dplyr)
require(ggplot2)
require(knitr)

setwd("C:/Github/MScDataMiningUBA2015/AID/data")

#2

chalets<-read.xlsx("chalets.xlsx",1)

#a
#valor atipico en duracion de hipoteca
boxplot(chalets[,2:4])

#b correlaciones positivas entre todas las variables
pairs(chalets[,2:4])


#c
colMeans(chalets[,2:4])
cov(chalets[,2:4])

#d 
cor(chalets[,2:4])

#e Si, dada la alta correlacion entre las variables

#f 

barplot(eigen(cor(chalets[,2:4]))$vectors[,1])

#g

eigen(cor(chalets[,2:4]))$values[[1]]/sum(eigen(cor(chalets[,2:4]))$values)

#h - Es una componente de tamaño

#i - Es posible, la 

chalets$pca1<-as.matrix(chalets[,2:4]) %*% eigen(cor(chalets[,2:4]))$vectors[,1]

chalets[chalets$pca1==min(chalets$pca1),]

chalets[chalets$pca1==max(chalets$pca1),]


#3


#factominer
