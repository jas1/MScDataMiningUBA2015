require(xlsx)
require(tidyr)
require(dplyr)
require(ggplot2)
require(knitr)

setwd("C:/Github/MScDataMiningUBA2015/AID/data")

gorriones<-read.xlsx("Gorriones.xlsx",1)

#a
nrow(gorriones)
ncol(gorriones)

#b
