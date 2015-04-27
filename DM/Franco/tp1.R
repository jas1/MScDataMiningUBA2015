require(xlsx)
require(tidyr)
require(dplyr)
require(ggplot2)
require(knitr)

setwd("C:/Github/MScDataMiningUBA2015/DM")

credit<-read.xlsx("CreditCard_ori.xls",1)

credit2<-credit %>% mutate(interval=cut(INGRESO, breaks=3))

credit2 %>% group_by(interval,GOLD) %>% 
  summarise(absoluto=n()) %>% 
  mutate(prop=absoluto/sum(absoluto)) %>% 
  ungroup() %>% 
  arrange(GOLD)

sharks<- read.xlsx("plainsharks.xlsx",1)

sharks %>% filter(grepl("FATAL",Injury))


