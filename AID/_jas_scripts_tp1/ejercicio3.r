
#para ejecutar el ejercicio 3
ejecutar3 <- function(){
  # pre requisitos
  
  # para eliminar error de java
  if (Sys.getenv("JAVA_HOME")!="")
    Sys.setenv(JAVA_HOME="")
  
  # carga librerias
  require(xlsx)
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  require(knitr)
  require(reshape2 )
  require(GGally )
  
  #Ejercicio 3: Base de datos: Gorriones.xls
  # setear WD 
  setwd('C:/Users/Julio Spairani/Dropbox/julio_box/educacion/maestria_explotacion_datos_uba/materias/cuat_1_aid/TP1/r_ws/')
  
  # levantar datos:
  gorriones <- levantarDatosParaPrueba()
  
  # enunciado A
  espacioTabs <- c('\t\t\t')
  cat('a- Indicar en cada caso de que tipo de variable se trata. \n')
  cat('\n')
  cat('Pájaro',espacioTabs,'ordinales: entero\n')
  cat('Largo total',espacioTabs,'cuantitativa continua: integer\n')
  cat('Extensión alar',espacioTabs,'cuantitativa continua: integer\n')
  cat('Largo del pico y cabeza',espacioTabs,'cuantitativa continua: float\n')
  cat('Largo del húmero',espacioTabs,'cuantitativa continua: float\n')
  cat('Largo de la quilla del esternón',espacioTabs,'cuantitativa continua: float\n')
  cat('Sobrevida vivo= 1, muerto= -1',espacioTabs,'categorica: booleano\n')
  

  
  # enunciado B
  cat('b- Realizar una tabla de frecuencias y un resumen* para cada una de ellas \n')
  cat('\n')
  filtroColumnas <- c('largoTotal','extAlar','largopicoCabeza','largoHumero','largoQuilla')
  filteredGorriones <- gorriones[,filtroColumnas]
  
  cantidadIntervalos <- 5
  # para cada columna menos id y  vivo
  for (coln in colnames(filteredGorriones) ) {
    #mostrar nombre columna
    cat("Var: ",coln,"\n")
    # frecuencias
    
    tabFreq <- armarTablaFrecuencias(filteredGorriones[,coln],cantidadIntervalos)
    
    print(tabFreq)
    
    resumen <- resumenMuestra(filteredGorriones[,coln],removeNA = TRUE)
    print(resumen)
    # doble enter asi no esta tan pegado
    cat("\n \n")
  }
  
  # enunciado C
  cat('c- Realizar en el caso que corresponda un histograma. Ensayar el número de intervalos que conviene en cada variable. \n')
  cat('\n')
  # reciclo los gorriones filtrados por las variables numericas
  # reciclo los intervalos
  for (coln in colnames(filteredGorriones) ) {
    #mostrar nombre columna
    cat("Var: ",coln,"\n")
    hist(filteredGorriones[,coln],breaks=cantidadIntervalos,freq=TRUE,include.lowest = TRUE, right = TRUE,main = coln,xlab = coln)
    # doble enter asi no esta tan pegado
    cat("\n \n")
  }

  # corroborar comentario :p 
 cat('viendo los histogramas, se puede verque en los campos largo total y largo pico cabeza, existe mayor heterogeneidad de valores.\n')
 
 # enunciado D
 cat('d- Realizar un boxplot comparativo para cada una de estas variables particionando por el grupo definido por la supervivencia. ')
 cat('Le parece que alguna de estas variables está relacionada con la supervivencia, es decir que toma valores muy distintos en ambos grupos. ')
 cat('Analizar en todos los casos la presencia de outliers. \n')
 
 # separo las variables que voy a analizar
 colsGGBox <- c('largoTotal','extAlar','largopicoCabeza','largoHumero','largoQuilla')
 # selecciono variables que vana  funcionar de id, vs las de medicion
 gorrionesGGplot<- melt(gorriones,id=c('id','vivo'),measure.vars = colsGGBox)

 #recorro todas las de medicion
  for (coln in colsGGBox ) {
    # hago un boxplot comparativo de VIVO/MUERTO respecto por cada variable de medicion
    plotVivoMuerto <- ggplot(data = gorrionesGGplot[gorrionesGGplot$variable==coln,], aes(x=variable, y=value)) + geom_boxplot(aes(fill=vivo))
    print(plotVivoMuerto)
  }
 
 cat('Viendo los Boxplot generados, se puede ver que en general son todos muy parecidos salvo \n')
 cat('Largo de Humero: en el que se puede ver que los vivos tienden a tener un humero cercano a la media. \n')
 cat('Existe un outlier de mayor tamaño que es el registro 13 que sigue vivo, sin embargo , queda comprendido en los bigotes del boxplot de los pajaros muertos; \n')
 cat('Manteniendo asi la observación respecto del humero.\n')
 cat('Largo Total: en el que se puede ver que gorriones mas cercanos a la media tienen mas chances de sobrevivir.\n')
 cat('\n')
 # enunciado E
 cat('e- Construir gráficos bivariados para las todas las variables,\n ')
 cat('particionando por el grupo de supervivencia. (un color para cada grupo).\n ')
 cat('Observa alguna regularidad que pueda explicar la supervivencia. \n')
 cat('\n')
 
 # pairs(gorriones[,2:7],col=gorriones[,7]) el gg pairs da mas info
 print(ggpairs(gorriones[,2:7],colour=colnames(gorriones)[7],params = c(binwidth = 0.1)))
 
 cat('Observando los graficos resultantes para comparacion bivariada podemos ver: \n')
 cat('- Hablando a nivel generico se observa una mayor correlacion entre todas las variables para los muertos respecto de los vivos.\n')
 cat('- en la relacion Extension alar vs Largo de Quilla; se destaca que los muertos tienen alta correlacion, mientras que los vivos no. la diferecia es de aprox ~34% \n')
 cat('- en la relacion Largo de humero vs Largo de Quilla; se destaca que los muertos tienen alta correlacion, mientras que los vivos no. la diferencia es de arpox ~25.7% \n')
 cat('- en la relacion Largo pico-cabeza vs Largo de Quilla; se destaca que los muertos tienen alta correlacion, mientras que los vivos no.la diferencia es de arpox ~13.6% \n')
 cat(' El resto de las variables en relacion al largo de quilla siguen tieniendo graneds diferencias.\n')
 cat(' Se puede concluir que el largo de quilla afecta mucho la calidad de vida de los gorriones.\n\n')
 
 # enunciado F
 cat('f- Calcular la matriz de varianzas y covarianzas muestrales y la matriz de correlación muestral: para el grupo general y para los subgrupos definidos por la sobrevida.')
 cat(' Comparar estas matrices con la del grupo general y entre sí. \n')
 cat('\n')
 #armar matriz varianzas y covarianzas para todos
 cat('\nMatriz varianzas y covarianzas \n')
 matrizVarCovT <- var(gorriones[,2:6])
 print(matrizVarCovT)
 
 # armar matriz correlacion  para todos
 cat('\nMatriz Correlación Vivos \n')
 matrizCorrelacionT <- cor(gorriones[,2:6])
 print(matrizCorrelacionT)

 #armar matriz varianzas y covarianzas para Vivos vs Muertos
 cat('\nMatriz varianzas y covarianzas Vivos \n')
 matrizVarCovV <-var(gorriones[gorriones$vivo==TRUE,2:6])
 print(matrizVarCovV)

 cat('\nMatriz varianzas y covarianzas Muertos \n')
 matrizVarCovM <-var(gorriones[gorriones$vivo==FALSE,2:6])
 print(matrizVarCovM)
 
 # armar matriz correlacion  para Vivos vs Muertos
 cat('\nMatriz Correlación VIVOS \n')
 matrizCorrelacionV <- cor(gorriones[gorriones$vivo==TRUE,2:6])
 print(matrizCorrelacionV)
 cat('\nMatriz Correlación MUERTOS \n')
 matrizCorrelacionM <- cor(gorriones[gorriones$vivo==FALSE,2:6])
 print(matrizCorrelacionM)
 
 #diferencia entre correlacion vivos vs muertos
 cat('\ndiferencia entre correlacion vivos vs muertos \n')
 diferenciaMuertosVsVivos <- matrizCorrelacionM-matrizCorrelacionV
 print(diferenciaMuertosVsVivos)
 
 cat('\n')
 cat('Como se puede observar en la matriz de correlacion la diferencia entre los valores de los muertos, que es mas grande, vs los vivos que es menor  \n')
 cat('confirmamos lo visto anteriormente en el punto F, sin embargo resaltan algunos valores mas como \n')
 cat('Largo Pico-cabeza Vs Largo humero: diferencia: 0.21055972 (~21%) \n')
 cat('Largo total Vs Extension alar: diferencia: 0.12172887 (~12%)  \n')
 cat('Largo total Vs Largo humero: diferencia: 0.11615907 (~11.6%)  \n')
 cat('\n Recordando lo visto en el punto F.')
 cat('A nivel generico se observa una mayor correlacion entre todas las variables para los muertos respecto de los vivos.\n')
 cat('Extension alar vs Largo de Quilla; diferecia: 0.34257144  ( ~34% ) \n')
 cat('Largo de humero vs Largo de Quilla; diferencia: 0.25751453 ( ~25.7% ) \n')
 cat('Largo pico-cabeza vs Largo de Quilla; diferencia: 0.13625102 ( ~13.6% ) \n')
 cat('Largo total vs Largo de Quilla; diferencia: 0.14651569 ( ~14.6% ) \n')
 cat('\n')
 cat('ordenando estos valores por la diferencia de las correlaciones obtenemos un grado de importancia respecto de la supervivencia del gorrion:')
 cat('Extension alar vs Largo de Quilla; diferecia: 0.34257144  ( ~34% ) \n')
 cat('Largo de humero vs Largo de Quilla; diferencia: 0.25751453 ( ~25.7% ) \n')
 cat('Largo Pico-cabeza Vs Largo humero: diferencia: 0.21055972 (~21%) \n')
 cat('Largo total vs Largo de Quilla; diferencia: 0.14651569 ( ~14.6% ) \n')
 cat('Largo pico-cabeza vs Largo de Quilla; diferencia: 0.13625102 ( ~13.6% ) \n')
 cat('Largo total Vs Extension alar: diferencia: 0.12172887 (~12%)  \n')
 cat('Largo total Vs Largo humero: diferencia: 0.11615907 (~11.6%)  \n')
 
 cat('\n')
 cat('\n')
 cat('g- Construir la matriz de diagramas de dispersión.  \n')
 cat('  Considera que algún par de estas medidas están relacionadas?.  \n')
 cat('	Estudiar si la asociación de algunas de estas medidas es diferente en alguno de los grupos. \n')
 cat('\n')
 pairs(gorriones[,2:7],col=gorriones[,7]) # el gg pairs da mas info
 cat('\n')
 
 cat('Se puede observar en el grafico que tanto vivos como muertos, en todas las variables estan muy correlacionadas, \n')
 cat('Salvo excepciones previamente analizadas en las matrices de correlacion y los graficos resultanntes en el punto F \n')
 
 
}


# para armar la tabla de frecuencias
armarTablaFrecuencias<- function (columna,cantidadIntervalos){
  # maximo de la muestra
  maximo <- max(columna)
  #minimo de la muestra
  minimo <- min(columna)
  
  
  # para tener que el maximo y minimo estan incluidos
#   maxInc <- (maximo+1)
#   minInc <- (minimo-1)
  espaciado <- ( maximo - minimo ) / cantidadIntervalos 
  
  # para armar los intervalos
  freqs <- seq( minimo, maximo, by= espaciado)
  
  cortaMuestra <- cut(columna,breaks = freqs,right = TRUE,include.lowest = TRUE)
  
  tablaFreq <- table(cortaMuestra)
  tablaFreq
}

# Arma resumen de la muestra teniendo en cuenta:
# 'Cant Muestra','Max', 'Min','Rango','Media','Q1','Mediana','Q3','Varianza','DesvioSTD','DistIQ','NA'
# puede configurarse para remover los NA
resumenMuestra <- function(muestraParam, removeNA=FALSE){
  
  muestra <- muestraParam
  NACount<- sum(is.na(muestra))
  
  if(removeNA ){
    if(NACount>0){
      cat("Muestra configurada para SACAR los NA, hay: ",NACount,"\n")
      muestra <- na.omit(muestraParam )
    }
    
  }else{
    cat("Muestra configurada para NO SACAR los NA, hay: ",NACount,"\n")
  }
  
  media <-NA
  mediana <- NA
  maximo <- NA
  minimo <- NA
  rango <-NA
  varianza <- NA
  desvioStd <- NA
  
  Q1 <- NA
  Q3 <- NA
  
  distInterQuart <- NA
  if(removeNA){
    Q1 <- quantile(muestra, 0.25)
    Q3 <- quantile(muestra, 0.75)
    distInterQuart <- IQR(muestra)
    
    media <-  mean(muestra)
    mediana <- median(muestra)
    maximo <- max(muestra)
    minimo <- min(muestra)
    rango <-max(muestra) -min(muestra)
    varianza <- var(muestra)
    desvioStd <- sd(muestra)
  }else{
    if(NACount == 0){
      Q1 <- quantile(muestra, 0.25)
      Q3 <- quantile(muestra, 0.75)
      distInterQuart <- IQR(muestra)
      
      media <-  mean(muestra)
      mediana <- median(muestra)
      maximo <- max(muestra)
      minimo <- min(muestra)
      rango <-max(muestra) -min(muestra)
      varianza <- var(muestra)
      desvioStd <- sd(muestra)
    }
  }
  
  result <- c(length(muestra),maximo, minimo,rango,media,Q1,mediana,Q3,varianza,desvioStd,distInterQuart, NACount)
  
  names(result) <- c('Cant Muestra','Max', 'Min','Rango','Media','Q1','Mediana','Q3','Varianza','DesvioSTD','DistIQ','NA')
  
  #N (número de observaciones de la variable)
  #media - mediana - ds (desvío standard)
  #Q1 (primer cuartil)- Q3 (tercer cuartil)- DI(desvío intercuartil)
  # saco el resto de las variables que no me solicitan
  result[-c(2,3,4,9,12)]


}


levantarDatosParaPrueba <- function(){
  # levantar datos:
  gorriones <- read.xlsx("Gorriones.xls", header=TRUE,sheetIndex = 1)
  # aprovecho para emprolijar nombres
  nombreCOlumnas<- c('id','largoTotal','extAlar','largopicoCabeza','largoHumero','largoQuilla','vivo')
  names(gorriones)<- nombreCOlumnas
  
  # aprovecho para emprolijar Valores
  gorriones$vivo[gorriones$vivo == 1] <- TRUE # es 1
  gorriones$vivo[gorriones$vivo == -1] <- FALSE # es 0 
  gorriones$vivo <- gorriones$vivo == 1
  gorriones$vivo <- as.factor(gorriones$vivo)
  
  gorriones
}

levantarDatosCrudos <- function(){
  # levantar datos:
  gorriones <- read.xlsx("Gorriones.xls", header=TRUE,sheetIndex = 1)
  # aprovecho para emprolijar nombres
  nombreCOlumnas<- c('id','largoTotal','extAlar','largopicoCabeza','largoHumero','largoQuilla','vivo')
  names(gorriones)<- nombreCOlumnas
  gorriones$vivo <- as.factor(gorriones$vivo)
  gorriones
}

probandoMelt <- function (muestra){
  result<- melt(muestra,id=c('id','vivo'),measure.vars = c('largoTotal','extAlar','largopicoCabeza','largoHumero','largoQuilla'))
  result
}

probandoMultiPlot<- function(muestra){
  melted <- probandoMelt(muestra)
  
  colsGGBox <- c('largoTotal','extAlar','largopicoCabeza','largoHumero','largoQuilla')
  for (coln in colsGGBox ) {
    plotVivoMuerto <- ggplot(data = melted[grep(coln, melted$variable),], aes(x=variable, y=value)) + geom_boxplot(aes(fill=vivo))
    print(plotVivoMuerto)
  }
  
}