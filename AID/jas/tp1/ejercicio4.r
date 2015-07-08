

ejecutar4 <- function(){
  # pre requisitos
  
  # para eliminar error de java
  if (Sys.getenv("JAVA_HOME")!="")
    Sys.setenv(JAVA_HOME="")
  
#   # carga librerias
#   require(xlsx)
#   
#   require(tidyr)
#   require(dplyr)
#   require(knitr)
#   
#   require(reshape2 )
#   require(GGally )
#   require(ggplot2)
#   #require(ggsubplot)
#   require(scales)
  #require(ggsubplot)
  checkLibs()
    
  # Ejercicio 4: Base de datos razaperros.xls
  # setear WD 
  #setwd('C:/Users/Julio Spairani/Dropbox/julio_box/educacion/maestria_explotacion_datos_uba/materias/cuat_1_aid/TP1/r_ws/')
  setwd('C:/Users/julio/Dropbox/julio_box/educacion/maestria_explotacion_datos_uba/materias/cuat_1_aid/TP1/r_ws/')
  
  # Se han registrado respecto de 27 razas de perros las siguientes características:
  #   Nombre de la raza
  # Tamaño: 1- pequeño, 2- mediano y 3- grande
  # Peso: 1- liviano, 2- medio y 3- pesado
  # Velocidad: 1- lento, 2- mediano y 3- rápido
  # Inteligencia: niveles 1 a 3
  # Afectividad: niveles 1 a 3
  # Agresividad: niveles 1 a 3
  # Función: con tres categorías caza, utilitario y compañía.
  perrosRaw <- levantarDatosParaPrueba()
  #perros<-levantarDatosValidados()
  
  
  # a- Realizar un gráfico de estrellas por raza(utilizando las variables tamaño, peso, velocidad, inteligencia y afectividad
  cat('\n  a- Realizar un gráfico de estrellas por raza(utilizando las variables tamaño, peso, velocidad, inteligencia y afectividad \n')
  cat(' \n  Ver Gráfico: perros x raza \n')  
  filtroA <- c(2,3,4,5,6)
  perrosNombres <- perrosRaw[,1]
  perrosFilteredA <-perrosRaw[, filtroA]
  
  rownames(perrosFilteredA)<- perrosNombres
  
  palette(rainbow(12, s = 0.6, v = 0.75))
  stars( perrosFilteredA,labels = abbreviate(case.names(perrosFilteredA),minlength = 6) , len = 0.6, key.loc = c(10, 2),main = "perros x raza", draw.segments = TRUE )
  
  
  cat(' \n  b- Idem por función. \n')
  cat(' \n  Ver Gráfico: perros x función \n')  
  # agregarlos por funcion y luego grafico
  perrosFilteredB <- ddply(perrosRaw,.(funcion),numcolwise(sum))
    #melt(perrosRaw,id=c('funcion'),measure.vars = c('size',  'peso',  'velocidad',  'inteligencia',	'afectividad'))  
  
  funcionNom <- levels(perrosFilteredB$funcion)
  perrosFilteredB <- perrosFilteredB[, filtroA]
  rownames(perrosFilteredB)<- funcionNom
  palette(rainbow(12, s = 0.6, v = 0.75))
  stars( perrosFilteredB,labels = substr((case.names(perrosFilteredB)),0,4) , len = 0.6, key.loc = c(5, 2.5),main = "perros x funcion", draw.segments = TRUE )
  
  
  # c- Idem por agresividad.
  cat(' \n  b- Idem por agresividad. \n')
  cat(' \n  Ver Gráfico: perros x agresividad. \n') 
  perrosFilteredC <- ddply(perrosRaw,.(agresividad),numcolwise(sum))
  perrosFilteredC$agresividad <- as.factor(perrosFilteredC$agresividad )
  agreNom <- droplevels(perrosFilteredC$agresividad)
  perrosFilteredC <- perrosFilteredC[, filtroA]
  rownames(perrosFilteredC)<- agreNom
  palette(rainbow(12, s = 0.6, v = 0.75))
  stars( perrosFilteredC,labels = substr((case.names(perrosFilteredC)),0,4) , len = 0.6, key.loc = c(5, 2.5),main = "perros x agresividad", draw.segments = TRUE )
  
  # d- En el primer gráfico encuentra estrellas similares. Le parece que las razas son parecidas?.
  cat(' \n  d- En el primer gráfico encuentra estrellas similares. Le parece que las razas son parecidas?. \n')
cat('   > tienen todas sus cualidades iguales, \n')
cat('   > los que tienen ~ se diferencian por poco, especificado entre parentesis\n')
cat('   > los que tienen -- no tienen similares\n')
cat('   \n')
cat('   beacrn, collie,~pitbull(mas inteligente)\n')
cat('   boxer, labrador, dalmata\n')
cat('   bulldog,teckel\n')
cat('   blmstf, ~snbrnr(menos inteligente 2),~mastin(menos inteligente 1)\n')
cat('   caniche --\n')
cat('   chihuahua,pekines\n')
cat('   cocker, --\n')
cat('   doberman, pointer\n')
cat('   dogo,--\n')
cat('   foxhound,galgo,~setter ( mas inteligente 2)\n')
cat('   gascon,~podf ( mas inteligente 2 )\n')
cat('   podb,--\n')
}

levantarParaGGPlot <- function(muestra){
  graph<- melt(muestra,id=c('raza'),measure.vars = c('size',  'peso',  'velocidad',	'inteligencia',	'afectividad'))
  #graph<- melt(muestra,id=c('raza'),measure.vars = c('size',  'peso',  'velocidad',  'inteligencia',	'afectividad',	'agresividad'))
  graph
}

levantarDatosParaPrueba <- function(){
  # levantar datos:
  perros <- read.xlsx("razaperros.xls", header=TRUE,sheetIndex = 1,encoding = 'ASCII')
  # aprovecho para emprolijar nombres
  nombreCOlumnas<- c('raza',  'size',	'peso',	'velocidad',	'inteligencia',	'afectividad',	'agresividad'	,'funcion')
  names(perros)<- nombreCOlumnas

  # agrego el tipo compañía bien escrito
  levels(perros$funcion) <- c(levels(perros$funcion), 'compañía')
  # lo seteo
  perros$funcion[grep('compa', perros$funcion)  ] <- 'compañía'
  # lo elimino al que quedo vacio
  perros$funcion <- droplevels(perros$funcion)
  
  # aprovecho para emprolijar Valores
  perros
}

# a modo practica 
clasificacionDeVars <- function(){
  # raza: Nombre de la raza
  # Tamaño: 1- pequeño, 2- mediano y 3- grande
  # Peso: 1- liviano, 2- medio y 3- pesado
  # Velocidad: 1- lento, 2- mediano y 3- rápido
  # Inteligencia: niveles 1 a 3
  # Afectividad: niveles 1 a 3
  # Agresividad: niveles 1 a 3
  # Función: con tres categorías caza, utilitario y compañía.
  cat('\n')
  cat(' Clasificacion de variables para practica \n')
  cat(' Raza: Nombre de la raza : categorica \n')
  cat(' Tamaño: 1- pequeño, 2- mediano y 3- grande : categorica: ordinal \n')
  cat(' Peso: 1- liviano, 2- medio y 3- pesado : categorica: ordinal \n')
  cat(' Velocidad: 1- lento, 2- mediano y 3- rápido : categorica: ordinal \n')
  cat(' Inteligencia: niveles 1 a 3 : categorica: ordinal \n')
  cat(' Afectividad: niveles 1 a 3 : categorica: ordinal \n')
  cat(' Agresividad: niveles 1 a 3 : categorica: ordinal \n')
  cat(' Función: con tres categorías caza, utilitario y compañía.: categorica \n')
  cat('\n')
}

levantarDatosValidados <- function(muestra ){
  #   Nombre de la raza
  # Tamaño: 1- pequeño, 2- mediano y 3- grande
  # Peso: 1- liviano, 2- medio y 3- pesado
  # Velocidad: 1- lento, 2- mediano y 3- rápido
  # Inteligencia: niveles 1 a 3
  # Afectividad: niveles 1 a 3
  # Agresividad: niveles 1 a 3
  # Función: con tres categorías caza, utilitario y compañía.
  
  muestra$size[muestra$size ==1 ] <- 'pequeño'
  muestra$size[muestra$size ==2 ] <- 'mediano'
  muestra$size[muestra$size ==3 ] <- 'grande'
  
  muestra$peso[muestra$peso ==1 ] <- 'liviano'
  muestra$peso[muestra$peso ==2 ] <- 'medio'
  muestra$peso[muestra$peso ==3 ] <- 'pesado'
  
  muestra$velocidad[muestra$velocidad ==1 ] <- 'lento'
  muestra$velocidad[muestra$velocidad ==2 ] <- 'mediano'
  muestra$velocidad[muestra$velocidad ==3 ] <- 'rápido'
  
  # agrego el tipo compañía bien escrito
  levels(muestra$funcion) <- c(levels(muestra$funcion), 'compañía')
  # lo seteo
  muestra$funcion[grep('compa', muestra$funcion)  ] <- 'compañía'
  # lo elimino al que quedo vacio
  muestra$funcion <- droplevels(muestra$funcion)
  
  
  muestra
}


#deprecated cosas

# melt para hacerlo apto para ggplot
#paraPlotA <- levantarParaGGPlot(perrosRaw)
#   plotA<-ggplot(paraPlotA, aes(x = variable, y = value, colour = raza, group = raza)) +
#     geom_star() +
#     scale_y_continuous(labels = value)


#   filterRaza <-   perrosRaw$raza
# 
# for (raz in filterRaza ) {
#   
#   current <- paraPlotA[paraPlotA$raza==raz,]
#   
#   print(ggplot(current) + geom_star(aes(x = variable, y = value, colour = raza, group = raza)))
# }

# el facet wrap engancha todo tipo grilla
# facet grid lo hace tipo tabla, sirve para armar bivariados.
# starPlots<-ggplot(paraPlotA) + 
#   geom_star(aes(x = variable, y = value, colour = raza, group = raza))+ 
#   facet_wrap( ~raza)
# print(starPlots)

checkLibs<- function(){
    # buscando paquete:  xlsx 
    if(!require(xlsx)) { 
        install.packages("xlsx")
        require(xlsx)
    }else{
        cat('ya instalado y cargado: xlsx \n ')
    }
    # buscando paquete:  ggplot2 
    if(!require(ggplot2)) { 
        install.packages("ggplot2")
        require(ggplot2)
    }else{
        cat('ya instalado y cargado: ggplot2 \n ')
    }
    # buscando paquete:  tidyr 
    if(!require(tidyr)) { 
        install.packages("tidyr")
        require(tidyr)
    }else{
        cat('ya instalado y cargado: tidyr \n ')
    }
    # buscando paquete:  plyr 
    if(!require(plyr)) { 
        install.packages("plyr")
        require(plyr)
    }else{
        cat('ya instalado y cargado: plyr \n ')
    }
    # buscando paquete:  dplyr 
    if(!require(dplyr)) { 
        install.packages("dplyr")
        require(dplyr)
    }else{
        cat('ya instalado y cargado: dplyr \n ')
    }
    # buscando paquete:  knitr 
    if(!require(knitr)) { 
        install.packages("knitr")
        require(knitr)
    }else{
        cat('ya instalado y cargado: knitr \n ')
    }
    # buscando paquete:  reshape2 
    if(!require(reshape2)) { 
        install.packages("reshape2")
        require(reshape2)
    }else{
        cat('ya instalado y cargado: reshape2 \n ')
    }
    # buscando paquete:  GGally 
    if(!require(GGally)) { 
        install.packages("GGally")
        require(GGally)
    }else{
        cat('ya instalado y cargado: GGally \n ')
    }
    
    # buscando paquete:  scales 
    if(!require(scales)) { 
        install.packages("scales")
        require(scales)
    }else{
        cat('ya instalado y cargado: scales \n ')
    }
}