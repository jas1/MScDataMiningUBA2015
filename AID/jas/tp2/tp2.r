# AID - Trabajo Práctico Nro. II
# Ejercicio 1:
# Dada la matriz de datos de Gorriones.xls (TPI), se pide:
# a) Dimensión de la base de datos(n= número de observaciones, p= cantidad de variables observadas sobre cada individuo)
# b) Hallar el vector de medias, la matriz de varianzas y covarianzas y la matriz de correlaciones. Como son estas matrices?.
# c) Explicar que representa el elemento m11 de la matriz de varianzas y covarianzas, ídem para el elemento m31.
# d) Explicar que representa el elemento m22 de la matriz de correlaciones, ídem para el elemento m13.
# e) Relacionar los elementos m21, m11 y m22 de la matriz de varianzas y covarianzas con el elemento m12 de la matriz de correlaciones.
# f) Hallar una nueva variable e incorporarla en la base de Gorriones, Diferencia entre el largo total y largo del humero, llamémosla diferencia de largos.
# g) Calcular nuevamente el vector de medias y las matrices de varianzas y covarianzas y la matriz de correlaciones de la nueva base de datos. Relacionar el nuevo vector de medias con el anterior.
# h) Hallar la traza de las cuatro matrices. Explicar el significado de cada uno de los resultados. Que traza/s no aumentan al aumentar una variable? Explique.
# 
# Ejercicio 2:
# Para el archivo de recepcionistas.xls(TPI) se pide:
# a) Calcular el vector de medias y explicar su significado.
# b) Hallar las matrices de varianzas y covarianzas y de correlaciones para la submatriz de puntuaciones del primer juez, ídem para el segundo juez. Ídem para el conjunto total.
# c) Se puede decir que la suma de las dos primeras submatrices den como resultado la matriz del grupo total. Si no es así por favor explique por que no.
# d) Se cumple esta relación para las trazas?

ejecutar2 <- function(){
    # pre requisitos
    
    # para eliminar error de java
    if (Sys.getenv("JAVA_HOME")!="")
        Sys.setenv(JAVA_HOME="")

    #setwd('C:/Users/Julio Spairani/Dropbox/julio_box/educacion/maestria_explotacion_datos_uba/materias/cuat_1_aid/TP1/r_ws/')
    setwd('C:/Users/julio/Dropbox/julio_box/educacion/maestria_explotacion_datos_uba/materias/cuat_1_aid/tp2/r_ws')
    
    #   # carga librerias
    checkLibs()
    
    # TP
    enunciado1()
    
    #ej1
    ejercicio1()
    
    # TP
    enunciado2()
    
    #ej2
    ejercicio2()

    
}
ejercicio1<-function(){
    muestra <- levantaGorriones()

    cat('\n a) Dimensión de la base de datos(n= número de observaciones, p= cantidad de variables observadas sobre cada individuo)\n')
    # sacar las columnas que categoricas
    filteredCats <- c('largoTotal','extAlar','largopicoCabeza','largoHumero','largoQuilla')
    muestra2 <- muestra[,filteredCats]
    dimensiones <-  dim(muestra2)
    print(dimensiones)
    cat('\n n:',dimensiones[1],'|p:',dimensiones[2],'  \n')
    
    cat('\n b) Hallar el vector de medias, la matriz de varianzas y covarianzas y la matriz de correlaciones. Como son estas matrices?.\n')
    
    
    vmedias <- colMeans(muestra2)
    cat('\nvector Medias \n')
    cat(vmedias,'\n')

    
    matVarCov <- var(muestra2)
    cat('\nmatriz de varianzas y covarianzas \n')
    print(matVarCov)
    
    matCor <- cor(muestra2)
    cat('\nmatriz de correlaciones. \n')
    print(matCor)
    
    cat('\n Estas Matrices son los mismos valores desplazados de escala, uno es con referencia al eje x,y de los valores, el otro es el origen desplazado a las medias de las variables. \n ')
    
    cat('\n c) Explicar que representa el elemento m11 de la matriz de varianzas y covarianzas, ídem para el elemento m31.\n')
    
    cat('\n El elemento m11 , es la varianza de la variable 1. \n ')
    cat('El elemento m31 , es la covarianza entre la variable 3 y la variable 1 , que es el mismo valor que m13 ( por productos 3*1 = 1*3)\n')
    cat('\n valores Var/Cov:  m11: ', matVarCov[1,1],'|var m31', matVarCov[3,1],'|var m13', matVarCov[1,3])
    
    cat('\n d) Explicar que representa el elemento m22 de la matriz de correlaciones, ídem para el elemento m13.\n')
    cat('\n El elemento m22 , es la correlacion de la variable 2. \n ')
    cat('El elemento m13 , es la correlacion entre la variable 1 y la variable 3, que es el mismo valor que m31 ( por productos 3*1 = 1*3)\n\n ')
    cat('\n valores  Corr:  m22: ', matCor[2,2],'|var m13:', matCor[1,3],'|var m31:', matCor[3,1])
   
    cat('\n e) Relacionar los elementos m21, m11 y m22 de la matriz de varianzas y covarianzas con el elemento m12 de la matriz de correlaciones.\n')
    cat('\n valores Var/Cov:  m21: ', matVarCov[2,1],'|var m11:', matVarCov[1,1],'|var m22:', matVarCov[2,2])
    cat('\n valores Corr:  m12: ', matCor[2,1])
    
    cat('\n TODO: que buena pregunta ... \n ')
    
    cat('\n f) Hallar una nueva variable e incorporarla en la base de Gorriones, Diferencia entre el largo total y largo del humero, llamémosla diferencia de largos.\n')
    muestra3 <- muestra2
    muestra3$diferenciaLargos <-  muestra3$largoTotal -  muestra3$largoHumero
    
    cat('\n g) Calcular nuevamente el vector de medias y las matrices de varianzas y covarianzas y la matriz de correlaciones de la nueva base de datos. Relacionar el nuevo vector de medias con el anterior.\n')
    
    vmediasN <- colMeans(muestra3)
    cat('\nNuevo vector Medias  \n')
    cat(vmediasN,'\n')
    
    matVarCovN <- var(muestra3)
    cat('\nNueva matriz de varianzas y covarianzas \n')
    print(matVarCovN)
    
    matCorN <- cor(muestra3)
    cat('\nNueva matriz de correlaciones. \n')
    print(matCorN)
    
    cat('\nComparacion / relacion Vectores medias. \n')
    cat('Anterior Vector De Medias: \n')
    cat(vmedias,'\n')
    cat('Nuevo Vector De Medias: \n')
    cat(vmediasN,'\n')
    
    cat('\n TODO: Relacion/comparacion: que buena pregunta ... \n ')
    
    cat('\nh) Hallar la traza de las cuatro matrices. Explicar el significado de cada uno de los resultados. Que traza/s no aumentan al aumentar una variable? Explique.\n')
    
    tr1 <- sum(diag(matVarCov))
    tr2 <- sum(diag(matCor))
    tr3 <- sum(diag(matVarCovN))
    tr4 <- sum(diag(matCorN))
    
    
    cat('\n Traza Matriz Varianza/covarianza Gorriones\n')
    print(tr1)
    cat('\n Traza Matriz Correlacion Gorriones\n')
    print(tr2)
    cat('\n Traza Nueva Matriz Varianza/covarianza Gorriones\n')
    print(tr3)
    cat('\n Traza Nueva Matriz Correlacion Gorriones\n')
    print(tr4)
    
    cat('\n Todas las trazas aumentaron. sin embargo la que mas relacion mantiene es la de correlacion , dado que la correlacion con la misma columna es 1 , por lo tanto , termina dando el total de variables analizadas de la muestra (columnas seleccionadas) \n ')
    
    cat('\n TODO: Relacion/comparacion: que buena pregunta ... algo mas ?  \n ')
}

#carga datos
levantaGorriones <- function(){
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

enunciado1<-function(){
# Ejercicio 1:
# Dada la matriz de datos de Gorriones.xls (TPI), se pide:
# a) Dimensión de la base de datos(n= número de observaciones, p= cantidad de variables observadas sobre cada individuo)
# b) Hallar el vector de medias, la matriz de varianzas y covarianzas y la matriz de correlaciones. Como son estas matrices?.
# c) Explicar que representa el elemento m11 de la matriz de varianzas y covarianzas, ídem para el elemento m31.
# d) Explicar que representa el elemento m22 de la matriz de correlaciones, ídem para el elemento m13.
# e) Relacionar los elementos m21, m11 y m22 de la matriz de varianzas y covarianzas con el elemento m12 de la matriz de correlaciones.
# f) Hallar una nueva variable e incorporarla en la base de Gorriones, Diferencia entre el largo total y largo del humero, llamémosla diferencia de largos.
# g) Calcular nuevamente el vector de medias y las matrices de varianzas y covarianzas y la matriz de correlaciones de la nueva base de datos. Relacionar el nuevo vector de medias con el anterior.
# h) Hallar la traza de las cuatro matrices. Explicar el significado de cada uno de los resultados. Que traza/s no aumentan al aumentar una variable? Explique.
    
    cat(' Ejercicio 1: \n')
    cat(' Dada la matriz de datos de Gorriones.xls (TPI), se pide:\n')
    cat(' a) Dimensión de la base de datos(n= número de observaciones, p= cantidad de variables observadas sobre cada individuo)\n')
    cat(' b) Hallar el vector de medias, la matriz de varianzas y covarianzas y la matriz de correlaciones. Como son estas matrices?.\n')
    cat(' c) Explicar que representa el elemento m11 de la matriz de varianzas y covarianzas, ídem para el elemento m31.\n')
    cat(' d) Explicar que representa el elemento m22 de la matriz de correlaciones, ídem para el elemento m13.\n')
    cat(' e) Relacionar los elementos m21, m11 y m22 de la matriz de varianzas y covarianzas con el elemento m12 de la matriz de correlaciones.\n')
    cat(' f) Hallar una nueva variable e incorporarla en la base de Gorriones, Diferencia entre el largo total y largo del humero, llamémosla diferencia de largos.\n')
    cat(' g) Calcular nuevamente el vector de medias y las matrices de varianzas y covarianzas y la matriz de correlaciones de la nueva base de datos. Relacionar el nuevo vector de medias con el anterior.\n')
    cat(' h) Hallar la traza de las cuatro matrices. Explicar el significado de cada uno de los resultados. Que traza/s no aumentan al aumentar una variable? Explique.\n')

    
}





ejercicio2<-function(){
    muestra <- levantaRecepcionistas()
    
    cat(' a) Calcular el vector de medias y explicar su significado.\n')
    #acomodo para que sea solo valores
    filtroValores <- c('cord.juez1','pres.juez1','idiom.juez1','cord.juez2','pres.juez2','idiom.juez2')
    muestra2 <- muestra[,filtroValores]
    rownames(muestra2) <- muestra[,c('candidatos')]
    
    #vector de medias es un vector con la media de cada variable, cada variable esta representada en cada columna
    vmedias <- colMeans(muestra2)
    cat(vmedias,'\n')
    cat('vector de medias es un vector con la media de cada variable, cada variable esta representada en cada columna \n')
    
    cat('\nb) Hallar las matrices de varianzas y covarianzas y de correlaciones para la submatriz de puntuaciones del primer juez, ídem para el segundo juez. Ídem para el conjunto total.\n')
    # separar la matriz del 1er juez y la del segundo
    filtroValoresJuez1 <- c('cord.juez1','pres.juez1','idiom.juez1')
    matJuez1 <- muestra2[,filtroValoresJuez1]
    matJuez1Cov <- var(matJuez1)
    cat('\nmatriz Varianza/covarianza Juez 1\n')
    print(matJuez1Cov)
    
    filtroValoresJuez2 <- c('cord.juez2','pres.juez2','idiom.juez2')
    matJuez2 <- muestra2[,filtroValoresJuez2]
    matJuez2Cov <- var(matJuez2)
    cat('\nmatriz Varianza/covarianza Juez 2\n')
    print(matJuez2Cov)
    
    matTotalCov <- var(muestra2)
    cat('\nmatriz Varianza/covarianza Total\n')
    print(matTotalCov)
    
    cat(' c) Se puede decir que la suma de las dos primeras submatrices den como resultado la matriz del grupo total. Si no es así por favor explique por que no.\n')
    # sumo
    totalSumado <- matJuez1Cov + matJuez2Cov
    cat('\n Suma de matrices de Varianza/covarianza J1 y J2\n')
    print(totalSumado)

    # diferencia deberia ser 0 si fuese cierto
    #resultadoDifCov <- totalSumado-matTotalCov
    cat('\n Diferencia de de matrices de Varianza/covarianza J1 + J2 VS total \n')
    print("No se puede realizar")
    
    cat('\n Conclusion C: no se puede realizar la diferencia entre la suma de las matrices j1+j2 , contra la total , porque se habla de dimensiones diferentes de matrices. \n')
    cat('J1+J2 es de 3x3 , mientras que total es de 6x6 \n')
    
    cat('En la suma de matrices se suma x[i,i] con y[i,i] lo que seria sumar cada 1 de las Var/Cov lo cual seria tratar de sumar cosas diferentes\n')
    cat('En la creacion de la matriz total , se muestran las relaciones entre todas las variables, que no es lo mismo que sumar relaciones. \n')
    
    cat('\n TODO:  Me falta speech desde el punto de vista de negocio ... o sea se que son variabels diferentes, pero me falta speech. \n')
    
    
    cat('\n d) Se cumple esta relación para las trazas?\n')
    # reciclo lo calculado anteriormente
    cat('\n Traza J1 \n')
    #trj1 <- matrix.trace(matJuez1Cov)
    trj1 <- sum(diag(matJuez1Cov))
    print(trj1)
    cat('\n Traza J2 \n')
    #trj2 <- matrix.trace(matJuez2Cov)
    trj2 <- sum(diag(matJuez2Cov))
    print(trj2)
    cat('\n Traza Total \n')
    #trTotal <- matrix.trace(matTotalCov)
    trTotal <- sum(diag(matTotalCov))
    print(trTotal)
    
    cat('\n Diferencia de Trazas de Matrices de Varianza/covarianza J1 + J2 VS total \n')
    difTr <- trj1 +trj2 - trTotal
    print(difTr)
    
    cat('\n Conclusion D: si esta relacion se cumple para las trazas. \n')
    cat('\n Por una de las propiedades de la traza: Tr(A+B) = Tr(A)+Tr(B) \n')
    
    cat('\n TODO: Me falta speech desde el punto de vista de negocio y matematico... se que es una propiedad ... pero que mas ? \n')

}

# imprime por pantalla el enunciado 2
enunciado2<-function(){
    # Ejercicio 2:
    # Para el archivo de recepcionistas.xls(TPI) se pide:
    # a) Calcular el vector de medias y explicar su significado.
    # b) Hallar las matrices de varianzas y covarianzas y de correlaciones para la submatriz de puntuaciones del primer juez, ídem para el segundo juez. Ídem para el conjunto total.
    # c) Se puede decir que la suma de las dos primeras submatrices den como resultado la matriz del grupo total. Si no es así por favor explique por que no.
    # d) Se cumple esta relación para las trazas?
    cat(' Ejercicio 2: \n')
    cat(' Para el archivo de recepcionistas.xls(TPI) se pide:\n')
    cat(' a) Calcular el vector de medias y explicar su significado.\n')
    cat(' b) Hallar las matrices de varianzas y covarianzas y de correlaciones para la submatriz de puntuaciones del primer juez, ídem para el segundo juez. Ídem para el conjunto total.\n')
    cat(' c) Se puede decir que la suma de las dos primeras submatrices den como resultado la matriz del grupo total. Si no es así por favor explique por que no.\n')
    cat(' d) Se cumple esta relación para las trazas?\n')
    
}

#carga datos
levantaRecepcionistas <- function(){
    # levantar datos:
    recepcionistas <- read.xlsx("recepcionistas_actualizado_version_excel.xls", header=TRUE,sheetName = "Hoja1")
    # aprovecho para emprolijar nombres
    nombreCOlumnas<- c('candidatos','cord.juez1','pres.juez1','idiom.juez1','cord.juez2','pres.juez2','idiom.juez2')
    names(recepcionistas)<- nombreCOlumnas
    recepcionistas
}

# utilitario
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
    
#     # buscando paquete:   matrixcalc 
#     if(!require( matrixcalc)) { 
#         install.packages(" matrixcalc")
#         require( matrixcalc)
#     }else{
#         cat('ya instalado y cargado:  matrixcalc \n ')
#     }
    
}