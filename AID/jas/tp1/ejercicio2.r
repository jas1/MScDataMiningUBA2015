# ESTE SCRIPT ESTA EN DESARROLLO , TODAVIA NO ESTA TERMINADO Y PUEDE TENER ERRORES

# depende del WS , tiene que tener el archivo de dataset en el WS
# devuelve el data table de dataset
iniciarEj2 <- function(workDir){
  
  #iniciar el working dir
  setwd(workDir)
  
  # eliminar error java home en windows de x64
  if (Sys.getenv("JAVA_HOME")!="")
    Sys.setenv(JAVA_HOME="")
  
  # para inistalar libs, descomentar si no estan instaladas
  #install.packages("ggplot2")
  #install.packages("xlsx")
  #install.packages("rmarkdown")
  
  # levanta librerias
  library(xlsx)
  library("ggplot2")
  
  # si explota por algo de no columnas, revisar o de version, guardar devuelta el excel con una version mas actual
  resultado <- read.xlsx("Internet2013.xls", header=TRUE,sheetName = "Semana Comp 2004")
  
  
}
# antes de poner a buscar valores hay que depurar un poco 
# muestra y nombre de campo
filtroMonoVar <- function (muestra){
#  tipovar  					variable&desc
#  categorica 					ID: nÃºmero de identificación del registro. 
  # no hace falta
  
#  categorica					Nacionalidad
  # no hace falta > podria depurarlo
  
#  cuantitativa discreta		Edad: cumplida en años
  #  hace falta > depurar rangos entre  5 y 80 > agregar flag de rompe Campo Edad 
    muestra$outlierEdad[muestra$Edad > 80 | muestra$Edad <5 ] <- TRUE
    muestra$outlierEdad[muestra$Edad <= 80 & muestra$Edad >=5 ] <- FALSE

#  categorica					Sexo: Masculino (1) Femenino (2)
  #  hace falta > depurar si 1 o 2, marcar como campo con error en campo sexo, y poner NA
    muestra$tieneErrorSexo[muestra$Sexo != 1 & muestra$Sexo != 2] <- TRUE
    muestra$Sexo[muestra$Sexo != 1 & muestra$Sexo != 2] <- NA
    muestra$tieneErrorSexo[muestra$Sexo == 1 | muestra$Sexo == 2]<- FALSE

#  cuantitativa				Estatura: en m.
  #  hace falta > marcar si va a esta fuera de 0.5 y 2.50
    
    muestra$outlierEstatura[muestra$Estatura > 2.20 | muestra$Estatura < 0.5 ] <- TRUE
    muestra$outlierEstatura[muestra$Estatura <= 2.20 & muestra$Estatura >= 0.5 ] <- FALSE

#  categorica					Sitio: sitio preferido al que se conecta; 1- chat\ 2 - correo electrÃ³nico\ 3- buscadores\ 4 â software\ 5 â mÃºsica\ 6 â deportes \ 7 - otros
  #  hace falta > marcar si va a esta fuera de  1...7
    #rangoSitio <- seq(from=1,to=7,by=1)

    muestra$errorSitio[muestra$Sitio < 1 | muestra$Sitio >7 ] <- TRUE
    muestra$Sitio[muestra$Sitio < 1 | muestra$Sitio >7 ] <- NA
    muestra$errorSitio[muestra$Sitio >= 1 & muestra$Sitio <= 7 ] <- FALSE

#  cuantitativa continua		Uso: Tiempo de uso promedio por dÃ­a en minutos
  #  hace falta > si esta muy fuera de minutos ...  habria que revisar ...
    muestra$errorUso[muestra$Uso < 0 ] <- TRUE
    muestra$Uso[muestra$Uso < 0 ] <- NA
    muestra$errorUso[muestra$Uso >= 0 ] <- FALSE
  
#  cuantitativa continua		Temperatura: media anual de la zona de residencia
  # hace falta >  si esta fuera de -10 a 40

muestra$outlierTemperatura[muestra$Temperatura < -15 | muestra$Temperatura > 40] <- TRUE
muestra$outlierTemperatura[muestra$Temperatura >= -15 & muestra$Temperatura <= 40] <- FALSE
  
#  cuantitativa discreta		Autos: cantidad de autos en la manzana donde reside
  # hace falta >  si esta fuera de 0 a 200

    muestra$outlierAutos[muestra$Autos < 0 | muestra$Autos > 200] <- TRUE
    muestra$Autos[muestra$Autos < 0 ] <- NA
    muestra$outlierAutos[muestra$Autos >= 0 & muestra$Autos <= 200] <- FALSE

  
#  cuantitativa discreta		Cigarrillos: Cantidad de cigarrillos consumidos mientras utiliza Internet
  # hace falta >  si esta fuera de 0 a 30
    #si hay menos de 0 es error
    muestra$outlierCigarrillos[muestra$Cigarrillos < 0 | muestra$Cigarrillos > 30 ] <- TRUE
    muestra$outlierCigarrillos[muestra$Cigarrillos >= 0 & muestra$Cigarrillos <= 30 ] <- TRUE
    muestra$errorCigarrillos[muestra$Cigarrillos < 0 ] <- TRUE
    muestra$errorCigarrillos[muestra$Cigarrillos >= 0 ] <- FALSE
    muestra$Cigarrillos[muestra$Cigarrillos < 0 ] <- NA

# devuelve muestra
  muestra
}

# dada una muestra en la que quiero buscar algunas columnas en particular
seleccionarColumnasConKeyword <- function (muestra, keyword){
    muestra[ ,grepl( keyword , names( muestra ) ) ]
}

# tabla de frecuencias de la muestra, debe ser 1 columna solamente.
tablaFrecuencias<-function(muestra){
    tab <- ftable(muestra)
    tab
}

# alfa es un porcentaje a eliminar del inicio y del fin de la muestra.
# TODO
muestraPodadaAlfa<-function(muestra,alfa){
    # ordenar
    # determinar como sacar el alfa 
    # devolver la muestra sin esos valores
}

#mostrar los valores que estan fuera de los bigotes del boxplot
# valores > 1.5 * IQR
# valores Extremos > 3 * IQR
resumenBoxPlotSalvajes <- function(muestra){
    
    Q1 <- quantile(muestra, 0.25,na.rm = TRUE)
    Q3 <- quantile(muestra, 0.75,na.rm = TRUE)
    iqr <- IQR(muestra,na.rm = TRUE)
    bigote <-  iqr * 1.5
    bigoteExtremo <-  iqr * 3
    
    min <- Q1 - bigote
    max <- Q3 + bigote
    
    minExtremo <- Q1 - bigoteExtremo
    maxExtremo <- Q3 + bigoteExtremo
    
    #Salvajes
    salvajes <- muestra[muestra < min | muestra > max]
    salvajesExtremos <- muestra[muestra < minExtremo | muestra > maxExtremo]
    cat("Q1:" ,Q1 , " | Q3 : ", Q3 , " | IQR:",iqr, " | Bigote:",bigote," | Bigote Extremo:",bigoteExtremo," \n" )
    cat("Limite Minimo:" ,min , " | Limite Max : ", max , " | Listado Salvajes: \n" )
    cat(salvajes)
    cat("\nLimite Minimo Extremo:" ,minExtremo , " | Limite Max Extremo: ", maxExtremo , " | Listado Salvajes Exremos: \n" )
    cat(salvajesExtremos)
}

# para ver el resumen de salvajes de a todas las variables de 1
aplicarPunto10ColsAPlicables<- function(muestra){
    # filtro de columnas a aplicar
    filtroCols<- c("Edad","Estatura", "Temperatura", "Uso", "Autos","Cigarrillos")
    # aplicando filtro de columnas
    filteredMUestra <- muestra[,filtroCols]
    
    # para cada columna
    for (coln in colnames(filteredMUestra) ) {
        #mostrar nombre columna
        cat("Var: ",coln,"\n")
        # aplicar analisis de salvajes
        resumenBoxPlotSalvajes(filteredMUestra[,coln])
        # doble enter asi no esta tan pegado
        cat("\n \n")
    }

}

# en realidad esta f uncion no dice nada, solo describe los puntos realizados y de que forma.
resumenEj2 <- function(){
    # punto 1 | A)  ver fuente de del metodo filtroMonovar
    #   muestra <- iniciarEj2("poner el WD que utilizas, en el mismo debe estar el xls")
    #   filtroMue <- filtroMonoVar(muestra) 
    #    > este se fija todas las columnas y agrega si tienen error u outlier segun criterios para cada col
    #    > en caso de error la variable se pone como NA, en caso de outlier se deja, pero se agrega una col que dice que es outlier
    
    # punto 2 | B)  tablaFrecuencias(muestra[,"Sexo"])
    #           >   existe un valor fuera de los validos, es un error de entrada.
    #

    # punto 3 | C) filtroMue$Edad[filtroMue$outlierEdad==TRUE,] > para ver los datos en conjunto
    #          filtroMue[filtroMue$outlierEdad==TRUE,"Edad"] > para mostrar los outliers
    #           > Los siguientes valores son outliers de edad segun elc riterio entre 5 y 80 años
    #           > 280 120 -44  88   1 180  99
    #           > 88 99 1 > pueden ser valores excepcionales
    #           > 280 120 -44  180 > son valores erroneos

    # punto 4 | D)  tablaFrecuencias(filtroMue["Sitio"])
    #           >   existen 2 valores del tipo erroneo , 8 y 28 , como son invalidos en la consulta fueron filtrados a NA
    #           filtroMue[filtroMue$errorSitio==TRUE,"Sitio"]
    #           >   se remplazo el valor erroneo por NA
    #
    
    # punto 5 | E)  tablaFrecuencias(filtroMue["Autos"]), 1 outlier de 2680 > podria ser si vive en un lugar de estacionamiento
    #           tablaFrecuencias(filtroMue["Temperatura"]), 2 outlier > 94 y 131. imposible salvo farenheit
    #           tablaFrecuencias(filtroMue["Cigarrillos"]). 2 outlier > 75 y 100 cigarrillos es demasiadisimo. y un area gris entre 20 y 30
    #
    
    # punto 6 | F)  filtroMue <- filtroMonoVar(muestra)
    #           en la muestra obtenida en filtroMue, se agregan columnas que señalan cuales son outliers y errores.
    #           en el caso de errores se remplaza el valor por NA
    #           en el caso de no estar del todo seguro simplemente se dejo como valor outlier.
    #           
    #
    
    # punto 7 | G)  Para cuales de las variables tiene sentido calcular la media? Y la mediana?
    #           ambos para variables numericas no categoricas, discretas o continuas.
    #           la mediana para cuando tenemos variables con outliers que no son errores, 
    #           pero alteran el comportamiento de la muestra
    #           La media se podria usar para la  todos salvo los sensibles.
    #           ej de utilizaciond el filtro aplicado atneriomente:
    #           de esta forma se excluyen los valores que perjudican las opereciones sobre temperatura.
    #           summary(filtroMue[filtroMue$outlierTemperatura==FALSE,"Temperatura"])

    # punto 8 | H)  En algún caso utilizaría la media alfa-podada? Con qué valor de alfa?. Justifique
    #           ambos para variables numericas no categoricas, discretas o continuas.
    #           
    #           
    #           (TODO) 
    #           
    #           
    
    
    # punto 9 | I)  ¿Cuáles de las variables le parecen simétricas a partir de estos resúmenes?. 
    #           Confirme estas observaciones mediante un boxplot.
    #           
    #           
    #           (TODO) 
    #           
    #           
    
    # punto 10 | J)  Calcular la desviación intercuartil y detectar presencia de valores salvajes moderados y severos
    #           resumenBoxPlotSalvajes(filtroMue[,"Autos"]) > con outliers
    #           resumenBoxPlotSalvajes(filtroMue[filtroMue$outlierAutos==FALSE,"Autos"]) > sin outliers
    #           para ahorrar tiempo usar: aplicarPunto10ColsAPlicables(filtroMue)
    #           si caben dudas usar > boxplot(filtroMue[,"Cigarrillos"]) o tabla de frecuencias.
    
}
