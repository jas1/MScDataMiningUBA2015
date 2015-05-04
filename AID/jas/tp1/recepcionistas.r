# ESTE SCRIPT ESTA EN DESARROLLO , TODAVIA NO ESTA TERMINADO Y PUEDE TENER ERRORES


# depende del WS , tiene que tener el archivo de recepcionistas en el WS
# devuelve el data table de recepcionistas
iniciarRecepcionistas <- function(workDir){

    #iniciar el working dir
    setwd(workDir)
    
    # eliminar error java home en windows de x64
    if (Sys.getenv("JAVA_HOME")!="")
        Sys.setenv(JAVA_HOME="")
    
    # para inistalar libs, descomentar si no estan instaladas
    #install.packages("ggplot2")
    #install.packages("xlsx")

    #install.packages("tidyr")
    #install.packages("dplyr")
    #install.packages("knitr")
    library(tidyr)
    library(dplyr)
    library(knitr)
    
    # levanta librerias
    library(xlsx)
    library("ggplot2")
    
    # si explota por algo de no columnas, revisar o de version, guardar devuelta el excel con una version mas actual
    recepcionistas <- read.xlsx("recepcionistas.xls", header=TRUE,sheetName = "Hoja1")
    
    
}

# para normalizar el set de recepcionistas
normalizarRecepcionistas<- function(muestra){
    #filtro nombres
    filtroSinNombres <- c('cord.juez.1', 'pres.juez1', 'idiom.juez1','cord.juez.2', 'pres.juez2', 'idiom.juez2')
    colNombres <- c('candidatos')
    norm <- normalizarDatos(muestra,filtroSinNombres)
    soloNombres <- muestra[,colNombres]
    
    norm$candidatos <- soloNombres    
    resultado <- norm
}

## recibe toda la muestra, y devuelve toda la muestra pero con los valores normalizados
normalizarDatos<- function(muestra, filtro = c()){

    x <- muestra
    # campos filtrados
    if(length(filtro) > 0){
        x <- muestra[,filtro]    
    }

    # normalizado
    normalized <- (x-min(x))/(max(x)-min(x))
    
    normalized
}

# dibuja Con ggplot
dibujar <- function (muestra){
    ggplot(muestra, aes(x=candidatos, y=cord.juez.1)) + geom_line() + expand_limits(y=0)
}

# transpone la muestra, mueve los nombres como titulos de columnas, y transforma los valores a numericos
aspirantesTranspuestas <- function (muestra){
    transp <- t(muestra)
    colnames(transp) <- transp[1,]
    transp <-  transp[-1,]
    class(transp) <- "numeric"
    
    tmp <- data.frame(transp)
    tmp
}

# ej1:comment
#a- Calcule en promedio por juez de cada una de las aspirantes. 
#   Cual le parece que seleccionaría cada uno de ellos. Existe coincidencia?.
promedioJuezPorAspirante <- function(muestra){
  filtro <- c('cord.juez.1', 'pres.juez1', 'idiom.juez1')
  filtroJuez2 <- c('cord.juez.2', 'pres.juez2', 'idiom.juez2')
  
  filtro <- c('cord.juez.1', 'pres.juez1', 'idiom.juez1')
  filtro2 <- c('cord.juez.2', 'pres.juez2', 'idiom.juez2')
  muestra$promJuez1<- round(rowMeans(muestra[,filtro]),digits = 2)
  muestra$promJuez2<- round(rowMeans(muestra[,filtro2]),digits = 2)
 
  muestra[,c("candidatos", "promJuez1","promJuez2")]
 }
# juez 1 va a elegir a Mariana, 
# juez 2 va a elegir a maia
# apenas ambos piensan que maia tiene muy buena presencia, mientras qu juez 2 no tanto de mariana.


#b- Calcule el promedio de cada una de las aspirantes tomando en cuenta todos los rubros y ambos jueces.

#esta funcion debe recibir las aspirantes transpuestas
# promedioTotalPorAspirante(aspirantesTranspuestas(muestra))["total",]
promedioTotalPorAspirante <- function(muestra){
    muestra["total",]  <-round( colSums(muestra)/6 , digits = 2)
    muestra
}
# segun los promedios generales, se puede ver que la que va a ganar es Mariana, seguida de Maia

#c- Transformar las puntuaciones observadas de modo tal que cada una seis variables tenga media 0 y dispersión 1. 
#   Cuál sería el objetivo de esta transformación?
# el objetivo de la transformacion seria normalizar la muestra, lo que implica llevar a la misma escala a todos , 
# eliminando desvios de percepcin de escala; tambien normalizar ayuda a tener mas herramientas estadisticas aplicables.
# promedioTotalPorAspirante(normalizarDatos(aspirantesTranspuestas(muestra)))
# para comparar con el sin normalizar
# promedioTotalPorAspirante(aspirantesTranspuestas(muestra))

#d- Transformar las puntuaciones de modo tal que cada candidata tenga para cada juez media 0 y dispersión 1. 
# Cuál sería el objetivo de esta transformación?.
# ver los totales por juez por candidata. 
# para lograr esto tenemos que depurar la muestra a numeros unicamente los nomberes seran los nombres de las filas,
# luego tenemos que sacar el promedio por variable por juez
# y por ultimo normalizar 
transformarAcandidatasNumerico <- function(muestra){
    row.names (muestra) <- muestra[,1]
    muestra <-  muestra[,-1]
    #class(muestra) <- "double"
    
    tmp <- data.frame(muestra)
    tmp  
}


normalizarPuntuacioenesSinFiltros <- function(muestra){
    #transformacion nombres filas
    row.names (muestra) <- muestra[,1]
    muestra <-  muestra[,-1]

    tmp <- data.frame(muestra)
    
    # normalizar los datos
    norm <- normalizarDatos(tmp  )
    
    # mostrar solo promedios
    norm
}


normalizarPuntuacioenesPorJuez <- function(muestra){
    #transformacion nombres filas
    row.names (muestra) <- muestra[,1]
    muestra <-  muestra[,-1]

    # promedio variables por juez
    filtro <- c('cord.juez.1', 'pres.juez1', 'idiom.juez1')
    filtro2 <- c('cord.juez.2', 'pres.juez2', 'idiom.juez2')
    muestra$promJuez1<- rowMeans(muestra[,filtro])
    muestra$promJuez2<- rowMeans(muestra[,filtro2])
    tmp <- data.frame(muestra)
    
    # normalizar los datos
    norm <- normalizarDatos(tmp  )
    
    # mostrar solo promedios
    norm[ , c("promJuez1",  "promJuez2")]
}
# luego de correr el test de normaliza rpuntuaciones por juez, se puede ver que el juez 2 tiende a ser mas pesimista.



#e- Grafique los perfiles multivariados de cada una de las candidatas para ambas transformaciones. Qué observa?
# e1. perfiles segun transformacion D
perfilesSegunD <- function(m){
    muestra <- normalizarPuntuacioenesPorJuez(m)
    muestra$nombres <- row.names (muestra)

    #acomodar la muestra para que encaje con gg plot
    
    #me tiene que quedar
    #col1) tipo juez
    #col2) aspirante
    #col3) valor
    muestra <- muestra %>% gather(juez, puntajes, -nombres) 
    # normalizar nombres columnas
    

    plot <- ggplot(muestra, aes(x=nombres, y=puntajes, group=juez,colour=juez)) + geom_line() + geom_point(size=4) 
    plot
}

# e2. perfiles segun transformacion C
# se puede especificar por juez o atributo.
# si  juez por default es sin filtro, validos: 1 y 2 
# si es attr hay que explicitar la llamada al param filtroAttr, validos: pres,cord,idiom, por default sin filtro
# si vienen fuera de rango, devuelve como sin filtro.
perfilesSegunC <- function(m, filtroJuez = 0,filtroAttr = ""){
    #acomodar la muestra para que encaje con gg plot
    
    #me tiene que quedar
    #col1) tipo juez
    #col2) aspirante
    #col3) valor
    
    # normalizar nombres columnas
    muestra <- muestra %>% 
                    rename(cord.juez1=cord.juez.1, cord.juez2=cord.juez.2) %>% 
                    gather(atributo, puntajes, -candidatos) 
    # %>% separate(atributo, into = c("atributo", "juez"), sep = "\\.")
    
    muestra$norm <- normalizarDatos(muestra$puntajes)
    # hago el merge de columnas
   # muestra <- muestra %>% gather(atributo, puntajes, -nombres) 
    
    # separo los atributos del juez por el separador punto ".", escapeado con \\
   # muestra <- muestra %>% separate(atributo, into = c("atributo", "juez"), sep = "\\.")

   ploteable <- muestra
   
   # filtro par abuscar por juez especificamente., si no se determina o pone 0 sale todo
   if(filtroJuez==1){
       ploteable <- ploteable[grep("juez1", muestra$atributo),]
   }else{
       if(filtroJuez==2){
           ploteable <- ploteable[grep("juez2", muestra$atributo),]
       }
   } 
   
   # filtro par abuscar para comparar vars de jueces explicitar filtroAttr en la llamada
   # perfilesSegunC(muestra,filtroAttr = "pres")
   if(filtroAttr == "cord"){
       ploteable <- ploteable[grep("cord", ploteable$atributo),]
   }else {
       if(filtroAttr=="pres"){
           ploteable <- ploteable[grep("pres", ploteable$atributo),]
       }else{
           if(filtroAttr=="idiom"){
               ploteable <- ploteable[grep("idiom", ploteable$atributo),]
           }
       }
   }
   
   # al fin salio el ggplot :D !
    plot <- ggplot(ploteable, aes(x=candidatos, y=norm, group=atributo,colour=atributo)) + geom_line() + geom_point(size=4) 
    plot
}