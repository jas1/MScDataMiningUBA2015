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
    
    # levanta librerias
    library(xlsx)
    library("ggplot2")
    
    # si explota por algo de no columnas, revisar o de version, guardar devuelta el excel con una version mas actual
    recepcionistas <- read.xlsx("recepcionistas.xls", header=TRUE,sheetName = "Hoja1")
    
    
}

opinionJuez1 <- function(muestra){
  filtro <- c('cord.juez.1', 'pres.juez1', 'idiom.juez1')
  resultado <- muestra[,filtro]
  resultado$juez1.media <- mean(muestra[,filtro])
}

opinionJuez2 <- function(muestra){
  filtro <- c('cord.juez.2', 'pres.juez2', 'idiom.juez2')
  resultado <- muestra[,filtro]
  mean()
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
normalizarDatos<- function(muestra, filtro){

    # campos filtrados
    x <- muestra[,filtro]
        
    # normalizado
    normalized <- (x-min(x))/(max(x)-min(x))
}

# dibuja Con ggplot
dibujar <- function (muestra){
    ggplot(muestra, aes(x=candidatos, y=cord.juez.1)) + geom_line() + expand_limits(y=0)
}
