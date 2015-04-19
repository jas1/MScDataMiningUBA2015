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
#  categorica 					ID: número de identificación del registro. 
  # no hace falta
  
#  categorica					Nacionalidad
  # no hace falta > podria depurarlo
  
#  cuantitativa discreta		Edad: cumplida en años
  #  hace falta > depurar rangos entre  5 y 80 > agregar flag de rompe Campo Edad 
  if(muestra[,'Edad'] >80 && muestra[,'Edad'] < 5 ){
    muestra$outlierEdad <-1
  }else{
    muestra$outlierEdad<-0
  }
  
  
#  categorica					Sexo: Masculino (1) Femenino (2)
  #  hace falta > depurar si 1 o 2, marcar como campo con error en campo sexo, y poner NA
  if(muestra$Sexo != 1 && muestra$Sexo != 2 ){
    muestra$tieneErrorSexo <- TRUE
    muestra$Sexo <- NA
  }else{
    muestra$tieneErrorSexo<- FALSE
  }
#  cuantitativa				Estatura: en m.
  #  hace falta > marcar si va a esta fuera de 0.5 y 2.50
#  categorica					Sitio: sitio preferido al que se conecta; 1- chat\ 2 - correo electrónico\ 3- buscadores\ 4 – software\ 5 – música\ 6 – deportes \ 7 - otros
  
  #  hace falta > marcar si va a esta fuera de  1...7
#  cuantitativa continua		Uso: Tiempo de uso promedio por día en minutos
  #  hace falta > marcar si va a esta fuera de  1...7
  
#  cuantitativa continua		Temperatura: media anual de la zona de residencia
  # hace falta >  si esta fuera de -10 a 40
  
#  cuantitativa discreta		Autos: cantidad de autos en la manzana donde reside
  # hace falta >  si esta fuera de 0 a 50
  
#  cuantitativa discreta		Cigarrillos: Cantidad de cigarrillos consumidos mientras utiliza Internet
  # hace falta >  si esta fuera de 0 a 20
# devuelve muestra
  muestra
}

reglaCampo1 <- function (){
  
}

