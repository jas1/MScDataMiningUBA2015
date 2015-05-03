# DM - Taller 1
#tarjetas de credito
#ANALISIS DE DATOS PARA UNA TARJETA DE CREDITO
# Un Banco dispone de los siguientes datos referidos a los clientes de una tarjeta de crédito sumarizados a
# partir de las transacciones del último año.
# 1. Id cliente
# 2. Frecuencia de uso de la tarjeta medida como días en promedio entre transacciones
# 3. Saldo mensual promedio en $$
# 4. Cupón promedio : Monto promedio por transacción
# 5. Posesión de Tarjeta Gold: 0 No - 1 Si
# 6. Cantidad de débitos automáticos adheridos a la tarjeta : 1,2,3 4 o más
# 7. Estado Civil : 1 Soltero, 2 Casado, 3 Viudo-Divorciado
# 8. Sexo Titular : 1 Masculino – 2 Femenino
# 9. Ocupación Titular : 1 Relación dependencia, 2 Cuenta propia, 3 No Trabaja/No informado
# 10. Edad en años
# 11. Hijos 0 No – 1 Si
# 12. Ingreso en $$ del grupo familiar
# La venta de tarjeta Gold se realizó con una campaña de publicidad masiva.
# Se quiere estudiar el perfil transaccional y sociodemográfico de los clientes y decidir cuáles son las
# características del grupo que resultó más interesado en la compra de tarjeta Gold para hacer campañas de
# marketing directo eligiendo clientes con características similares.
# Tarea.
# Se trata de analizar los datos disponibles utilizando una hoja Excel para proporcionar respuesta a las
# cuestiones de negocio que se plantean en la descripción del problema.
#
# Preguntas de negocio.
# Si se quiere realizar una nueva campaña para venta de tarjeta Gold, cuáles son las características de los
# clientes que ya la compraron ?.
# Sexo, Estado Civil, Tenencia de hijos, Ingreso promedio, etc.
# ¿En qué se diferencian de los No compradores?
# ¿Cuántos clientes encuentra en la base que NO tienen tarjeta Gold pero sus características son similares a
# los que ya la compraron?

# si bien el TP restrinje las lherramientas a una hoja excel , lo hago en R a modo práctica.
ejecuarTarjetas<-function(){
    # prerequisitos
    # para eliminar error de java
    solucionJava()
    
    # carga librerias
    #libreriasName <- c('xlsx','ggplot2','tidyr','dplyr','knitr','reshape2','GGally')
    #armaFuncionReqs(libreriasName,FALSE)
    checkLibs()
    
    # setear WD
    setwd('C:/Users/julio/Dropbox/julio_box/educacion/maestria_explotacion_datos_uba/materias/cuat_1_data_mining/PreDatamining/wd_tp_dm')
    
    
    # TP
    enunciado();
    
    # levantar datos
    # como es pesado levantar los datos lo levanto 1 vez en el WS.
    #muestra <- levantaDatos()
    
    #punto a
    # Si se quiere realizar una nueva campaña para venta de tarjeta Gold, cuáles son las características de los
    # clientes que ya la compraron ?.
    puntoA(muestra)
    
    
}

# hace un resumen de las columnas seleccionadas, de la muestra
# si no se especifica columnas se toma:
# 'gold','saldoMensualPromedio','ingresoFamiliar','categoriaSal','estadoCivilS','sexoS','hijos','ocupacionS'
resumenMuestraPorFila <- function(muestra,columnas = c('')){
    columnasFilter <- c('gold','saldoMensualPromedio','ingresoFamiliar','categoriaSal','categoriaEdad','estadoCivilS','sexoS','hijos','ocupacionS')
    if(columnas[1] != ''){
        columnasFilter<-columnas
    }
    filtradosDF <- as.data.frame(muestra[,columnasFilter])
    summary(filtradosDF)
}

puntoC<- function(muestra){
    
    cat(' ¿Cuántos clientes encuentra en la base que NO tienen tarjeta Gold pero sus características son similares a los que ya la compraron?\n')
    
    #haciendo el mismo analsisi de casado y otras variables como el aplicado al grupo de los que ya son clientes:
    
    #ingerso B(1500..5000):296         		> 296/3028~ 9.7% > y casado
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$categoriaSal == 'B(1500..5000)' & muestra$estadoCivil == 2,])
    
    #hijos SI :857 					> 857/3028 ~ 28.3% > y casado
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$hijos == 1 & muestra$estadoCivil == 2,])
    
    #Casado(2):217 					> 1249/3028 ~41%
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$estadoCivil == 2 ,] )
    
    #Masculino(1):684 				> 684/3028 ~ 22.58% > y casado
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$sexo == 1 & muestra$estadoCivil == 2,])
    
    #Ocupacion: Cuenta propia(2):639	> 639/3028 ~ 21.10% > y casado
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$ocupacion == 2 & muestra$estadoCivil == 2,])
    
    #categoriaEdad Adulto(30-50):829 > 829/3028~27.37781% > y casado
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$estadoCivil == 2 & muestra$categoriaEdad == 'Adulto' ,])
    
    
    #siendo mas detallistas podemos ver que la muestra no gold, tiene mucho publico de la categoria C de ingresos
    #sepuede buscar quienes de categoria C tuvieron exito en el grupo que ya tiene la tarjeta
    #
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$categoriaSal == 'C(<1500)' ,]) (2542/3028 ~83%)
    #vs
    #resumenMuestraPorFila(muestra[muestra$gold == 1 & muestra$categoriaSal == 'C(<1500)' ,]) (84/217 ~38%)
    #
    #una tarea posible a realizar es multiperfiles
    #no un unico perfil , sino armar varios perfiles y luego sumar los totales de lo comprendido.

}

puntoB<- function(muestra){
    cat(' ¿En qué se diferencian de los No compradores?\n')
    filtrados <- muestra[muestra$gold==0,]
    resumenMuestraPorFila(filtrados,columnasFilter)
    #RESUEMN NO GOLD
    # gold     saldoMensualPromedio ingresoFamiliar         categoriaSal  categoriaEdad              estadoCivilS           sexoS     
    # 0:3028   Min.   :    0.0      Min.   : 500.0   A(>5000)     :  72   Adulto:1640   Casado(2)          :1249   Femenino(2) :1553  
    # 1:   0   1st Qu.:  145.0      1st Qu.: 837.8   B(1500..5000): 414   Joven :1379   Soltero(1)         :1610   Masculino(1):1475  
    #          Median :  366.5      Median : 981.0   C(<1500)     :2542   Mayor :   9   Viudo-Divorciado(3): 169                      
    #          Mean   :  873.0      Mean   :2022.4                                                                                    
    #          3rd Qu.: 1021.2      3rd Qu.:2177.2                                                                                    
    #          Max.   :18016.0      Max.   :9988.0                                                                                    
    # hijos                         ocupacionS  
    # 0:2014   Cuenta propia(2)          :1268  
    # 1:1014   No Trabaja/No informado(3): 198  
    #          Relación dependencia(1)   :1562 
    
    # se observa mayor cantidad de estado civil soltero
    # se observa mayor cantidad de categoria salarial del tipo C(<1500)
    # se observa menor cantidad con hijos
    
    #Aplicando algunos filtros a los posibles candidatos: ( 3028 )
    #casados : 
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$estadoCivil == 2 ,] )
    #1249  > ~41%
    #
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$estadoCivil == 2 & muestra$categoriaEdad == 'Adulto' ,])
    #casados y adultos
    #829 > ~27%
    #
    #Solo adultos (30 a 50)
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$categoriaEdad == 'Adulto' ,])
    #1640 > ~54%
    #
    #hijos:
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$hijos == 1 ,])
    #1014 > ~ 33%
    #
    #hombres
    #resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$sexo == 1 ,])
    #1475 > ~ 48%
    
}

puntoA <- function(muestra){
    filtrados <- muestra[muestra$gold==1,]
    resumenMuestraPorFila(filtrados,columnasFilter)
    
    #
    #RESUMEN GOLD:
    # gold    saldoMensualPromedio ingresoFamiliar        categoriaSal categoriaEdad              estadoCivilS          sexoS    
    # 0:  0   Min.   :   35        Min.   : 511    A(>5000)     : 12   Adulto:202    Casado(2)          :217   Femenino(2) : 62  
    # 1:217   1st Qu.: 1088        1st Qu.:1233    B(1500..5000):121   Joven : 15    Soltero(1)         :  0   Masculino(1):155  
    #         Median : 1760        Median :3229    C(<1500)     : 84   Mayor :  0    Viudo-Divorciado(3):  0                     
    #         Mean   : 2185        Mean   :4205                                                                                  
    #         3rd Qu.: 2685        3rd Qu.:7082                                                                                  
    #         Max.   :14862        Max.   :9981                                                                                  
    # hijos                        ocupacionS 
    # 0: 67   Cuenta propia(2)          :151  
    # 1:150   No Trabaja/No informado(3):  6  
    #         Relación dependencia(1)   : 60  
    #
    #viendo el resumen de los clientes las caracteristicas son: 
    #total compradores: 217
    #( calculado a mano # / # muestra * 100 y trimmed en 2 dec)
    #ingerso B(1500..5000):121     			> ~ 55% > y casado
    #hijos SI :150 					> ~ 69% > y casado
    #Casado(2):217 					> ~ 100% > todos los gold son casados! el resto de las variables implica que estan casados
    #Masculino(1):155 				> ~ 71% > y casado
    #Ocupacion: Cuenta propia(2):151	> ~ 69% > y casado
    #categoriaEdad Adulto(30-50):202 > ~ 93% > y casado
    #
    #empezando a combinar los grupos de mayor ocurrencia:
    #sobre los ya existentes tenemos un resultado de: 
    #
    #categoriaEdad Adulto + estadoCivil Casado(2) > 202/217 > ~ 93%
    #todos los casados , son adultos
    #resumenMuestraPorFila(muestra[muestra$gold == 1 & muestra$categoriaEdad == 'Adulto' & muestra$estadoCivil == 2 ,])
    #
    #categoriaEdad Adulto + Masculino(1):149 > 149/217 > ~ 71%
    #todos los hombers , son adultos
    #resumenMuestraPorFila(muestra[muestra$gold == 1 & muestra$categoriaEdad == 'Adulto' & muestra$sexo == 1 ,])
    #
    #categoriaEdad Adulto + con hijos:141 > 141/217 > ~ 64%
    #resumenMuestraPorFila(muestra[muestra$gold == 1 & muestra$categoriaEdad == 'Adulto' & muestra$hijos == 1 ,])
    #
    #para probar sobre la otra muestra, de basico entrar con que sean casados.
    #resumenMuestraPorFila(muestra[muestra$gold == 1 & muestra$estadoCivil == 2 ,])


        
    
}

filtrarPerfilesCandidatos <- function (muestra){
    # no gold, con hijos, con casado = 2 , que no sean mayores y ganen enter 1500 y 5000
    perf1 <- muestra[muestra$gold == 0 & muestra$hijos != 0 & muestra$estadoCivil == 2 &  muestra$categoriaEdad != 'Mayor' & muestra$categoriaSal=='B(1500..5000)',]
    
    # no gold, con hijos, con casado = 2 y adulto ( entre 30 y 50)
    perf2 <- muestra[muestra$gold == 0 & muestra$hijos != 0 & muestra$estadoCivil == 2 ,]
    
    # no gold, con hijos, con casado = 2 , que no sean mayores y ganen enter 1500 y 5000
    perf3 <- muestra[muestra$gold == 0 & muestra$hijos != 0 & muestra$estadoCivil == 2 &  muestra$categoriaEdad != 'Mayor' & muestra$categoriaSal=='B(1500..5000)',]
    resumenMuestraPorFila(muestra[muestra$gold == 0 & muestra$categoriaEdad == 'Adulto' & muestra$estadoCivil == 2 & muestra$hijos != 0,])
}

levantaDatos <- function(){
    # levantar datos:
    muestra <- read.xlsx("CreditCard_ori.xls", header=TRUE,sheetIndex = 1)
    # aprovecho para emprolijar nombres
    nombreCOlumnas<- c('id','frec','saldoMensualPromedio','cuponPromedio','gold','debitos','estadoCivil','sexo','hijos','edad','ocupacion','ingresoFamiliar','columnaQueSobra')
    names(muestra)<- nombreCOlumnas
    
    # aprovecho para emprolijar Valores
    # Posesión de Tarjeta Gold: 0 No - 1 Si
    muestra$gold[muestra$gold == 1] <- TRUE # es 1
    muestra$gold[muestra$gold == 0] <- FALSE # es 1
    # como categorica
    muestra$gold <- as.factor(muestra$gold)
    
    # Hijos 0 No – 1 Si
    muestra$hijos[muestra$hijos == 1] <- TRUE # es 1
    muestra$hijos[muestra$hijos == 0] <- FALSE # es 1
    # como categorica
    muestra$hijos <- as.factor(muestra$hijos)
    
    # como puede ser que sea mas facil filtrar sin moner el nobre , sino los codigos, agrego columnas.
    #1 Masculino – 2 Femenino
    muestra$sexoS[muestra$sexo == 1] <- 'Masculino(1)' # es 1
    muestra$sexoS[muestra$sexo == 2] <- 'Femenino(2)' # es 1
    # como categorica
    muestra$sexoS <- as.factor(muestra$sexoS)
    muestra$sexo <- as.factor(muestra$sexo)
    
    # Estado Civil : 1 Soltero, 2 Casado, 3 Viudo-Divorciado:
    muestra$estadoCivilS[muestra$estadoCivil == 1] <- 'Soltero(1)' # es 1
    muestra$estadoCivilS[muestra$estadoCivil == 2] <- 'Casado(2)' # es 1
    muestra$estadoCivilS[muestra$estadoCivil == 3] <- 'Viudo-Divorciado(3)' # es 1
    # como categorica
    muestra$estadoCivilS <- as.factor(muestra$estadoCivilS)
    muestra$estadoCivil <- as.factor(muestra$estadoCivil)
    
    # 1 Relación dependencia, 2 Cuenta propia, 3 No Trabaja/No informado
    muestra$ocupacionS[muestra$ocupacion == 1] <- 'Relación dependencia(1)' # es 1
    muestra$ocupacionS[muestra$ocupacion == 2] <- 'Cuenta propia(2)' # es 2
    muestra$ocupacionS[muestra$ocupacion == 3] <- 'No Trabaja/No informado(3)' # es 3
    # como categorica
    muestra$ocupacionS <- as.factor(muestra$ocupacionS)
    muestra$ocupacion <- as.factor(muestra$ocupacion)
    
    # agrego categorias de dinero 
    
    muestra$categoriaSal[muestra$saldoMensualPromedio < 1500] <- 'C(<1500)' # sueldo mensual menor a 1500
    muestra$categoriaSal[muestra$saldoMensualPromedio > 5000 ] <- 'A(>5000)' # sueldo mensual menor a 5000
    muestra$categoriaSal[muestra$saldoMensualPromedio >= 1500 & muestra$saldoMensualPromedio  <= 5000 ] <- 'B(1500..5000)' # sueldo mensual mayor igual a 1500 y menor igual a 5000
    # como categorica
    muestra$categoriaSal <- as.factor(muestra$categoriaSal)
    
    # agrego categorias de edad 
    
    muestra$categoriaEdad[muestra$edad < 30] <- 'Joven' # sueldo mensual menor a 1500
    muestra$categoriaEdad[muestra$edad > 50 ] <- 'Mayor' # sueldo mensual menor a 5000
    muestra$categoriaEdad[muestra$edad >= 30 & muestra$edad  <= 50 ] <- 'Adulto' # sueldo mensual mayor igual a 1500 y menor igual a 5000
    # como categorica
    muestra$categoriaEdad <- as.factor(muestra$categoriaEdad)
    
    
    muestra
}

enunciado<-function() {
    cat('\n')
    cat('ANALISIS DE DATOS PARA UNA TARJETA DE CREDITO \n')
    cat('\n')
    cat(' Un Banco dispone de los siguientes datos referidos a los clientes de una tarjeta de crédito sumarizados a\n')
    cat(' partir de las transacciones del último año.\n')
    cat('\n')
    cat(' 1. Id cliente\n')
    cat(' 2. Frecuencia de uso de la tarjeta medida como días en promedio entre transacciones\n')
    cat(' 3. Saldo mensual promedio en $$\n')
    cat(' 4. Cupón promedio : Monto promedio por transacción\n')
    cat(' 5. Posesión de Tarjeta Gold: 0 No - 1 Si\n')
    cat(' 6. Cantidad de débitos automáticos adheridos a la tarjeta : 1,2,3 4 o más\n')
    cat(' 7. Estado Civil : 1 Soltero, 2 Casado, 3 Viudo-Divorciado\n')
    cat(' 8. Sexo Titular : 1 Masculino – 2 Femenino\n')
    cat(' 9. Ocupación Titular : 1 Relación dependencia, 2 Cuenta propia, 3 No Trabaja/No informado\n')
    cat(' 10. Edad en años\n')
    cat(' 11. Hijos 0 No – 1 Si\n')
    cat(' 12. Ingreso en $$ del grupo familiar\n')
    cat('\n')
    cat(' La venta de tarjeta Gold se realizó con una campaña de publicidad masiva.\n')
    cat(' Se quiere estudiar el perfil transaccional y sociodemográfico de los clientes y decidir cuáles son las\n')
    cat(' características del grupo que resultó más interesado en la compra de tarjeta Gold para hacer campañas de\n')
    cat(' marketing directo eligiendo clientes con características similares.\n')
    cat('\n')
    cat(' Tarea.\n')
    cat(' Se trata de analizar los datos disponibles utilizando una hoja Excel para proporcionar respuesta a las\n')
    cat(' cuestiones de negocio que se plantean en la descripción del problema.\n')
    cat(' Preguntas de negocio.\n')
    cat(' Si se quiere realizar una nueva campaña para venta de tarjeta Gold, cuáles son las características de los\n')
    cat(' clientes que ya la compraron ?.\n')
    cat(' Sexo, Estado Civil, Tenencia de hijos, Ingreso promedio, etc.\n')
    cat(' ¿En qué se diferencian de los No compradores?\n')
    cat(' ¿Cuántos clientes encuentra en la base que NO tienen tarjeta Gold pero sus características son similares a los que ya la compraron?\n')
    cat('\n')
}


# a modo practica tambien clasifico los datos que tengo
categorizacionDatos <- function(){
    # 1. Id cliente
    print('id Cliente: ordinal')
    # 2. Frecuencia de uso de la tarjeta medida como días en promedio entre transacciones
    print('Frecuencia de uso de la tarjeta medida como días en promedio entre transacciones: cuantitativa discreta: integer')
    # 3. Saldo mensual promedio en $$
    print('Saldo mensual promedio en $$: cuantitativa discreta: integer : se tomaron todos discretos en el origen de datos')
    # 4. Cupón promedio : Monto promedio por transacción
    print(' Cupón promedio : Monto promedio por transacción: cuantitativa discreta: integer : se tomaron todos discretos en el origen de datos')
    # 5. Posesión de Tarjeta Gold: 0 No - 1 Si
    print(' Posesión de Tarjeta Gold: 0 No - 1 Si: categorica: boolean : si 1 / no 0')
    # 6. Cantidad de débitos automáticos adheridos a la tarjeta : 1,2,3 4 o más
    print('Cantidad de débitos automáticos adheridos a la tarjeta : 1,2,3 4 o más:cuantitativa discreta: integer')
    # 7. Estado Civil : 1 Soltero, 2 Casado, 3 Viudo-Divorciado
    print(' Estado Civil : 1 Soltero, 2 Casado, 3 Viudo-Divorciado: categorica: String : 1 Soltero, 2 Casado, 3 Viudo-Divorciado')
    # 8. Sexo Titular : 1 Masculino – 2 Femenino
    print(' Sexo Titular : 1 Masculino – 2 Femenino: categorica: String : 1 Masculino – 2 Femenino')
    # 9. Ocupación Titular : 1 Relación dependencia, 2 Cuenta propia, 3 No Trabaja/No informado
    print(' Ocupación Titular : 1 Relación dependencia, 2 Cuenta propia, 3 No Trabaja/No informado: categorica: String : 1 Relación dependencia, 2 Cuenta propia, 3 No Trabaja/No informado')
    # 10. Edad en años
    print('  Edad en años: cuantitativa discreta: integer')
    # 11. Hijos 0 No – 1 Si
    print('Hijos 0 No – 1 Si: categorica: boolean : si 1 / no 0')
    # 12. Ingreso en $$ del grupo familiar
    print('Ingreso en $$ del grupo familiar: cuantitativa discreta: integer : se tomaron todos discretos en el origen de datos')
}

solucionJava <- function(){
    
    if (Sys.getenv("JAVA_HOME")!="")
        Sys.setenv(JAVA_HOME="")
    
    #instalarRjava <- c("rJava","xlsxjars")
    #armaFuncionReqs(instalarRjava,FALSE)
    
    # buscando paquete:  rJava 
    if(!require(rJava)) { 
        install.packages("rJava")
        require(rJava)
    }else{
        cat('ya esta instalado: rJava  ')
    }
    # buscando paquete:  xlsxjars 
    if(!require(xlsxjars)) { 
        install.packages("xlsxjars")
        require(xlsxjars)
    }else{
        cat('ya esta instalado: xlsxjars  ')
    }
    
}

checkLibs<- function(){
    # buscando paquete:  xlsx 
    if(!require(xlsx)) { 
        install.packages("xlsx")
        require(xlsx)
    }else{
        cat('no esta instalado y hay que instalar: xlsx  ')
    }
    # buscando paquete:  ggplot2 
    if(!require(ggplot2)) { 
        install.packages("ggplot2")
        require(ggplot2)
    }else{
        cat('no esta instalado y hay que instalar: ggplot2  ')
    }
    # buscando paquete:  tidyr 
    if(!require(tidyr)) { 
        install.packages("tidyr")
        require(tidyr)
    }else{
        cat('no esta instalado y hay que instalar: tidyr  ')
    }
    # buscando paquete:  dplyr 
    if(!require(dplyr)) { 
        install.packages("dplyr")
        require(dplyr)
    }else{
        cat('no esta instalado y hay que instalar: dplyr  ')
    }
    # buscando paquete:  knitr 
    if(!require(knitr)) { 
        install.packages("knitr")
        require(knitr)
    }else{
        cat('no esta instalado y hay que instalar: knitr  ')
    }
    # buscando paquete:  reshape2 
    if(!require(reshape2)) { 
        install.packages("reshape2")
        require(reshape2)
    }else{
        cat('no esta instalado y hay que instalar: reshape2  ')
    }
    # buscando paquete:  GGally 
    if(!require(GGally)) { 
        install.packages("GGally")
        require(GGally)
    }else{
        cat('no esta instalado y hay que instalar: GGally  ')
    }
}

# armaReqs
armaFuncionReqs <- function (install, conInstall){
    #install <- c("xlsx","ggplot2","tidyr","dplyr","knitr")
    
    for (ins in install) {
        cat('# buscando paquete: ',ins,'\n' )
        cat('if(!require(',ins,')) { \n',sep = "")
        cat('\tinstall.packages("',ins,'")\n',sep = "")
        cat('\trequire(',ins,')\n',sep = "") 
        cat('}else{\n') 
        cat('\tcat(\'ya esta instalado:',ins,' \')\n') 
        cat('}\n')

    }
    # intento dinamico: no funciono porque cuando hace require ins, no engancha el string, buscar como bypasear
    # for (ins in install) {
    #         if(!require(ins) && conInstall) {
    #             install.packages(as.String(ins)); 
    #             require(as.String(ins))
    #         }
    #}
    
    # vieja usanza
    #install.packages("ggplot2")
    #install.packages("xlsx")
    #install.packages("tidyr")
    #install.packages("dplyr")
    #install.packages("knitr")
    #require(tidyr)
    #require(dplyr)
    #require(knitr)
    #require(xlsx)
    #require("ggplot2")
}
