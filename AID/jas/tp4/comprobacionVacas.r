# este archivo fue armado para ver como armaba esto en R 
# tambien para corroborar numeros con la parte teorica de las vacas
probandoChi <- function(){
    
    #levantando el set de datos
    vacas <- matrix(
    c(
    c(8, 7, 12),
    c(6, 37, 36),
    c(37, 148, 351)),nrow=3,ncol=3)
    # en realidad lo levante alrevez, por lotanto hago la transpuesta
    # ya que estoy lo armo dataframe para ponerle nombre a las columnas y filas
    vacas <- data.frame(t(vacas))
    #pongo nombres a las columnas
    colnames(vacas) <- c('ninguna','ligera','notable')
    #pongo nombres a las filas
    rownames(vacas) <-c('leve','intermedio','grave')
    
    # muestro mensaje
    cat('\n muestra \n')
    
    print(vacas)

    # muestro mensaje
    #calculo grados de libertad
    #      3-1=2 * 3-1 = 2 -> 2*2 -> 4
    #seteando el 5%  nivel de significacia
    signi <- 0.05
    signiEnProporcion <- signi*100
    
    #averiguando dimensiones
    dimesiones <- dim(vacas)
    # averiguando grados de libertad
    gradosLibertad <- (dimesiones[1]-1) * (dimesiones[2]-1)
    
    # imprimo mensaje
    cat('\n chi para gl=',gradosLibertad,' y alfa ',signiEnProporcion,'% \n')
    
    
    #para chi lo lee alrevez
    signiCHi <- 1-signi
    # imprimo resultado de chi de h0
    print(qchisq(signiCHi, df=gradosLibertad))
    # corro el test para la muestra
    chiVac <- chisq.test(vacas)
    # muestro mensaje
    cat('\n observaciones \n')
    print(chiVac$observed)
    # muestro mensaje
    cat('\n esperado \n')
    print(chiVac$expected)
    # muestro mensaje
    cat('\n residuos \n')

    print(chiVac$residuals)
    # muestro mensaje
    cat('\n resumen \n')

    print(chiVac)
}