library(tidyverse)
#Funcion ejercicio 1:
#Recibe: número de transacciones del día, una ventana y una alerta.
#Devuelve: fecha, número de transacciones, la media, la desviación estándar, y la alerta si existe.

alerta.movil <- function(df, ventana, k.alerta){
  dias <- df %>% 
    summarise(n.tx = n(), .by = fecha)
  n <- nrow(dias)
  dias$media <- NA
  dias$desv <- NA
  dias$alerta <- rep(FALSE, n)
  
  
  for (i in (ventana + 1):n){
    anteriores <- dias$n.tx[(i-ventana):(i-1)]
    dias$media[i] <- mean(anteriores)
    dias$desv[i] <- sd(anteriores)
  }
  
  dias$alerta[(ventana+1):n] <- dias$n.tx[(ventana+1):n] > (dias$media[(ventana+1):n] + (k.alerta*dias$desv[(ventana+1):n]))
  
  return(dias)
}


