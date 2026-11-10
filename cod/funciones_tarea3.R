library(tidyverse)
#Función ejercicio 1:
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




##Funciones ejercicio 2:
#Función 1:
#Recibe: vector con los montos.
#Devuelve: la desviación media absoluta (MAD).
benford.mad <- function(x){
  digito <- 1:9
  prop.benford <- log10(1 + 1/digito)
  primer.digito <- floor(x / 10^floor(log10(x)))
  prop.observada <- as.numeric(table(factor(primer.digito, levels = digito))) / length(x)
  mad <- sum(abs(prop.observada - prop.benford)) / 9
  
  return(mad)
}

#Función 2:
#Recibe: data, un canal y la información de los cantones.
#Devuelve: una tabla con la informacion de los cantones, ordenada por el mad.
benford.canton <- function(df, canal, cantones){
  
  df.benford <- df %>% 
    filter(.data$canal == .env$canal) %>% 
    group_by(cod_canton) %>% 
    summarise(
      n = n(),
      mad = benford.mad(monto_crc),
      .groups = "drop"
    ) %>% 
    left_join(cantones, by = "cod_canton") %>% 
    mutate(
      clase = case_when(
        mad <= 0.006 ~ "conformidad cercana",
        mad <= 0.012 ~ "conformidad aceptable",
        mad <= 0.015 ~ "conformidad marginal",
        .default = "no conformidad"
      )
    ) %>% 
    select(cod_canton, canton, n, mad, clase) %>% 
    arrange(desc(mad))
  
  return(df.benford)
}







