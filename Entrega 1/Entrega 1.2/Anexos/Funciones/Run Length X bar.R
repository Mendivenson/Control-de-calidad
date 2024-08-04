RunLengthXMod_Optimized = function(mu1 = 0, sigma1 = 1,               # Parámetros estimados del proceso
                                   mu = 0, sigma = 1,                 # Parámetros reales del proceso
                                   L = 3, Corrimiento = 0,
                                   n = 3, m = 1000){
  # Note que si los parámetros estimados del proceso son iguales a los parámetros
  # reales del proceso podremos calcular los ARLs teóricos de la carta.
  
  # Límites de control
  UCL = mu1 + L * sigma1/sqrt(n)             # Tenga en cuenta que sigma1 = Rango/d2 por lo que 
  LCL = mu1 - L * sigma1/sqrt(n)             # L/sqrt(n) * sigma1 = A_2 * R. Además, si quisieramos usar
  # otro tamaño de muestra necesitaríamos cambiar a d2.
  
  medias = c()                               # Medias de los subgrupos racionales
  RL = c()                                   # Muestras de longitud de corrida
  last_count = 0                             # Place holder ¿Cuántas muestras quedaron al final de las medias sin salirse de la zona de control? 
  # Corrimiento
  muCorr = mu + Corrimiento * sigma          # Media del proceso con corrimientos
  
  
  while (length(RL) < m){
    # Se calculan n*m*100 medias y se revisa qué medias quedaron fuera de control
    medias = colMeans(matrix(rnorm(n*m, mean = muCorr, sigma), 
                             nrow = n, byrow = F))
    
    # Se consiguen los índices de las medias que quedaron fuera de control (i.e Tiempo)
    OutControl = which(medias < LCL | medias > UCL)
    
    # Auxiliar para arreglar el problema
    # aux = length(medias) - OutControl[length(OutControl)]
    
    # Se calcula el tiempo real (Como reiniciar el proceso después de una señal)
    OutControl = diff(c(0, OutControl))
    
    # Al final del vector van a quedar z cantidad de muestras que no dieron señal
    # y eso es lo que lleva la variable de last_count
    OutControl[1] = OutControl[1] + last_count
    
    # El último error estaba en esta línea, la asignación se estaba haciendo de forma errónea
    # last_count =  length(medias) - OutControl[length(OutControl)] 
    
    last_count = length(medias) - sum(OutControl)
    
    RL = c(RL, OutControl[1:length(OutControl)])
  }
  return(RL[1:m])
}

# NOTA: La función anterior era muy rápida, pero ocupaba demasiada memoria y es posible quedarse sin memoria, 
#       en teoría esta función debería ocupar menos memoria.
