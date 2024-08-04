RunLengthR_optimized = function(mu = 0, sigma = 1, k= 0,n = 3, m = 1000){
  #matrixStats: Librería para operaciones rowDiff y rowRange más eficientes (apply es muy lento)
  # Constantes d3 y d2 para carta R
  d3 = c(0.853, 0.888, 0.880, 0.864, 0.848, 0.833, 0.820, 0.808, 0.797, 0.787, 0.778, 0.770,
         0.763, 0.756, 0.750, 0.744, 0.739, 0.734, 0.729, 0.724, 0.72, 0.716, 0.712,0.708)
  d2 = c(1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.258, 3.336, 
         3.407, 3.472, 3.532, 3.588, 3.640, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931)
  
  d3 = d3[n-1]; d2 = d2[n-1]    # Selección de constantes específicas dado el tamaño de muestra
  UCL = (d2 + 3 * d3) * sigma;  # Límite superior para carta R
  LCL = (d2 - 3 * d3) * sigma   # Límite inferior para carta R

  RL = c();                     # Vector para guardar las longitudes de corrida
  last_count = 0                # Z longitudes de corrida sin señal al final de la matriz
  
  while(length(RL) < m){
    # Se generan m*100 subgrupos racionales, se toma el rango y la diferencia
    rangos = matrixStats::rowDiffs(matrixStats::colRanges(matrix(rnorm(n*m*100, 
                                             mean = mu,
                                             sd = sigma * (1 - k)),
                                       nrow = n, byrow = F)))
    # ¿Qué índices se salieron de control?
    OutControl = which(rangos < LCL | rangos > UCL)
    # ¿Cuánto tiempo hay entre estos índices?
    OutControl = diff(c(0, OutControl))
    # Se suman al primer RL la cantidad de subgrupos racionales sin señal 
    # del final del ciclo anterior
    OutControl[1] = OutControl[1] + last_count
    # Se actualiza a z subgrupos racionales al final de este ciclo sin dar señal
    last_count = length(rangos) - sum(OutControl)
    
    # Se agregan los resultados al final del vector de longitudes de corrida
    RL = c(RL,OutControl)}
  return(RL[1:m])}
