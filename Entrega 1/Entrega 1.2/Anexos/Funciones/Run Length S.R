RunLengthS_optimized = function(mu = 0, sigma = 1, k = 0,n = 3, m = 1000){
  # Constante c4 para corrección de sesgo
  c4 = sqrt(2/(n-1)) * gamma(n/2) / gamma((n-1)/2)
  LCL = sigma * (c4 - 3 * sqrt(1 - c4**2))
  UCL = sigma * (c4 + 3 * sqrt(1 - c4**2))
  
  RL= c()
  last_count = 0
  while (length(RL) < m){
    desviaciones = matrixStats::colSds(matrix(rnorm(n*m*100, 
                                                   mean = mu,
                                                   sd = sigma * (1 - k)),
                                             nrow = n, byrow = F))
    OutControl = which(desviaciones < LCL | desviaciones > UCL)
    OutControl = diff(c(0, OutControl))
    OutControl[1] = OutControl[1] + last_count
    last_count = length(desviaciones) - sum(OutControl)
    RL = c(RL,OutControl)}
  return(RL[1:m])}
