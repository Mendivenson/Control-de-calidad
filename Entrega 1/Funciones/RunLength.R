RunLengthS = function(mu = 0, sigma = 1, CorrimientoSigma = 1,n = 3, m = 1000){
  S = c() ; c4 = sqrt(2/(n-1)) * gamma(n/2) / gamma((n-1)/2); CLs = c4 * sigma;
  LCL = sigma * (c4 - 3 * sqrt(1 - c4**2)); UCL = sigma * (c4 + 3 * sqrt(1 - c4**2))
  
  pb = txtProgressBar(min = 0, max = m, style = 3) # Barra de progreso
  
  i = 0
  while (length(S) < m){
    s = sd(rnorm(n, mu, CorrimientoSigma))
    i = i + 1
    if (s < LCL | s > UCL){
      S = c(S, i)
      i  = 0; setTxtProgressBar(pb, length(S))
    }}
  return(S)}

RunLengthR = function(mu = 0, sigma = 1, CorrimientoSigma = 1,n = 3, m = 1000){
  # Constantes carta R
  d3 = c(0.853, 0.888, 0.880, 0.864, 0.848, 0.833, 0.820, 0.808, 0.797, 0.787, 0.778, 0.770,
         0.763, 0.756, 0.750, 0.744, 0.739, 0.734, 0.729, 0.724, 0.72, 0.716, 0.712,0.708)
  d2 = c(1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.970, 3.078, 3.173, 3.258, 3.336, 
         3.407, 3.472, 3.532, 3.588, 3.640, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931)
  d3 = d3[n]; d2 = d2[n]; R = c(); UCL = (d2 + 3 * d3) * sigma; LCL = (d2 - 3 * d3) * sigma

  pb = txtProgressBar(min = 0, max = m, style = 3) # Barra de progreso
  
  i = 0
  while(length(R) < m){
    r = diff(range(rnorm(n, mu, CorrimientoSigma)))
    i = i + 1
    if (r < LCL | r > UCL){
      R = c(R, i)
      i = 0; setTxtProgressBar(pb, length(R))
    }}
  return(R)}
