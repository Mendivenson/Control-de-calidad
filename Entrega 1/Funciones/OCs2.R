OCs2 = function(n = 5, sigma2_0 = 1, corrimiento = 1){
  UCL = sigma2_0/(n-1) * qchisq(0.975, df = n - 1)
  LCL = sigma2_0/(n-1) * qchisq(0.025, df = n - 1)
  beta = pchisq((n-1) * UCL / corrimiento , df = n - 1) 
  - pchisq((n-1) * LCL / corrimiento , df = n - 1)
  return(beta)
}
