
# Punto 1 -----------------------------------------------------------------

# == ARL Carta R
library(here)
setwd(here())
source('Entrega 1/Entrega 1.2/Anexos/Funciones/Run Length R.R')
require(pbmcapply)

tamaños = c(3,10)
corrimientos = seq(-2,0, by = 0.025)
resultados = matrix(ncol = length(corrimientos), nrow = length(tamaños), 
                    dimnames = list(paste0('n = ', tamaños), corrimientos))
cores = detectCores()
m = 50000
for (i in tamaños){
  resultados[paste0('n = ', i),] = as.numeric(pbmclapply(corrimientos,
                                                               FUN = function(x) mean(RunLengthR_optimized(k = x, m = m, n = i)),
                                                               mc.cores = cores, mc.set.seed = 1305))
}

write.csv(resultados, file = 'Entrega 1/Entrega 1.2/Anexos/Resultados/ARL R Chart.csv', row.names = T)

# == ARL Carta S
source('Entrega 1/Entrega 1.2/Anexos/Funciones/Run Length S.R')
resultados = matrix(ncol = length(corrimientos), nrow = length(tamaños), 
                    dimnames = list(paste0('n = ', tamaños), corrimientos))
for (i in tamaños){
  resultados[paste0('n = ', i),] = as.numeric(pbmclapply(corrimientos,
                                                         FUN = function(x) mean(RunLengthS_optimized(k = x, m = m, n = i)),
                                                         mc.cores = cores, mc.set.seed = 1305))
}

write.csv(resultados, file = 'Entrega 1/Entrega 1.2/Anexos/Resultados/ARL S Chart.csv', row.names = T)

# Punto 3 -----------------------------------------------------------------
source('Entrega 1/Entrega 1.2/Anexos/Funciones/Run Length X bar.R')
tamaños = c(3,5,8,10)
corrimientos = seq(0,3, by = 0.025)
resultados = matrix(ncol = length(corrimientos), nrow = length(tamaños), 
                    dimnames = list(paste0('n = ', tamaños), corrimientos))
for (i in tamaños){
  resultados[paste0('n = ', i),] = as.numeric(pbmclapply(corrimientos,
                                                         FUN = function(x) mean(RunLengthXMod_Optimized(Corrimiento = x, m = m, n = i)),
                                                         mc.cores = cores, mc.set.seed = 1305))
}

write.csv(resultados, file = 'Entrega 1/Entrega 1.2/Anexos/Resultados/ARL X Chart.csv', row.names = T)
