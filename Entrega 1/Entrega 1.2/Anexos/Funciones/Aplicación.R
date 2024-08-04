
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

# == ARL teórico
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

# == ARL (20 subgrupos)
# Simulación de los 20 subgrupos racionales
mu = 0; sigma = 1                 # Parámetros para muestra aleatoria
n = 3; m = 20                     # Tamaño de muestra y cantidad de muestras              
set.seed(10)                      # Semilla para replicación
muestras = matrix(nrow = m, ncol = n)
for (i in 1:m){
  muestras[i,] = rnorm(n, mean = mu, sd = sigma)
}

medias = rowMeans(muestras)               # Media de cada una de las muestras
rangos = apply(muestras, MARGIN = 1,      # Rango de cada una de las muestras
               FUN = function(x) diff(range(x)))
rango = mean(rangos)                      # Media de los rangos
media = mean(medias)                      # Media de las medias
d2 = 1.693                                # Constante para hacer insesgado al rango.
sigma = rango/d2

# Al revisar el gráfico, no se encuentran medias afuera de los límites por lo que en fase II
# se utilizan estas mismas estimaciones

# Cálculo aproximado del ARL
resultados = resultados["n = 3",]
rownames(resultados) = 'Teórico'
resultados['Fase I (20 subgrupos)',] = as.numeric(pbmclapply(corrimientos, 
                                                             FUN = function(x) mean(RunLengthXMod_Optimized(mu1 = media, 
                                                                                                            sigma1 = sigma,
                                                                                                            Corrimiento = x,
                                                                                                            m = 50000)),
                                                             mc.cores = cores, mc.set.seed = 1305))

# == ARL (50 subgrupos)
# Simulación de los 50 subgrupos racionales
mu = 0; sigma = 1                 # Parámetros para muestra aleatoria
n = 3; m = 50                     # Tamaño de muestra y cantidad de muestras              
set.seed(10)                      # Semilla para replicación
muestras = matrix(nrow = m, ncol = n)
for (i in 1:m){
  muestras[i,] = rnorm(n, mean = mu, sd = sigma)
}

medias = rowMeans(muestras)               # Media de cada una de las muestras
rangos = apply(muestras, MARGIN = 1,      # Rango de cada una de las muestras
               FUN = function(x) diff(range(x)))
rango = mean(rangos)                      # Media de los rangos
media = mean(medias)                      # Media de las medias
d2 = 1.693                                # Constante para hacer insesgado al rango.
sigma = rango/d2

# Al revisar el gráfico, no se encuentran medias afuera de los límites por lo que en fase II
# se utilizan estas mismas estimaciones

# Cálculo aproximado del ARL
resultados['Fase I (50 subgrupos)',] = as.numeric(pbmclapply(corrimientos, 
                                                             FUN = function(x) mean(RunLengthXMod_Optimized(mu1 = media, 
                                                                                                            sigma1 = sigma,
                                                                                                            Corrimiento = x,
                                                                                                            m = 50000)),
                                                             mc.cores = cores, mc.set.seed = 1305))

write.csv(resultados, file = 'Entrega 1/Entrega 1.2/Anexos/Resultados/ARL X Chart theory and stimated.csv', row.names = T)