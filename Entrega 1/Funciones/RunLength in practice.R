setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('RunLength.R')

corrimientos = seq(from = 1, to = 4, by = 0.05)
library(parallel)
library(pbmcapply)
cores = detectCores() - 1
# Tenga cuidado, esta función en ocasiones no funciona en equipos corriendo windows
S3 = as.numeric(pbmclapply(corrimientos, FUN = function(x) mean(RunLengthS(CorrimientoSigma = x, 
                                                                        m = 1000, n = 3)), 
             mc.cores = cores, mc.set.seed = 1305))
R3 = as.numeric(pbmclapply(corrimientos, FUN = function(x) mean(RunLengthR(CorrimientoSigma = x, 
                                                                           m = 1000, n = 10)), 
             mc.cores = cores, mc.set.seed = 1305))

S10 = as.numeric(pbmclapply(corrimientos, FUN = function(x) mean(RunLengthS(CorrimientoSigma = x, 
                                                                          m = 1000, n = 10)), 
                          mc.cores = cores, mc.set.seed = 1305))
R10 = as.numeric(pbmclapply(corrimientos, FUN = function(x) mean(RunLengthR(CorrimientoSigma = x,
                                                                            n = 10, m = 1000)), 
                          mc.cores = cores, mc.set.seed = 1305))

Resultados = rbind('Carta R (n = 3)' = R3, 'Carta S (n = 3)' = S3,
                   'Carta R (n = 10)' = R10, 'Carta S (n = 10)' = S10)
colnames(Resultados) = paste('k = ', corrimientos)

write.csv(Resultados, file = '../Resultados/Punto 1')
