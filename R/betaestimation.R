estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

estBetaParams(0.1005, 0.0251) #rrOSEXuc
estBetaParams(0.0023, 0.0006) #rrEREXuc, rrHOEXuc
estBetaParams(0.00276, 0.0007) #amrc

estBetaParams(0.0963, 0.0241) #rrOSEXc
estBetaParams(0.0023, 0.0006) #rrEREXc
estBetaParams(0.00276, 0.0007) #amrc


estBetaParams(0.670, 0.15) #uNES
0.36^2 = (S)^2
estBetaParams(0.57, 0.1296) #uOCEX

estBetaParams(0.45, 0.37^2) #UEREX
estBetaParams(0.33, 0.39^2) #uHOEX

estBetaParams(0.04, 0.009354416) #pCAPH
estBetaParams(0.03, 0.006374058) #pCAUC
estBetaParams(0.01, 0.001780351) #pUAPH
estBetaParams(0.01, 0.0087) #pUAUC


(0.15*sqrt(50))^2
