# Solow with technology
productionFunction <- function(A, K, L, alpha){
  Y = (A)*(K^alpha)*(L^(1-alpha))
  return(Y)
  }

capitalAcumulation <- function(s, d, A, K, L, alpha){
  Kdot = s*productionFunction(A, K, L, alpha) - d*K
  return(Kdot)
}

capitalPerWorkerGrowth <- function(s, g, n, d, A, K, L, alpha){
  y_perWorker = productionFunction(A,K,L,alpha)/(A*L)
  k_perWorker = K/(A*L)
  kdot = s*y_perWorker - (n+d+g)*k_perWorker
  return(kdot)
}

steadyStateIncomePerWorker <- function(s, n, g, d,alpha){
  y_star = ((s/n+g+d))^(alpha/(1-alpha))
  return(y_star)
}

steadyStateCapitalPerWorker <- function(s, n, g, d,alpha){
  k_star = ((s/n+g+d))^(1/(1-alpha))
  return(k_star)
}

savingsLine <- function(s, A, K, L, alpha){
  y_perWorker = productionFunction(A, K, L, alpha)/(A*L)
  return(s*y_perWoker)
}

depreciationLine <- function(d, g, n, K, A, L){
  k_perWorker = K/(A*L)
  return((d+g+n)*k_perWorker)
}

# Exemplo
df <-NULL
for(i in 0:20){
  ss = savingsLine(0.2, 1, i, 1, 0.5)
  dd = depreciationLine(0.01, 0.03, 0.02, i, 1, 1)
  temp <- data.frame(ss, dd)
  df <- rbind(df, temp)
}
plot(df$ss, type = 'l', col = 'green')
lines(df$dd, col = 'red')
