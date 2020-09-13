library(Rlab)

S0 = 940
sigma = 0.15
r = 0.05

# 8 dias com intervalos de 2d -> t = 4 
t = 4
delta = 2

# Número de simulações
m = 3

P = matrix(NA, nrow = t, ncol = m)
P_0 = 940

P[1,] = P_0

coin = rbern(1, 0.5)

for (i in 2:t) for (j in 1:m) {

coin = rbern(1, 0.5)

if (coin == 1) { 
  
  deltaW = sqrt(2)
  
} else {
  
  deltaW = -sqrt(2)
  
}

P[i,j] = P[(i-1),j]*exp(delta*r -0.5*sigma^2*delta + 0.15*deltaW)

}




# 11.algo

X = matrix(nrow = 5, ncol = 1)
X_cbm = matrix(nrow = 5, ncol = 1)

for (i in (1:nrow(X))) {
  
  X[i,] = rnorm(1, 0, sqrt(0.2))
}

X = rnorm(5,0,sqrt(0.2))

X_cbm[1,] = X[1]

for (i in (2:5)) {
  X_cbm[i,] = X_cbm[(i-1),] + X[i]
}

St = function(S0, r, vol, delta, t, shock) {
  
 S0*exp(r*t*delta + vol*shock - (0.5)*(vol^2)*t*delta)
  
}

Price = matrix(NA, nrow = 5, ncol = 2)
Price[1,] = 1
for (i in 2:nrow(Price)) {
  
  Price[i,1] = St(1, 0.03, 0.05, 0.2, 5, X_cbm[i,])
  
}
Price

call_exp = max(0, (Price[5,1] - 1.5))
call_exp

# Now for runif.

Y = runif(5, -sqrt(0.6), sqrt(0.6))

Y_cbm = matrix(NA, nrow = 5, ncol = 1)

Y_cbm[1,] = Y[1]

for (i in (2:5)) {
  Y_cbm[i,] = Y_cbm[(i-1),] + Y[i]
}


for (i in 2:nrow(Price)) {
  
  Price[i,2] = St(1, 0.03, 0.05, 0.2, 5, Y_cbm[i])
  
}

Price_unif = Price[,2]
Price_unif

max(Price_unif - 1.5, 0)

### Monte Carlo Simulation
## 1. Normal. Estou usando K = 1 (strike) pois para K = 1.5, o valor no vencimento é muito proximo de zero (choques precisam ser grandes e positivos para call estar in the money.)

mc_norm = matrix(NA, nrow = 5, ncol = 1000)

mc_norm[1,] = 1

for (i in (2:5)) for (j in 1:1000) {
  
  norm_i = rnorm(5, 0, sqrt(0.2))
  cbm_norm = matrix(NA, nrow = 5, ncol = 1)
  cbm_norm[1,] = norm_i[1]
  for (k in (2:5)) {
    cbm_norm[k,] = cbm_norm[(k-1),] + norm_i[k]
  }
  mc_norm[i, j] = St(1, 0.03, 0.05, 0.2, 5, cbm_norm[i,])
  
}

mc_norm

call_norm = matrix(NA, nrow = 1000, ncol = 1)

for (i in (1:1000)) {
  
  call_norm[i,] = max(mc_norm[i] - 1, 0)
  
}

mean(call_norm)

### Monte Carlo Simulation
## 2. Unif. Estou usando K = 1 (strike) pois para K = 1.5, o valor no vencimento é muito proximo de zero (choques precisam ser grandes e positivos para call estar in the money.)

mc_unif = matrix(NA, nrow = 5, ncol = 1000)

mc_unif[1,] = 1

for (i in (2:5)) for (j in 1:1000) {
  
  unif_i = runif(5, -sqrt(0.6), sqrt(0.6))
  cbm_unif = matrix(NA, nrow = 5, ncol = 1)
  cbm_unif[1,] = unif_i[1]
  for (k in (2:5)) {
    cbm_unif[k,] = cbm_unif[(k-1),] + unif_i[k]
  }
  mc_unif[i, j] = St(1, 0.03, 0.05, 0.2, 5, cbm_unif[i,])
  
}

mc_unif

call_unif = matrix(NA, nrow = 1000, ncol = 1)

for (i in (1:1000)) {
  
  call_unif[i,] = max(mc_unif[i] - 1, 0)
  
}

mean(call_unif)



