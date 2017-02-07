
x = rep(0,100000)
x[1] = 1
for(i in 461915:1000000){
  x[i] = 65539 * x[i-1] %% 2^31
}
index = 1:100000
index.x1 = index[index %% 3 == 1]
index.x2 = index[index %% 3 == 2]
index.x3 = index[index %% 3 == 0]
x1 = x[index.x1]
x2 = x[index.x2]
x3 = x[index.x3]

randu1 = x1 / 2^31
randu2 = x2 / 2^31
randu3 = x3 / 2^31

plot(randu3,(6*randu2 - 9 * randu1))

N = 10000
randu = matrix(0,ncol = 3, nrow = N)
x3 =1
for(i in 1:N){
  x1 = (65539 * x3) %% 2^31
  x2 = (65539 * x1) %% 2^31
  x3 = (65539 * x2) %% 2^31
  randu[i,] = c(x1/ 2^31,x2/ 2^31,x3/ 2^31) 
}
plot(randu[,3], (9*randu[,1] - 6*randu[,2]), ylim = c(-7,11), pch = 21, lwd = .5, xlab = expression(X[t+1]), 
     ylab = expression(9*X[t] - 6*X[t-1]))
