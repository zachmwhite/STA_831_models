# HW 02

# Draws from a cauchy normal posterior
nreps = 10000
draws = rep(NA,nreps)
for(i in 1:nreps){
  theta.draw = rcauchy(1,0,1)
  x = rnorm(1,theta.draw,1)
  u = runif(1,0,1)
  post = (1/)
  if(u <= )
}