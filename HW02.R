# HW 02

# Draws from a cauchy normal posterior
nreps = 10000
draws = rep(NA,nreps)
for(i in 1:nreps){
  theta.draw = rcauchy(1,0,1)
  x = rnorm(1,theta.draw,1)
  u = runif(1,0,1)
  post = (1/(pi*(1+theta.draw^2)))*(1/(2*pi))*exp((-(x-theta.draw)^2)/2)
  if(u <= post){draws[i] = theta.draw}
}

real.draws = draws[is.na(draws) == FALSE]
draws = NULL
while(length(draws) <= 1000){
  theta.draw = rcauchy(1)
  x = rnorm(1,theta.draw,1)
  u = runif(1,0,1)
  post = (1/(pi*(1+theta.draw^2)))*(1/(2*pi))*exp((-(x-theta.draw)^2)/2)
  if(u <= post){
    draws = c(draws,theta.draw)
  }
}
f1=function(t){ t/(1+t*t)*exp(-(x-t)^2/2)}
f2=function(t){ 1/(1+t*t)*exp(-(x-t)^2/2)}
plot(f1(x),col=1,ylim=c(-0.5,1),xlab="t",ylab="",type = "l")
lines(f2(x),col=2,type = "l")
legend("topright", c("f1=t.f2","f2"), lty=1,col=1 :2)


## Question 2
num = function(theta){(theta/(1+theta^2)) * exp(-(1/2)*(x-theta)^2)}
denom = function(theta){(1/(1+theta^2)) * exp(-(1/2)*(x-theta)^2)}
x = seq(-3,3,length= 10000)
plot(num(x), type = "l", ylim = c(-.5,1), xlab = expression(theta))
lines(denom(x), type = "l", col = "red",xlab = expression(theta))


x = 2
nreps = 100000
cauchy.draws = rcauchy(nreps,0,1)
overall.int = mean(cauchy.draws*dnorm(cauchy.draws,x)) / mean(dnorm(cauchy.draws,x))
Niter=10^4
co=rcauchy(Niter)
I=mean(co*dnorm(co,mean=x))/mean(dnorm(co,mean=x))
# Part B
cauchy.draws = rcauchy(nreps,0,1)
eval.denom = dnorm(cauchy.draws,x)
cum.sum.denom = cumsum(eval.denom) / (1:nreps)
stand.error = sqrt(cumsum((eval.denom - cum.sum.denom)^2))/(1:nreps)
plot(cum.sum.denom, type = "l", ylim = c(.08,.1))
lines(cum.sum.denom + stand.error, col = "red")
lines(cum.sum.denom - stand.error, col = "red")

eval.num = cauchy.draws * eval.denom
cum.sum.num = cumsum(eval.num) / (1:nreps)
num.stand.error = sqrt(cumsum((eval.num - cum.sum.num)^2))/(1:nreps)
plot(cum.sum.num, type = "l",ylim = c(.10,.15))
lines(cum.sum.num + num.stand.error, col = "red")
lines(cum.sum.num - num.stand.error, col = "red")
## Some simple calculations for sample size

# Exercise 3.2

probs = dnorm()