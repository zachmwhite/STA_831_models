g <- function(x){
  exp(-.5*(x^2))+.5*exp(-.5*((x-3)^2))
}

xx<-seq(-3,6,length=100)

#initial plot
plot(xx,g(xx))
lines(xx,g(xx),lwd=4,col="red")

out <- numeric(3000)
out[1] <- 3
save <- matrix(0,nrow=3000,ncol=5)

for(i in 2:3000){
  out[i]<-out[i-1]
  cand <- rnorm(1,out[i-1],1)
  accept <- g(cand)/g(out[i-1])
  u <- runif(1,0,1)
  if (u < accept) {out[i] <- cand}
  save[i,1] <- out[i-1]
  save[i,2] <- cand
  save[i,3] <- accept
  save[i,4] <- u
  save[i,4] <- out[i]
}

save[1:20,]
lines(density(out),lwd=4,col="purple")
lines(xx,dnorm(xx,0,1)*.67+dnorm(xx,3,1)*.33,lwd=4,col="blue")
