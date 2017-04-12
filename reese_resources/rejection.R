g <- function(x){
  exp(-.5*(x^2))+.5*exp(-.5*((x-3)^2))
}

xx<-seq(-3,6,length=100)

#initial plot
plot(xx,g(xx),type="l",lwd=4,col="red")
lines(xx,1/(pi*(1+(xx-1)^2)),lwd=4,col="black")

#renormalized plot
plot(xx,1/(pi*(1+(xx-1)^2)),type="l",lwd=4,col="black")
lines(xx,.08*g(xx),lwd=4,col="red")

cand <- rt(10000,df=1)+1
accept <- .08*g(cand)/dt(cand-1,df=1)
u <- runif(10000)
out <- cand[u<accept]

length(out)
mean(out)
sqrt(var(out))

hist(out,nclass=50,prob=T,xlab="")
lines(xx,dnorm(xx,0,1)*.67 + dnorm(xx,3,1)*.33,lwd=4)
lines(density(out),lwd=4,col="red")


######################################################
h<-function(x){
  x
}

draws <- rt(10000,df=1)
wh <- h(draws)*g(draws)/dt(draws,df=1)
w <- g(draws)/dt(draws,df=1)


numerator <- mean(wh)
denominator <- mean(w)
expectation <- numerator/denominator

c(numerator, denominator, expectation)


xx<-seq(-3,6,length=100)
plot(xx,g(xx),type="l",col="red",lwd=4)
lines(xx,dt(xx,df=1),col="black",lwd=4)
legend(3,1,legend=c("I()","g()"),col=c("black","red"),lwd=4)
