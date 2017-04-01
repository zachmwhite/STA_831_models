p0 = c(.03,.02,.02,.01,0,.92)
p1 = c(.06,.05,.08,.02,.01,.78)
p2 = c(.09,.05,.12,0,.02,.72)
p1p0 = p1 / p0
p2p0 = p2 / p0
p1p0
p2p0


x.seq = seq(-100,100, by = 1)
theta1 = c(4,4,2)
theta0= c(2,4,4)

likeli.rat = function(x,theta.1,theta.0){
  (1 + (x-theta1.0)^2) / (1 + (x-theta.0)^2)
}

par(mfrow = c(1,3))
i = 1
plot(x.seq,likeli.rat(x.seq,theta1[i],theta0[i]))
for(i 2:3){
  plot(x.seq,likeli.rat(x.seq,theta1[i],theta0[i]))
}