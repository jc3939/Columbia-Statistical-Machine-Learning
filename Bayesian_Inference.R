
sample=rexp(256,1)
alpha=2
beta=0.2
size=4
theta=seq(0,4,by=0.01)
prior0=theta**(alpha-1)*(beta**(alpha))*exp(-beta*theta)/exp(lgamma(alpha))
plot(theta,prior0,type='l',col='black',ylim=range(c(0,0.1)))

alpha=2
beta=0.2
alpha=alpha+4
beta=beta+sum(sample(sample,4))
prior4=((theta**(alpha-1))*(beta**(alpha))*exp(-beta*theta))/exp(lgamma(alpha))
plot(theta,prior4,type='l',col='black',ylim=range(c(0,0.8)))

alpha=2
beta=0.2
alpha=alpha+8
beta=beta+sum(sample(sample,8))
prior8=theta**(alpha-1)*(beta**(alpha))*exp(-beta*theta)/exp(lgamma(alpha))
plot(theta,prior8,type='l',col='black',ylim=range(c(0,1.5)))

alpha=2
beta=0.2
alpha=alpha+16
beta=beta+sum(sample(sample,16))
prior16=theta**(alpha-1)*(beta**(alpha))*exp(-beta*theta)/exp(lgamma(alpha))
plot(theta,prior16,type='l',col='red',ylim=range(c(0,1.5)))


alpha=2
beta=0.2
alpha=alpha+256
beta=beta+sum(sample(sample,256))
prior256=exp((alpha-1)*log(theta)+alpha*log(beta)-beta*theta-lgamma(alpha))
plot(theta,prior256,type='l',col='blue',ylim=range(c(0,1)))
par(new=T)
plot(theta,prior16,type='l',col='red',ylim=range(c(0,1)))
par(new=T)
plot(theta,prior8,type='l',col='red',ylim=range(c(0,1)))
par(new=T)
plot(theta,prior4,type='l',col='red',ylim=range(c(0,1)))
par(new=T)
plot(theta,prior0,type='l',col='red',ylim=range(c(0,1)))