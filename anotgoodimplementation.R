
#--------------------------------
#   Primero generamos los datos  |
#--------------------------------

library(mvtnorm)
library(ggplot2)

Mu1=c(1,1)
Mu2=c(7,7)
Sigma1=matrix(c(2,1,1,1),2,2)
Sigma2=matrix(c(2,2,2,5),2,2)

pi=1/3
n=500


set.seed(2)
data=matrix(0,n,2)

z = rep(0,n)
for (i in 1:n){
  z[i] = rbinom(1,1,pi)
  if (z[i] ==1){
    data[i,] = rmvnorm(1, Mu1,Sigma1)
  }else{
    data[i,] = rmvnorm(1, Mu2,Sigma2)
  }
}

to.plot = data.frame(x = data[,1],
                     y = data[,2],
                     class =  z)
ggplot(to.plot)+ aes(x, y, color = class)+
  geom_point()+geom_density_2d()

#Aqui el EM

pi = 0.5
mu1 = data[1,]
mu2 = data[150,]
sigma1 = cov(data)
sigma2 = cov(data)



# EM algorithm
Alpha = NULL
phi1 = dmvnorm(data, mu1, sigma1)
phi2 = dmvnorm(data, mu2, sigma2)
n_iter=50
for (i in 1:n_iter){
  # Expectation step
  alpha = (pi*phi1)/(pi*phi1+(1-pi)*phi2)
  Alpha = cbind(Alpha, alpha)
  
  # Maximization step
  mu1 =apply(alpha*data/sum(alpha),2,sum)
  mu2 =apply((1-alpha)*data/sum(1-alpha),2,sum)
  
  sigma1 = cov.wt(data, wt = alpha, method = "ML")$cov
  sigma2 = cov.wt(data, wt = 1-alpha, method = "ML")$cov
  pi = mean(alpha)
  
  phi1 = dmvnorm(data, mu1, sigma1)
  phi2 = dmvnorm(data, mu2, sigma2)
}
