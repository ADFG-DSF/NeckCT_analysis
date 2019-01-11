model{
  for(i in 1:n_diff){diff[i] ~ dnorm(mu, tau)}
  mu ~ dnorm(0, 0.0001)
  sigma <- 1 / sqrt(tau)
  tau ~ dgamma(0.001,0.001)
  
  for (i in 1:n2){
	n2_diff[i] ~ dnorm(mu, tau)
	fl[i] = n2_fl[i] - n2_diff[i]
  }
  n2_star = sum(fl > 180)
  N ~ dunif(1800, 10000)
  b <- round(N) - n1
  m2 ~ dhyper(n1, b, n2_star, 1)
}