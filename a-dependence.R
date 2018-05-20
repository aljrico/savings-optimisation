
# Libraries & Functions ---------------------------------------------------

source("functions.R")
source("estimate_equiv-pi.R")
library(data.table)
library(tidyverse)


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
A <- 2
init_A <- 0.1
pi <- 0.2
nsim <- 1e5
theta <- 0.95
years <- 60

pi_b <- c()



# Without Mortality -------------------------------------------------------

es <- cppi(pi = pi, nsim =nsim)[[1]]

for(i in 1:20){
	A <- init_A*i
	factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
	K <- es*factor

	ret <- montses(K = K, nsim = nsim, A = A)[[2]]
	pi_b[i] <- equiv_pi(ret = ret, m=1e3)
}



# With Mortality ----------------------------------------------------------

es <- cppi_mortality(pi = pi, nsim =nsim)[[1]]

for(i in 1:20){
	A <- init_A*i
	factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
	K <- es*factor

	ret <- alt_mort(K = K, nsim = nsim, A = A)[[2]]
	pi_b[i] <- equiv_pi(ret = ret, m=1e3)
}

plot(pi_b)
