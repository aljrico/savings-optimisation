
# Table Results -----------------------------------------------------------


library(ggplot2)
library(dplyr)
source("functions.R")

factor <- -3.255
K <- c()
cppi_ret <- c()
pi_b <- c()
es <- c()
cppi_ret <- c()
montses_ret <- c()
pis <- c()

for(i in 1:10){
	print(i)
	pi <- 0.1*i
	pis[i] <- pi
	cppi_res<- cppi_mortality(pi, nsim=1000)
	es[i] <- cppi_res[1]
	K[i] <- es[i]*factor
	cppi_ret[i] <- cppi_res[2]

	montses_res <- montses_mortality(K[i], nsim=100)
	montses_ret[i] <- montses_res[2]
	pi_b[i] <- montses_res[1]
}

df <- data.frame(Pi = pis*100, ES = es, K = K, cppi_ret, montses_ret, diff = (montses_ret - cppi_ret), pi_b)
