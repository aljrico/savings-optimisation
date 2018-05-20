
# Table Results -----------------------------------------------------------



# Libraries and Functions -----------------------------------------------------------------

library(ggplot2)
library(dplyr)
source("functions.R")
source("estimate_equiv-pi.R")
library(data.table)



# Parameters --------------------------------------------------------------

alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
A <- 1

factor <- -3.255
K <- c()
cppi_ret <- c()
pi_b <- c()
es <- c()
cppi_ret <- c()
montses_ret <- c()
pis <- c()
mortality <- FALSE
nsim <- 1e4
set.seed(666)

if(mortality == TRUE ){

# Simulation Loop with mortality ------------------------------------------

	for(i in 4:10){
		print(i)
		pi <- 0.1*i
		pis[i] <- pi
		cppi_res<-  cppi_mortality(pi = pi,
															 nsim = nsim,
															 alpha = alpha,
															 sigma = sigma,
															 a = a,
															 years = years)
		es[i] <- cppi_res[1]
		K[i] <- es[i]*factor
		cppi_ret[i] <- cppi_res[2]
		#K[K <0] <- 0
		# K[i] <- -K[i]
		montses_res <- alt_mort(K = K[i],
														nsim = nsim,
														alpha = alpha,
														sigma = sigma,
														a = a,
														years = years)
		montses_ret[i] <- montses_res[2]
		pi_b[i] <- equiv_pi(ret = montses_ret[i], mortality = mortality)
	}

	df <- data.frame(Pi = pis*100, ES = es, K = K, cppi_ret, montses_ret, diff = (montses_ret - cppi_ret), pi_b)
}

if(mortality != TRUE){
# Simulation Loop without mortality ---------------------------------------

	for(i in 1:10){
		print(i)
		pi <- 0.1*i
		pis[i] <- pi
		cppi_res<- cppi(pi = pi,
										nsim = nsim,
										alpha = alpha,
										sigma = sigma,
										a = a,
										years = years)
		es[i] <- cppi_res[1]
		K[i] <- es[i]*factor
		cppi_ret[i] <- cppi_res[2]
		montses_res <- montses(K = K[i],
													 nsim = nsim,
													 alpha = alpha,
													 sigma = sigma,
													 a = a,
													 years = years,
													 A = A)
		montses_ret[i] <- montses_res[2]
		pi_b[i] <- equiv_pi(ret = montses_ret[i], mortality = mortality)
	}

	df <- data.frame(Pi = pis*100, ES = es, K = K, cppi_ret, montses_ret, diff = (montses_ret - cppi_ret), pi_b)

}

df







