
# Table Results -----------------------------------------------------------



# Libraries and Functions -----------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(Rcpp)
source("functions.R")
sourceCpp("cppi.cpp")
source("estimate_equiv-pi.R")
library(data.table)



# Parameters --------------------------------------------------------------

alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
A <- 0.5

factor <- factor_kes(A = A)
K <- c()
cppi_ret <- c()
pi_b <- c()
es <- c()
cppi_ret <- c()
montses_ret <- c()
pis <- c()
mortality <- TRUE
nsim <- 1e5
set.seed(666)
new_es <- c()

if(mortality == TRUE ){

	# Simulation Loop with mortality ------------------------------------------

	for(i in 1:10){
		print(i)
		pi <- 0.1*i
		pis[i] <- pi
		cppi_res<-  cppi_mort_fasto(pi = pi,
																nsim = nsim,
																alpha = alpha,
																sigma = sigma,
																a = a,
																years = years)
		es[i] <- cppi_res %>% ES()
		K[i] <- es_to_k(A = A, pi = pi, nsim = 1000, err = 0.15, k_max = 1200)
		cppi_ret[i] <- cppi_res %>% compute_return()

		montses_res <- alt_mort_fasto(K = K[i],
																	nsim = nsim,
																	alpha = alpha,
																	sigma = sigma,
																	a = a,
																	years = years)
		montses_ret[i] <- montses_res %>% na.omit() %>% compute_return()
		new_es[i] <- montses_res %>% na.omit() %>% ES()
		pi_b[i] <- equiv_pi(ret = montses_ret[i], mortality = mortality, max_pi = 2, m = 50, nsim = 1e3)
	}

	df <- data.frame(Pi = pis*100, ES = es, K = K, cppi_ret, montses_ret, diff = (montses_ret - cppi_ret), pi_b)
}

if(mortality != TRUE){
	# Simulation Loop without mortality ---------------------------------------

	for(i in 1:10){
		print(i)
		pi <- 0.1*i
		pis[i] <- pi
		cppi_res<- cppi_c(pi = pi,
										nsim = nsim,
										alpha = alpha,
										sigma = sigma,
										a = a,
										years = years)
		es[i] <- cppi_res %>% ES()
		K[i] <- es[i]*factor
		cppi_ret[i] <- cppi_res %>% compute_return()
		montses_res <- alt_c(K = K[i],
													 nsim = nsim,
													 alpha = alpha,
													 sigma = sigma,
													 a = a,
													 years = years,
													 A = A)
		montses_ret[i] <- montses_res %>% compute_return()
		pi_b[i] <- equiv_pi(ret = montses_ret[i], mortality = mortality)
	}

	df <- data.frame(Pi = pis*100, ES = es, K = K, cppi_ret, montses_ret, diff = (montses_ret - cppi_ret), pi_b)

}

df







