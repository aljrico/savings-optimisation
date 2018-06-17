
# Libraries & Functions ---------------------------------------------------

source("functions.R")
source("estimate_equiv-pi.R")
library(data.table)
library(tidyverse)
library(viridis)
library(Rcpp)
sourceCpp("cppi.cpp")


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
A <- 0.5
nsim <- 1e5
theta <- 0.95
years <- 60
m <- 1e2
max_A <- 25
a <- 10

es_cppi <- c()
es_alt <- c()
pi <- 0.1
for(i in 1:200){
	A <- i/100
	es_cppi[i] <- cppi_c(alpha = alpha, sigma = sigma, years = years, pi = pi, a=a, nsim = nsim) %>%
		ES()
	factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
	K <- es_cppi[i]*factor

	cat(paste0("... ", i, " ..."))

	es_alt[i] <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
		ES()

	while(is.na(es_alt[i])) {es_alt[i] <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
		ES()}
}

cbind(es_cppi, es_alt) %>%
	as_tibble() %>%
	ggplot() +
	geom_point(aes(x = es_cppi, y = es_alt)) +
	xlab("ES (CPPI)") +
	ylab("ES (Alternative)") +
	theme_bw()
