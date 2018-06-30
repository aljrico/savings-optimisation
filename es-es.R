
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
nsim <- 1e4
theta <- 0.95
years <- 60
m <- 1e2
max_A <- 25
a <- 10

es_cppi <- c()
pi_record <- c()
es_alt <- c()
A_record <- c()
A <- 0.5
as <- c(0.5, 1, 1.5, 2)
as <- seq(from = 0.5, to =2, by = 0.1)
dat <- tibble()

for(j in 1:length(as)){
	cat(paste0("... ", j, " ... \n"))
	for(i in 1:100){
		A_record[i] <- as[j]
		pi <- i/100
		pi_record[i] <- pi
		es_cppi[i] <- cppi_mort_fasto(alpha = alpha, sigma = sigma, years = years, pi = pi, a=a, nsim = nsim) %>%
			ES()
		# factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
		# K <- es_cppi[i]*factor
		K <- es_to_k(A = A, pi = pi, nsim = 100, err = 0.15, k_max = 1200)

		es_alt[i] <- alt_mort_fasto(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
			ES()

		df <- cbind(A_record, es_cppi, es_alt, pi_record)
	}
	dat <- rbind(dat,df)
}

dat %>%
	as_tibble() %>%
	dplyr::mutate(pi_record = ifelse(pi_record <= 0.1, 0.1, pi_record)) %>%
	dplyr::mutate(pi_record = ifelse(pi_record >= 0.9, 0.9, pi_record)) %>%
	dplyr::mutate(pi_record = (floor(pi_record*10)/10) %>% as.factor()) %>%
	ggplot() +
	geom_point(aes(x = es_cppi, y = (es_alt))) +
	xlab("ES (CPPI)") +
	ylab("ES (Alternative)") +
	geom_abline(intercept = 0, slope = 1) +
	scale_colour_viridis(discrete=TRUE, name = "Pi") +
	theme_bw() +
	theme(legend.position = "none")


dat %>%
	as_tibble() %>%
	ggplot() +
	geom_point(aes(x = pi_record, y = (es_alt-es_cppi), colour = (A_record))) +
	xlab("ES (CPPI)") +
	ylab("ES (Alternative)") +
	theme_bw()
