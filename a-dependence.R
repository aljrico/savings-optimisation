
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
A <- 2
init_A <- 0.1
nsim <- 1e5
theta <- 0.95
years <- 60
m <- 1e2
max_A <- 25
a <- 10
pi_b <- c()
all_rets <- c()


pis <- c(0.2, 0.4)

wm_pi <- c()
wm_mort <- c()
wm_A <- c()
wm_ret <- c()
wm_inpi <- c()

m_pi <- c()
m_mort <- c()
m_A <- c()
m_ret <- c()
m_inpi <- c()

df_total <- tibble()

error_count <- 0

for(pi in pis){
	es <- cppi_c(alpha = alpha, sigma = sigma, years = years, pi = pi, a=a, nsim = nsim) %>%
		ES()

	# Without Mortality -------------------------------------------------------
	for(i in 1:max_A){
		A <- init_A*i
		factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
		K <- es*factor

		ret <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
			compute_return(c = a, years = years)

		# In order to avoid random errors, just repeat it one more time.
		if(is.na(ret)){	ret <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
			compute_return(c = a, years = years); error_count <- error_count + 1}


		cat(paste0("...",i/max_A*100, "()% ... \n"))
		pi_b[i] <- equiv_pi(ret = ret, m=m)
		all_rets[i] <- ret
	}

	wm_pi <- as_tibble(data.frame(pi_b))
	wm_mort <- FALSE
	wm_A <- (1:i)/10
	wm_ret <- all_rets
	wm_inpi <- pi

	df_wm <- cbind(pi = wm_pi, mort = wm_mort, A = wm_A, ret = wm_ret, in_pi = wm_inpi)

	# With Mortality ----------------------------------------------------------

	pi_b <- c()
	es <- cppi_mortality(pi = pi, nsim =nsim)[[1]]

	for(i in 1:max_A){
		A <- init_A*i
		factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
		K <- es*factor

		ret <- alt_mort(K = K, nsim = nsim, A = A)[[2]]
		if(!is.na(ret)){
			all_rets[i] <- ret
			pi_b[i] <- equiv_pi(ret = ret, m=m)
		}else{
			pi_b[i] <- 0
			all_rets[i] <- 0
			}
		cat(paste0("...",i/max_A*100, "()% ... \n"))
	}

	m_pi <- as_tibble(data.frame(pi_b))
	m_mort <- TRUE
	m_A <- (1:max_A)/10
	m_ret <- all_rets
	m_inpi <- pi

	df_m <- cbind(pi = m_pi, mort = m_mort, A = m_A, ret = m_ret, in_pi = m_inpi)



	df_total <- df_total %>% rbind(df_wm, df_m)
}


# Plots -------------------------------------------------------------------



# Comparing Pi
df_total %>%
	# filter(mort == FALSE) %>%
	ggplot(aes(colour = as.factor(mort))) +
	geom_line(aes(y = pi_b, x = A, linetype = as.factor(in_pi)), size=1) +
	geom_point(aes(y = pi_b, x = A), size = 1.25) +
	# geom_line(aes(y = in_pi, x = A), size = 0.75, linetype = "dashed")+
	# facet_grid(.~ mort) +
	theme_minimal() +
	scale_colour_viridis(discrete=TRUE, end =0.75, begin = 0.1) +
	# scale_colour_brewer(palette = "Set1") +
	xlab("A") +
	ylab(expression(pi)) +
	labs(linetype = "Actual Pi of the \nBenchmark", colour = "Mortality")


# Comparing Mortality
df_total %>%
	filter(in_pi == 0.4) %>%
	ggplot(aes(colour = mort)) +
	geom_line(aes(y = pi_b, x = A), size=1.3) +
	geom_point(aes(y = pi_b, x = A), size = 2) +
	# geom_line(aes(y = ret, x = A), size = 1)+
	theme_minimal() +
	scale_colour_viridis(discrete=TRUE, end =0.75) +
	xlab("A") +
	ylab(expression(pi)) +
	labs(colour = "Mortality")


