
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
init_A <- 0.01
nsim <- 1e6
theta <- 0.95
years <- 60
m <- 1e2
max_A <- 250
a <- 10
with_mortality <- TRUE

pi_b <- c()
all_rets <- c()


pis <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1)

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
es_alt <- c()
es_cppi <- c()

df_total <- tibble()

pis <- seq(0.1, 0.9, by = 0.1)
as <- seq(0.1, 2, by = 0.1)

error_count <- 0
for(pi in pis){
		cat(paste0("... ... ", pi, " ... ... \n"))
		pi_b <- c()
		for(i in 1:length(as)){
			cat(paste0("... ", i, " ... \n"))
			es_cppi[i] <- cppi_mort_fasto(pi = pi, nsim =nsim) %>% na.omit() %>% ES()
			A <- as[i]
			K <- es_to_k(A = A, pi = pi, nsim = 1e4, err = 0.01, k_max = 1000, size = 1)
			if(is.na(K)) K <- es_to_k(A = A, pi = pi, nsim = 1e3, err = 0.1, k_max = 2000, size = 2)
			if(is.na(K)) K <- es_to_k(A = A, pi = pi, nsim = 1e2, err = 0.25, k_max = 5000, size = 5)
			# secant(x0 = 0, x1 = 2000, tol=0.05, niter=500, nsim = 1e3, A = A, pi = pi)

			if(!is.na(K)){
				res <- alt_mort_fasto(K = K, nsim = nsim, A = A) %>% na.omit()
				ret <- res %>% compute_return()
				es_alt[i] <- res %>% ES()
				all_rets[i] <- ret
				pi_b[i] <-  equiv_pi(ret = ret, m=m, mortality = with_mortality, nsim = 1e3, max_pi = 2)
			}else{
				es_alt[i] <- NA
				all_rets[i] <- NA
				pi_b[i] <- NA
			}
		}

		m_pi <- as_tibble(data.frame(pi_b))
		m_mort <- TRUE
		m_A <- as
		m_ret <- all_rets
		m_inpi <- pi
		es <- es_cppi

		df_m <- cbind(pi = m_pi, mort = m_mort, A = m_A, ret = m_ret, in_pi = m_inpi, es_cppi = es, es_alt)

		df_total <- df_total %>% rbind(df_m)
}

df_total %>%
	# filter(mort == FALSE) %>%
	ggplot(aes(colour = as.factor(in_pi))) +
	geom_line(aes(y = pi_b, x = A), size=1) +
	geom_point(aes(y = pi_b, x = A), size = 1.25) +
	geom_line(aes(y = in_pi, x = A), size = 0.85, linetype = "dashed")+
	# facet_grid(.~ mort) +
	theme_bw() +
	scale_colour_viridis(discrete=TRUE, end =1, begin = 0, option = "D") +
	# scale_colour_brewer(palette = "Set1") +
	xlab("A") +
	ylab(expression(pi)) +
	labs(colour = "Actual Pi of the \nBenchmark") +
	scale_y_continuous(limits = c(0,1)) +
	scale_x_continuous(limits = c(0,2))




# Comparing Pi
df_total %>%
	filter(mort == TRUE) %>%
	# filter(es < 0) %>%
	ggplot(aes(colour = as.factor(in_pi))) +
	# geom_jitter(aes(y = pi_b, x = A), size=1) +
	geom_smooth(aes(y = pi_b, x = A), se = FALSE, method = "loess") +
	# geom_point(aes(y = pi_b, x = A), size = 1.25) +
	geom_line(aes(y = in_pi, x = A), size = 0.85, linetype = "dashed")+
	# facet_grid(.~ mort) +
	theme_bw() +
	scale_colour_viridis(discrete=TRUE, end =1, begin = 0, option = "D") +
	# scale_colour_brewer(palette = "Set1") +
	xlab("A") +
	ylab(expression(pi)) +
	labs(colour = "Actual Pi of the \nBenchmark") +
	scale_y_continuous(limits = c(0,1)) +
	scale_x_continuous(limits = c(0,2))

df_total %>%
	# filter(mort == FALSE) %>%
	ggplot(aes(colour = as.factor(in_pi))) +
	geom_line(aes(y = pi_b, x = A), size=1) +
	geom_point(aes(y = pi_b, x = A), size = 1.25) +
	geom_line(aes(y = in_pi, x = A), size = 0.85, linetype = "dashed")+
	# facet_grid(.~ mort) +
	theme_bw() +
	scale_colour_viridis(discrete=TRUE, end =1, begin = 0, option = "D") +
	# scale_colour_brewer(palette = "Set1") +
	xlab("A") +
	ylab(expression(pi)) +
	labs(colour = "Actual Pi of the \nBenchmark") +
	scale_y_continuous(limits = c(0,1)) +
	scale_x_continuous(limits = c(0,2))

df_total %>%
	ggplot() +
	geom_jitter(aes(x = es_cppi, y = es_alt, colour = (A))) +
	theme_bw()
