
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
with_mortality <- TRUE

pi_b <- c()
all_rets <- c()


pis <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

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
		pi_b <- c()
		es <- cppi_mort_fasto(pi = pi, nsim =nsim) %>% na.omit() %>% ES()

		for(i in 1:max_A){
			A <- init_A*i
			factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
			K <- es*factor

			ret <- alt_mort_fasto(K = K, nsim = nsim, A = A) %>% na.omit() %>% compute_return()
				all_rets[i] <- ret
				pi_b[i] <- equiv_pi(ret = ret, m=m, mortality = with_mortality)
			cat(paste0("...",i/max_A*100, "% ... \n"))
		}

		m_pi <- as_tibble(data.frame(pi_b))
		m_mort <- TRUE
		m_A <- (1:max_A)/10
		m_ret <- all_rets
		m_inpi <- pi

		df_m <- cbind(pi = m_pi, mort = m_mort, A = m_A, ret = m_ret, in_pi = m_inpi)

		df_total <- df_total %>% rbind(df_m)
}

# Comparing Pi
df_total %>%
	filter(mort == TRUE) %>%
	ggplot(aes(colour = as.factor(in_pi))) +
	geom_line(aes(y = pi_b, x = A), size=1) +
	geom_point(aes(y = pi_b, x = A), size = 1.25) +
	geom_line(aes(y = in_pi, x = A), size = 0.85, linetype = "dashed")+
	# facet_grid(.~ mort) +
	theme_minimal() +
	scale_colour_viridis(discrete=TRUE, end =1, begin = 0, option = "D") +
	# scale_colour_brewer(palette = "Set1") +
	xlab("A") +
	ylab(expression(pi)) +
	labs(colour = "Actual Pi of the \nBenchmark")

