
# Libraries & Functions ---------------------------------------------------

source("functions.R")
source("estimate_equiv-pi.R")



alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
A <- 2
init_A <- 0.1
nsim <- 3e5
theta <- 0.95
years <- 60
m <- 1e2
max_A <- 20
a <- 10
with_mortality <- FALSE

pi_b <- c()
all_rets <- c()

pis <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

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
pb <- progress_bar$new(total = length(pis)*max_A)
for(pi in pis){
	es <- cppi_c(alpha = alpha, sigma = sigma, years = years, pi = pi, a=a, nsim = nsim) %>%
		ES()

	# Without Mortality -------------------------------------------------------
	for(i in 1:max_A){
		pb$tick()
		A <- init_A*i
		factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
		K <- es*factor

		ret <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
			compute_return(c = a, years = years)

		# In order to avoid random errors, just repeat it one more time.
		if(is.na(ret)){	ret <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
			compute_return(c = a, years = years); error_count <- error_count + 1}
		pi_b[i] <- equiv_pi(ret = ret, m=m)
		all_rets[i] <- ret
	}

	wm_pi <- as_tibble(data.frame(pi_b))
	wm_mort <- FALSE
	wm_A <- (1:i)/10
	wm_ret <- all_rets
	wm_inpi <- pi

	df_wm <- cbind(pi = wm_pi, mort = wm_mort, A = wm_A, ret = wm_ret, in_pi = wm_inpi)

	df_total <- df_wm
}


# Plots -------------------------------------------------------------------



# Comparing Pi
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


