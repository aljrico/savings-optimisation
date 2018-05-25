
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
pi <- 0.4
nsim <- 1e5
theta <- 0.95
years <- 60
m <- 1e2
max_A <- 25
a <- 10
pi_b <- c()
all_rets <- c()


error_count <- 0
# Without Mortality -------------------------------------------------------

es <- cppi_c(alpha = alpha, sigma = sigma, years = years, pi = pi, a=a, nsim = nsim) %>%
	ES()

for(i in 1:max_A){
	A <- init_A*i
	factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
	K <- es*factor

	ret <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
		compute_return(c = a, years = years)

	# In order to avoid random errors, just repeat it one more time.
	if(is.na(ret)){	ret <- alt_c(alpha = alpha, sigma = sigma, years = years, a = a, K = K, nsim = nsim, A = A) %>%
		compute_return(c = a, years = years); error_count <- error_count + 1}


	cat(paste0("...",ret, "... \n"))
	pi_b[i] <- equiv_pi(ret = ret, m=m)
	all_rets[i] <- ret
}

df_pi <- as_tibble(data.frame(pi_b))
df_pi$mort <- FALSE
df_pi$A <- (1:i)/10
df_pi$ret <- all_rets

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


}

df <- as_tibble(data.frame(pi_b))
df$mort <- TRUE
df$A <- (1:max_A)/10
df$ret <- all_rets


df_pi <- rbind(df_pi, df)


# Plots -------------------------------------------------------------------


df_pi %>%
	ggplot(aes(colour = mort)) +
	geom_line(aes(y = pi_b, x = A), size=1.3) +
	geom_point(aes(y = pi_b, x = A), size = 2) +
	# geom_line(aes(y = ret, x = A), size = 1)+
	theme_minimal() +
	scale_colour_viridis(discrete=TRUE, end =0.75) +
	xlab("A") +
	ylab(expression(pi)) +
	labs(colour = "Mortality")


