
# Libraries & Functions ---------------------------------------------------

source("functions.R")
source("estimate_equiv-pi.R")
library(data.table)
library(tidyverse)
library(viridis)


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
A <- 2
init_A <- 0.1
pi <- 0.5
nsim <- 1e3
theta <- 0.95
years <- 60

pi_b <- c()



# Without Mortality -------------------------------------------------------

es <- cppi(pi = pi, nsim =nsim)[[1]]

for(i in 1:20){
	A <- init_A*i
	factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
	K <- es*factor

	ret <- montses(K = K, nsim = nsim, A = A)[[2]]
	pi_b[i] <- equiv_pi(ret = ret, m=1e3)
}

df_pi <- as_tibble(data.frame(pi_b))
df_pi$mort <- FALSE
df_pi$A <- (1:20)/10

# With Mortality ----------------------------------------------------------

pi_b <- c()
es <- cppi_mortality(pi = pi, nsim =nsim)[[1]]

for(i in 1:20){
	A <- init_A*i
	factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
	K <- es*factor

	ret <- alt_mort(K = K, nsim = nsim, A = A)[[2]]
	pi_b[i] <- equiv_pi(ret = ret, m=1e2)
}

df <- as_tibble(data.frame(pi_b))
df$mort <- TRUE
df$A <- (1:20)/10

df_pi <- rbind(df_pi, df)


# Plots -------------------------------------------------------------------


df_pi %>%
	ggplot(aes(y = pi_b, x = A)) +
	geom_line(aes(colour = mort), size=1.3) +
	geom_point(aes(colour = mort), size = 2) +
	theme_minimal()

