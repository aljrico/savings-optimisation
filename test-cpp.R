
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(data.table)
library(viridis)
library(MASS)
library(rPython)
source("functions.R")
library(Rcpp)

sourceCpp("cppi.cpp")
python.load("loop.py")

cppi2 <- function(pi,
									nsim,
									alpha = 0.0343,
									sigma = 0.01544,
									a = 10,
									years = 60){

	c <- a # Still factor 'a'
	C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))

	X_T <- c()

	for (j in 1:nsim){
		x <- a # Initial wealth
		for (i in 1:(years-1)){
			random <- rnorm(1, mean = alpha, sd = sigma)
			x <- x*(1+random)*pi + (1-pi)*x + C[i+1]
		}
		X_T[j] <- x

	}
	return(X_T)
}

alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
nsim <- 2000 # Number of simulations
pi <- 0.1 # Constant proportion for risky investment
K <- 42
A <- 0.5


r <- c()
c <- c()
python <- c()
nsim2 <- nsim

for(i in 1:nsim2){
	nsim <- i
	r[i] <- system.time({cppi2(alpha = alpha, sigma = sigma, a = a, years = years, nsim = nsim, pi = pi)})
	c[i] <- system.time({cppi_c(alpha = alpha, sigma = sigma, a = a, years = years, nsim = nsim, pi = pi)})
	python[i] <- system.time({python.call("cppi_adv", alpha = alpha, sigma = sigma, a = a, years = years, nsim = nsim, pi = pi, K = K)})
}

meas <- cbind(c,r,python)
meas %>% melt() %>%
	ggplot(aes(x = Var1, y = value, colour = Var2)) +
	geom_jitter(size = 1) +
	xlab("Dimension") +
	ylab("Time") +
	theme_minimal()



c <- alt_c(alpha = alpha, sigma = sigma, a = a, years = years, nsim = nsim, K = K, A_factor = A)
r <- montses(alpha = alpha, sigma = sigma, a = a, years = years, nsim = nsim, K = K, A = A)[-c(1,2)]

c <- cppi_c(alpha = alpha, sigma = sigma, a = a, years = years, nsim = nsim, pi = pi)
r <- cppi(alpha = alpha, sigma = sigma, a = a, years = years, nsim = nsim, pi = pi)[-c(1,2)]


test <- cbind(c,r)
test %>% melt() %>%
	ggplot(aes(x = value)) +
	geom_histogram(aes(fill = Var2), position = "identity", alpha =0.5)
