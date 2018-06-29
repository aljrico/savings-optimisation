library(tidyverse)
library(data.table)
library(viridis)
library(MASS)
library(evir)
library(ercv)
library(Rcpp)

source("functions.R")
sourceCpp("cppi.cpp")


# Create Data------------------------------------------------------


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
nsim <- 1e6 # Number of simulations
pi <- 0.5 # Constant proportion for risky investment
K <- 42
A <- 0.5

all_data <- generate_all_data(alpha = alpha,
															sigma = sigma,
															a = a,
															years = years,
															nsim = nsim,
															pi = pi,
															K = K,
															A = 0.5,
															include.mortality = TRUE)

all_data %>%
	filter(model %in% c("cppi-mort", "alt-mort")) %>%
	ggplot(aes(x = X_T, fill = model)) +
	geom_density(alpha = 0.5) +
	theme_bw() +
	theme(legend.position = "none") +
	xlab("Final Wealth") +
	ylab("")

all_data %>%
	filter(model %in% c("cppi-mort", "alt-mort")) %>%
	filter(X_T <= 0) %>%
	ggplot(aes(x = X_T, fill = model)) +
	geom_density(alpha = 0.5) +
	theme_bw() +
	theme(legend.position = "none") +
	xlab("Final Wealth") +
	ylab("")

