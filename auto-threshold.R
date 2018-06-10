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
nsim <- 1e4 # Number of simulations
pi <- 0.1 # Constant proportion for risky investment
K <- 42
A <- 0.5

thresh <- c(NA)
pvalue <- c(NA)
pi_record <- c(NA)
model <- c(NA)
es <- c()
final_data <- tibble(thresh,pvalue,pi_record, model)

for(k in c("cppi-simple","alt-simple")){
	pi_start <- 0.1
	for(i in 1:10){
		pi <- pi_start*i
		pi_record[i] <- pi
		all_data <- generate_all_data(alpha = alpha,
															sigma = sigma,
															a = a,
															years = years,
															nsim = nsim,
															pi = pi,
															K = K,
															A = A,
															include.mortality = FALSE)
		# GPD ---------------------------------------------------------------------

		data <- all_data %>%
			filter(model == k)

		es[i] <- ES(data$X_T)
		# We first try to guess some threshold in order to define the tail.
		threshold <- 0
		u <- threshold

		# Vector of Losses
		x <- -data$X_T

		# Tail Definition
		y  <- x[x>u]-u


		# Auto threshold
		auto.thresh <- thrselect(y, evi = 0)
		thresh[i] <- auto.thresh$solution[["threshold"]]
		pvalue[i] <- auto.thresh$solution[["pvalue"]]
	}
	df <- cbind(thresh, pvalue, pi_record) %>% as_tibble()
	df$model <- k
	final_data <- rbind(final_data,df)
}

final_data %>%
	filter(pvalue >0.05) %>%
	as_tibble() %>%
	ggplot() +
	geom_point(aes(y = thresh, x = pi_record, colour = model)) +
	theme_minimal() +
	xlab("Pi") +
	ylab("Threshold")

