library(tidyverse)
library(data.table)
library(viridis)
library(MASS)
# library(evir)
# library(ercv)
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
pi <- c(NA)
model <- c(NA)
es <- c(NA)
evi <- c(NA)
var <- c(NA)

df <- data.table(thresh, pvalue, pi, model, es, evi, var)
final_data <- data.table()

pi_start <- 0.01

for(r in 1:3){
	for(i in 1:100){
		pi <- pi_start*i
		all_data <- generate_all_data(alpha = alpha,
																	sigma = sigma,
																	a = a,
																	years = years,
																	nsim = nsim,
																	pi = pi,
																	K = K,
																	A = A,
																	include.mortality = TRUE)

		for(k in c("cppi-simple","alt-simple", "cppi-mort", "alt-mort")){
			if(!(k %in% c("cppi-mort", "alt-mort") & pi < 0.53)) {
				# GPD ---------------------------------------------------------------------
				data <- all_data %>%
					filter(model == k) %>%
					na.omit()

				# We first try to guess some threshold in order to define the tail.
				threshold <- 0
				u <- threshold

				# Vector of Losses
				x <- -data$X_T

				# Tail Definition
				y  <- x[x>u]-u


				# Auto threshold
				auto.thresh <- thrselect(y, evi = 0)

				df$es <- ES(data$X_T)
				df$var <- VaR(data$X_T)
				df$thresh <- auto.thresh$solution[["threshold"]]
				df$pvalue <- auto.thresh$solution[["pvalue"]]
				df$evi <- auto.thresh$solution[["evi"]]
				df$model <- k
				df$pi <- pi

				final_data <- rbind(final_data,df)
			}
		}
	}
}

# Threshold | Pi
final_data %>%
	filter(pvalue >0.05) %>%
	as_tibble() %>%
	ggplot() +
	geom_jitter(aes(y = thresh, x = pi, colour = model)) +
	theme_minimal() +
	xlab("Pi") +
	ylab("Threshold")

