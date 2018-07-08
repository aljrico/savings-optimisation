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
pi <- c(NA)
model <- c(NA)
es <- c(NA)
evi <- c(NA)
var <- c(NA)
A_record <- c(NA)

df <- data.table(thresh, pvalue, pi, model, es, evi, var, A_record)
final_data <- data.table()

pi_start <- 0.1
as <- seq(from = 0.5, to =2, by = 0.1)
for(r in 1:1){
	for(i in 1:10){
		for(j in as){
			pi <- pi_start*i
			all_data <- generate_all_data(alpha = alpha,
																	sigma = sigma,
																	a = a,
																	years = years,
																	nsim = nsim,
																	pi = pi,
																	K = K,
																	A = j,
																	include.mortality = TRUE)

			for(k in c("cppi-simple","alt-simple", "cppi-mort", "alt-mort")){
				# if(!(k %in% c("cppi-mort", "alt-mort") & pi < 0.053)) {
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
					y  <- x[x>(-u)]-u

					if(length(y) < 25){
						threshold <- median(data$X_T)
						u <- threshold

						# Vector of Losses
						x <- -data$X_T

						# Tail Definition
						y  <- x[x>(-u)]-u
					}
					if(length(y) < 25){
						y <- -data$X_T
					}

					if(length(y) < 25) next


					# Auto threshold
					auto.thresh <- thrselect(y)

					df$es <- ES(data$X_T)
					df$var <- VaR(data$X_T)
					df$thresh <- auto.thresh$solution[["threshold"]]
					df$pvalue <- auto.thresh$solution[["pvalue"]]
					df$evi <- auto.thresh$solution[["evi"]]
					df$model <- k
					df$pi <- pi
					df$A_record <- j

					final_data <- rbind(final_data,df) %>% na.omit()
				# }
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


# EVi | Pi
final_data %>%
	filter(pvalue >0.05) %>%
	as_tibble() %>%
	ggplot() +
	geom_jitter(aes(y = evi, x = pi, colour = model)) +
	theme_bw() +
	xlab("Pi") +
	ylab("EVI")

# EVI | ES
final_data %>%
	filter(pvalue >0.05) %>%
	as_tibble() %>%
	ggplot() +
	geom_jitter(aes(y = evi, x = es, colour = model)) +
	theme_bw() +
	xlab("Expected Shortfall") +
	ylab("EVI")

# EVI | VaR
final_data %>%
	filter(pvalue >0.05) %>%
	as_tibble() %>%
	ggplot() +
	geom_jitter(aes(y = evi, x = var, colour = model)) +
	theme_minimal() +
	xlab("VaR") +
	ylab("EVI")

#  ES | Pi
final_data %>%
	filter(pvalue >0.05) %>%
	as_tibble() %>%
	ggplot() +
	geom_jitter(aes(y = es, x = pi, colour = model)) +
	theme_minimal() +
	xlab("Pi") +
	ylab("ES")

#  Var | Pi
final_data %>%
	filter(pvalue >0.05) %>%
	as_tibble() %>%
	ggplot() +
	geom_jitter(aes(y = var, x = pi, colour = model)) +
	theme_minimal() +
	xlab("Pi") +
	ylab("Var")

# Thresh -  Var | Pi
final_data %>%
	filter(pvalue >0.05) %>%
	as_tibble() %>%
	ggplot() +
	geom_jitter(aes(y = (thresh-var), x = pi, colour = model)) +
	theme_minimal() +
	xlab("Pi") +
	ylab("Threshold - VaR")


# ES | ES
cppi_es <- final_data %>%
	filter(model == "cppi-simple") %>%
	dplyr::select(es) %>%
	c()

alt_es <- final_data %>%
	filter(model == "alt-simple") %>%
	dplyr::select(es) %>%
	c()

cbind(cppi = -cppi_es$es, alt= -alt_es$es) %>%
	as_tibble() %>%
	ggplot() +
	geom_jitter(aes(x = cppi, y = alt)) +
	theme_minimal() +
	xlab("ES (CPPI)") +
	ylab("ES (Alternative")


# ES | ES
cppi_es <- final_data %>%
	filter(model == "cppi-mort") %>%
	dplyr::select(es) %>%
	c()

alt_es <- final_data %>%
	filter(model == "alt-mort") %>%
	dplyr::select(es) %>%
	c()

cbind(cppi = -cppi_es$es, alt= -alt_es$es) %>%
	as_tibble() %>%
	ggplot() +
	geom_jitter(aes(x = cppi, y = alt)) +
	theme_minimal() +
	xlab("ES (CPPI)") +
	ylab("ES (Alternative")


