
# Libraries and Functions -------------------------------------------------

source("functions.R")
library(tidyverse)
library(data.table)
library(viridis)



# Initial Parameters ------------------------------------------------------


alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
nsim <- 1e6 # Number of simulations
pi <- 0.1 # Constant proportion for risky investment
K <- 42

# CPPI simple --------------------------------------------------------------------
X_T <- cppi(pi = pi,
						nsim = nsim,
						alpha = alpha,
						sigma = sigma,
						a = a,
						years = years)[-c(1,2)]
final_wealth <- as_tibble(as.data.frame(X_T))
final_wealth$model <- "cppi-simple"
final_wealth$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100


# Alternative simple ------------------------------------------------------
X_T <- montses(K = K,
						nsim = nsim,
						alpha = alpha,
						sigma = sigma,
						a = a,
						years = years)[-c(1,2)]
df <- as_tibble(as.data.frame(X_T))
df$model <- "alt-simple"
df$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100
final_wealth <- rbind(final_wealth,df)


# CPPI | Mortality --------------------------------------------------------
X_T <- cppi_mortality(pi = pi,
						nsim = nsim,
						alpha = alpha,
						sigma = sigma,
						a = a,
						years = years)[-c(1,2)]
df <- as_tibble(as.data.frame(X_T))
df$model <- "cppi-mort"
df$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100
final_wealth <- rbind(final_wealth,df)

# Alternative simple ------------------------------------------------------
X_T <- alt_mort(K = K,
							 nsim = nsim,
							 alpha = alpha,
							 sigma = sigma,
							 a = a,
							 years = years)[-c(1,2)]
df <- as_tibble(as.data.frame(X_T))
df$model <- "alt-mort"
df$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100
final_wealth <- rbind(final_wealth,df)


# Histogram
final_wealth %>%
ggplot() +
	geom_histogram(aes(x = ret, fill = model, y = ..count../sum(..count..)),
								 bins=200,
								 alpha = 0.75,
								 position = "identity") +
	theme_minimal() +
	scale_fill_viridis(discrete=TRUE)


