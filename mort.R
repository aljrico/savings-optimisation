
# Mortality ---------------------------------------------------------------



# Functions ---------------------------------------------------------------

# Computation of 'pi' value
fpi <- function(A, K, X, C, time){
	g <- sum(C[-c(1:time)])
	xpi <- A*(K + X + g)/X
	return(xpi)
}

# Expected Shortfall
ES <- function(distr, a){
	VaR <- quantile(distr, a)
	ES <- mean(distr[distr<VaR])

	return(ES)
}



# Libraries ---------------------------------------------------------------

library(dplyr)
library(data.table)

# Parameters --------------------------------------------------------------

alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
nsim <- 100000 # Number of simulations
c <- a # Still factor 'a'
A <- 0.5 # Factor 'A'
C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))
returns <- c(years)
number_humans_alive <- 1000
starting_age <- 30
pi <- 0.1

mort_table <- fread("mortality.csv")/1000

x <- c()
x[1] <- a # Initial wealth

for(i in 1:(years-1)){
	time <- i
	X <- x[i]
	xpi <- fpi(A,K,X,C,time)
	return <- rnorm(1, mean = alpha, sd = sigma)

	prob_mort <- mort_table$total[i+starting_age]
	number_deads <- rbinom(1,number_humans_alive,prob_mort)
	number_humans_alive <- number_humans_alive - number_deads

	x[i+1] <- x[i]*(1+return)*pi + (1-pi)*x[i] + C[i+1] + (x[i]*number_deads/number_humans_alive)
}
number_humans_alive

