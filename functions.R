
# Functions ---------------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)


# Functions ---------------------------------------------------------------

# Computation of 'pi' value
fpi <- function(A, K, X, C, time){
	g <- sum(C[-c(1:time)])
	xpi <- A*(K + X + g)
	return(xpi)
}

# Expected Shortfall
ES <- function(distr, a){
	VaR <- quantile(distr, a)
	ES <- mean(distr[distr<VaR])

	return(ES)
}



# CPPI --------------------------------------------------------------------

cppi <- function(pi){

	alpha <- 0.0343 # Expected return of the risky market
	sigma <- 0.1544 # Expected volatility of the risky market
	a <- 10 # Factor 'a'
	years <- 60 # Total time
	nsim <- 10000 # Number of simulations
	c <- a # Still factor 'a'

	C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))

	X_T <- c()

	for (j in 1:nsim){
		x <- c()
		x[1] <- a # Initial wealth

		for (i in 1:years){
			random <- rnorm(1, mean = alpha, sd = sigma)
			x[i+1] <- x[i]*(1+random)*pi + (1-pi)*x[i] + C[i]
		}

		X_T[j] <- x[years+1]
	}

	# Final return of every individual
	x_m <- median(X_T)
	ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100

	return(c(ES(X_T, 0.05), ret2))
}



# Montse's ----------------------------------------------------------------

montses <- function(K){

	alpha <- 0.0343 # Expected return of the risky market
	sigma <- 0.1544 # Expected volatility of the risky market
	a <- 10 # Factor 'a'
	years <- 60 # Total time
	A <- 0.5 # Factor 'A'
	nsim <- 10000 # Number of simulations
	gamma <- -alpha/(A*sigma^2)+1 # Factor 'gamma'
	c <- a # Factor 'c'

	x <- c()
	x[1] <- a # Initial wealth

	C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))

	X_T <- c()


	for (j in 1:nsim){
		x <- c()
		x[1] <- a # Initial wealth

		for (i in 1:years){
			time <- i
			X <- x[i]
			xpi <- fpi(A,K,X,C,time)
			pi <- xpi/X
			random <- rnorm(1, mean = alpha, sd = sigma)
			x[i+1] <- xpi*(1+random)+ (1-pi)*x[i] + C[i]
		}
		X_T[j] <- x[years+1]
	}

	# Final return of every individual
	x_m <- median(X_T)
	ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100
	pi_b <- log(1+median(ret2))/alpha

return(c(pi_b,ret2))
}
