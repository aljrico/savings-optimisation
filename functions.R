
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
ES <- function(distr, a = 0.05){
	VaR <- quantile(distr, a)
	ES <- mean(distr[distr<VaR])

	return(ES)
}

# Compute Return
compute_return <- function(vec, c = 10, years = 60){
	x_m <- median(vec)
	ret <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100
	return(ret)
}




# CPPI --------------------------------------------------------------------

cppi <- function(pi,
								 nsim,
								 alpha = 0.0343,
								 sigma = 0.1544,
								 a = 10,
								 years = 60){

	c <- a # Still factor 'a'
	C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))

	X_T <- c()

	for (j in 1:nsim){
		x <- c()
		x[1] <- a # Initial wealth

		for (i in 1:(years-1)){
			random <- rnorm(1, mean = alpha, sd = sigma)
			x[i+1] <- x[i]*(1+random)*pi + (1-pi)*x[i] + C[i+1]
		}
		X_T[j] <- x[years]

	}

	# Final return of every individual
	x_m <- median(X_T)
	ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100

	return(c(ES(X_T, 0.05), ret2, X_T))
}


# CPPI | Mortality --------------------------------------------------------

cppi_mortality <- function(pi,
								 nsim,
								 alpha = 0.0343,
								 sigma = 0.1544,
								 a = 10,
								 years = 60,
								 starting_humans = 1e3,
								 starting_age = 30){

	c <- a # Still factor 'a'
	C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))
	mort_table <- fread("mortality.csv")/1000
	X_T <- c()

	for (j in 1:nsim){
		x <- c()
		x[1] <- a # Initial wealth
		number_humans_alive <- starting_humans

		for (i in 1:(years-1)){
			prob_mort <- mort_table$total[i+starting_age-1]
			number_deads <- rbinom(1,number_humans_alive,prob_mort)
			number_humans_alive <- number_humans_alive - number_deads


			random <- rnorm(1, mean = alpha, sd = sigma)
			x[i+1] <- x[i]*(1+random)*pi + (1-pi)*x[i] + C[i+1] + (x[i]*number_deads/number_humans_alive)
		}
		X_T[j] <- x[years]

	}

	# Final return of every individual
	x_m <- median(X_T)
	ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100

	return(c(ES(X_T, 0.05), ret2, X_T))
}


# Montse's ----------------------------------------------------------------

montses <- function(K,
										nsim,
										alpha = 0.0343,
										sigma = 0.1544,
										a = 10,
										years = 60,
										A = 0.5){


	gamma <- -alpha/(A*sigma^2)+1 # Factor 'gamma'
	c <- a # Factor 'c'

	x <- c()
	x[1] <- a # Initial wealth

	C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))

	X_T <- c()


	for (j in 1:nsim){
		x <- c()
		x[1] <- a # Initial wealth

		for (i in 1:(years-1)){
			time <- i
			X <- x[i]
			xpi <- fpi(A,K,X,C,time)
			pi <- xpi/X
			random <- rnorm(1, mean = alpha, sd = sigma)
			x[i+1] <- xpi*(1+random)+ (1-pi)*x[i] + C[i+1]
		}
		X_T[j] <- x[years]
	}

	# Final return of every individual
	x_m <- median(X_T)
	ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100
	pi_b <- log(1+ret2)/alpha

return(c(pi_b,ret2, X_T))
}


# Alt | Mortality ---------------------------------------------------------

alt_mort <- function(K,
										nsim,
										alpha = 0.0343,
										sigma = 0.1544,
										a = 10,
										years = 60,
										A = 0.5,
										starting_humans = 1e3,
										starting_age = 30){


	gamma <- -alpha/(A*sigma^2)+1 # Factor 'gamma'
	c <- a # Factor 'c'

	x <- c()
	x[1] <- a # Initial wealth

	C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))
	X_T <- c()

	mort_table <- fread("mortality.csv")/1000

	for (j in 1:nsim){
		x <- c()
		x[1] <- a # Initial wealth
		number_humans_alive <- starting_humans

		for (i in 1:(years-1)){
			prob_mort <- mort_table$total[i+starting_age-1]
			number_deads <- rbinom(1,number_humans_alive,prob_mort)
			number_humans_alive <- number_humans_alive - number_deads


			time <- i
			X <- x[i]
			xpi <- fpi(A,K,X,C,time)
			pi <- xpi/X
			random <- rnorm(1, mean = alpha, sd = sigma)
			x[i+1] <- xpi*(1+random)+ (1-pi)*x[i] + C[i+1] + (x[i]*number_deads/number_humans_alive)
		}
		X_T[j] <- x[years]
	}

	# Final return of every individual
	x_m <- median(X_T)
	ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100
	pi_b <- log(1+ret2)/alpha

	return(c(pi_b,ret2, X_T))
}




# Graveyard ---------------------------------------------------------------

montses_mortality_graveyard <- function(
	K,
	alpha = 0.0343, # Expected return of the risky market
	sigma = 0.1544, # Expected volatility of the risky market
	a = 10, # Factor 'a'
	years = 60, # Total time
	nsim = 100000, # Number of simulations
	c = a, # Still factor 'a'
	A = 0.5, # Factor 'A'
	number_humans_alive = 1000,
	starting_age = 30
){
	C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))
	returns <- c(years)
	X_T <- c(nsim)
	mort_table <- fread("mortality.csv")/1000

	x <- c()
	x[1] <- a # Initial wealth
	starting_humans_alive <- number_humans_alive
	for(j in 1:nsim){
		number_humans_alive <- starting_humans_alive
		for(i in 1:(years-1)){
			return <- rnorm(1, mean = alpha, sd = sigma)

			time <- i
			X <- x[i]
			pi <- fpi(A,K,X,C,time)/X

			prob_mort <- mort_table$total[i+starting_age]
			number_deads <- rbinom(1,number_humans_alive,prob_mort)
			number_humans_alive <- number_humans_alive - number_deads

			x[i+1] <- x[i]*(1+return)*pi + (1-pi)*x[i] + C[i+1] + (x[i]*number_deads/number_humans_alive)
		}
		X_T[j] <- x[years]
	}

	# Final return of every individual
	x_m <- median(X_T)
	ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100
	pi_b <- log(1+median(ret2))/alpha

	return(c(pi_b, ret2, number_humans_alive))
}


cppi_mortality_graveyard <- function(
	pi,
	alpha = 0.0343, # Expected return of the risky market
	sigma = 0.1544, # Expected volatility of the risky market
	a = 10, # Factor 'a'
	years = 60, # Total time
	nsim = 100000, # Number of simulations
	c = a, # Still factor 'a'
	A = 0.5, # Factor 'A'
	starting_humans = 1000,
	starting_age = 30
){
	C <- append(rep(a, round(years/2)),rep(-2*a, round(years/2)))
	returns <- c(years)
	X_T <- c(nsim)
	mort_table <- fread("mortality.csv")/1000

	x <- c()
	x[1] <- a # Initial wealth

	for(j in 1:nsim){
		number_humans_alive <- starting_humans
		for(i in 1:(years-1)){

			prob_mort <- mort_table$total[i+starting_age]
			number_deads <- rbinom(1,number_humans_alive,prob_mort)
			number_humans_alive <- number_humans_alive - number_deads

			random <- rnorm(1, mean = alpha, sd = sigma)
			x[i+1] <- x[i]*(1+random)*pi + (1-pi)*x[i] + C[i+1]
			#x[i+1] <- x[i]*(1+return)*pi + (1-pi)*x[i] + C[i+1] #+ (x[i]*number_deads/number_humans_alive)
		}
		X_T[j] <- x[years]
	}

	# Final return of every individual
	x_m <- median(X_T)
	ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100

	return(c(ES(X_T, 0.05), ret2, X_T))
}



# Factor K/ES -------------------------------------------------------------
factor_kes <- function(alpha = 0.0343,
											 sigma = 0.1544,
											 years = 60,
											 theta = 0.95,
											 A){
	factor <- 1/(-1 + (1/(1 - theta))*exp(alpha*A*years)*pnorm(qnorm(1-theta)- A*sigma*sqrt(years)))
}

# Generate All Data -------------------------------------------------------

generate_all_data <- function(
	alpha = 0.343,
	sigma = 0.1544,
	a = 10,
	years = 60,
	nsim = 1e3,
	pi = 0.1,
	K = 42,
	A = 0.5,
	include.mortality = FALSE
){
	library(tidyverse)
	library(data.table)
	library(viridis)
	library(MASS)
	library(evir)

	library(Rcpp)
	sourceCpp("cppi.cpp")


	# CPPI simple --------------------------------------------------------------------
	X_T <- cppi_c(pi = pi,
								nsim = nsim,
								alpha = alpha,
								sigma = sigma,
								a = a,
								years = years)

	final_wealth <- as_tibble(as.data.frame(X_T))
	final_wealth$model <- "cppi-simple"
	final_wealth$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100

	K = ES(X_T)*factor_kes(A =A)


	# Alternative simple ------------------------------------------------------
	X_T <- alt_c(K = K,
							 nsim = nsim,
							 alpha = alpha,
							 sigma = sigma,
							 a = a,
							 years = years,
							 A_factor = A)

	df <- as_tibble(as.data.frame(X_T))
	df$model <- "alt-simple"
	df$ret <- (1/60)*(-1 + (1 + (8*(X_T))/(a*60))^(1/2))*100
	final_wealth <- rbind(final_wealth,df)

if(include.mortality == TRUE){
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

		K = ES(X_T)*factor_kes(A =A)

		# Alternative | Mortality ------------------------------------------------------
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



	}

	return(final_wealth)
}
