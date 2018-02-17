# Replicating script ------------------------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>



# Libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)


# Parameters --------------------------------------------------------------

alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 1 # Factor 'a'
years <- 60 # Total time
A <- 0.5 # Factor 'A'
K <- 668 # Factor 'K'
nsim <- 10000 # Number of simulations
gamma <- -alpha/(A*sigma^2)+1 # Factor 'gamma'
c <- a # Factor 'c'

# Array that defines the actual wealth of the investor at every time step
x <- c()
x[1] <- a # Initial wealth

# Array that contains the inputs and outputs of cash within each investor's account
C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))

# Array that stores the final wealth of each investor
X_T <- c()

# Array that stores the historic 'pi' factor of an investor
pirec <- c()



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

# Stock Simulation --------------------------------------------------------
pi <- 0.1


for (j in 1:nsim){
	for (i in 1:years){
		time <- i
		X <- x[i]
		xpi <- fpi(A,K,X,C,time)
		#xpi <- pi*X
		pirec[i] <- xpi/X
		random <- rnorm(1, mean = alpha, sd = sigma)
		x[i+1] <- xpi*(1+random)+ (1-pirec[i])*x[i] + C[i]
	}
	X_T[j] <- x[years+1]
}



# Measurements ------------------------------------------------------------

# Final return of every individual
x_m <- median(X_T)
ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100

# Pi value of the benchmark
pi_b <- log(1+median(ret2))/alpha

# Histogram simple visualisation
hist(X_T, xlab= "X(T)")

# Output of final results
ES(X_T, 0.05)
ret2
pi_b

