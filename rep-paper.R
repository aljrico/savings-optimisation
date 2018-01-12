# Replicating script ------------------------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>



# Libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)


# Parameters --------------------------------------------------------------

alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
A <- 0.5 # Factor 'A'
K <- -3.255 # Factor 'K'
nsim <- 10000 # Number of simulations
gamma <- -alpha/(A*sigma^2)+1 # Factor 'gamma'
c <- a # Factor 'c'

# Array that defines the actual wealth of the investor at every time step
x <- c()
x[1] <- 10 # Initial wealth

# Array that contains the inputs and outputs of cash within each investor's account
C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))

# Array that stores the final wealth of each investor
X_T <- c()

# Array that stores the historic 'pi' factor of an investor
pirec <- c()




# Functions ---------------------------------------------------------------

# Differential of X
dX <- function(pi, x, alpha, sigma, C, win){
	dX <- pi*x*(alpha + sigma*win) + C
	return(dX)
}

# Computation of 'pi' value
fpi <- function(A, K, X, C, time){
	g <- sum(C[-c(1:time)])
	pi <- A*(K + X + g)/X
	return(pi)
}

# Expected Shortfall
ES <- function(distr, a){
	VaR <- quantile(distr, a)
	ES <- mean(distr[distr<VaR])

	return(ES)
}


# Simulation --------------------------------------------------------------

# Index variables
ylim <- round(years/2)
i <- 0
j <- 0

# Loop over each simulation
for(k in 1:nsim){

	# Loop over each saving year
	for(i in 1:ylim){
		time <- i + j
		cap <- C[i]
		X <- x[i]
		win <- rnorm(1)
		pi <- fpi(A,K,X,C,time)
		pirec[i] <- pi
		x[i+1] <- x[i] + dX(pi,X,alpha,sigma,cap,win)
		lim <- i +1
	}

	# Loop over each consumption year
	for(j in lim:years){
		time <- i + j
		cap <- C[j]
		X <- x[j]
		win <- rnorm(1)
		pi <- fpi(A,K,X,C,time)
		pirec[j] <- pi
		x[j+1] <- x[j] + dX(pi,X,alpha,sigma,cap,win)
	}

	# Output the final wealth of every simulated individual
	X_T[k] <- tail(x, n=1)
}



# Measurements ------------------------------------------------------------

# Final return of every individual
ret2 <- (1/years)*(-1 + (1 + (2*(X_T))/(c*years))^(1/2))*100

# Pi value of the benchmark
pi_b <- alpha^(-1)*log(1+median(ret2))

# Histogram simple visualisation
par(mfrow=c(1,2))
hist(X_T, xlab= "X(T)")
hist(ret2, xlab="Annual Return")

# Output of final results
ES(X_T, 0.05)
median(ret2)
pi_b

