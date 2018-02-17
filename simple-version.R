# Replicating script ------------------------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>


# Parameters --------------------------------------------------------------

alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
nsim <- 100000 # Number of simulations
c <- a # Still factor 'a'

# Array that defines the actual wealth of the investor at every time step
x <- c()
x[1] <- a # Initial wealth

# Array that contains the inputs and outputs of cash within each investor's account
C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))

# Array that stores the final wealth of each investor
X_T <- c()


# Functions ---------------------------------------------------------------

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
		random <- rnorm(1, mean = alpha, sd = sigma)
		x[i+1] <- x[i]*(1+random)*pi + (1-pi)*x[i] + C[i]
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

