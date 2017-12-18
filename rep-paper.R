# Replicating script ------------------------------------------------------
# Author: Alejandro Jiménez Rico <aljrico@gmail.com>



# Libraries ---------------------------------------------------------------

library(ggplot2)
library(dplyr)


# Parameters --------------------------------------------------------------

pi <- 0.8
alpha <- 0.0343
sigma <- 0.1544
a <- 10
win <- runif(1)
x <- c()
x[1] <- 10
years <- 60
A <- 0.5
K <- 1
C <- append(rep(a, round(years/2)),rep(-a, round(years/2)))
r <- c()
nsim <- 1000
pirec <- c()
c <- 10 # Need to know exactly



# Functions ---------------------------------------------------------------

dX <- function(pi, x, alpha, sigma, C, win){
	dX <- pi*x*(alpha + sigma*win) + C
	return(dX)
}

fg <- function(C, time){
	dum <- C[-c(1:time)]
	sum(dum)
}

fpi <- function(A, K, X, C, time){
	g <- fg(C,time)
	pi <- A*(K + X + g)/X
	return(pi)
}

ES <- function(distr, a){
	VaR <- quantile(distr, a)
	ES <- mean(distr[distr<VaR])

	return(ES)
}


# Simulation --------------------------------------------------------------
i <- 0
j <- 0
ylim <- round(years/2)
for(k in 1:nsim){
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

	for(j in lim:years){
		time <- i + j
		cap <- C[j]
		X <- x[j]
		win <- rnorm(1)
		pi <- fpi(A,K,X,C,time)
		pirec[j] <- pi
		x[j+1] <- x[j] + dX(pi,X,alpha,sigma,cap,win)
	}
	r[k] <- tail(x, n=1)
}



# Measurements ------------------------------------------------------------

ret2 <- (1/years)*(-1 + (1 + (2*(r))/(c*years))^(1/2))*100
pi_b <- alpha^(-1)*log(1+median(ret2))

par(mfrow=c(1,2))
hist(r, xlab= "X(T)")
hist(ret2, xlab="Annual Return")

ES(r, 0.05)
median(ret2)
pi_b

