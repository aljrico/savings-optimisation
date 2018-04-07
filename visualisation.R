######## VISUALISATION #########


# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(plotly)
library(MASS)



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

nsim <- 10000
pi <- 0.1

alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
# Number of simulations
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

normalfit <- fitdistr(X_T, densfun = "normal")
ks.test(X_T, "pnorm", normalfit$estimate[[1]], normalfit$estimate[[2]])

# Plots
final_wealth <- as.data.frame(X_T)
ggplot(data = final_wealth, aes(x = final_wealth$X_T)) +
	geom_histogram(bins=70, fill = "dodgerblue3") +
	theme(axis.line = element_blank(),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank(),
				panel.border = element_blank(),
				panel.background = element_blank())


cppi_final_wealth <- X_T

plot_ly(  x = X_T
				, type = "histogram"
				, histnorm = "probability"
				) %>%
	layout(xaxis = list(title = "Final Wealth"))




# Montse's ----------------------------------------------------------------

K <- 42
nsim <- 10000

alpha <- 0.0343 # Expected return of the risky market
sigma <- 0.1544 # Expected volatility of the risky market
a <- 10 # Factor 'a'
years <- 60 # Total time
A <- 0.5 # Factor 'A'

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
		x[i+1] <- xpi*(1+random)+ (1-pi)*x[i] + C[i+1]
	}
	X_T[j] <- x[years]
}

# Final return of every individual
x_m <- median(X_T)
ret2 <- (1/years)*(-1 + (1 + (8*(x_m))/(c*years))^(1/2))*100
pi_b <- log(1+ret2)/alpha

montse_final_wealth <- X_T

	plot_ly() %>%
		add_histogram(x = montse_final_wealth, histnorm = "probability", color = "red") %>%
		layout(xaxis = list(title = "Final Wealth"))

cppi_fw <- as.data.frame(cbind(cppi_final_wealth))
alt_fw <- as.data.frame(cbind(montse_final_wealth))

colnames(cppi_fw) <- "wealth"
colnames(alt_fw) <- "wealth"

cppi_fw$veg <- 'cppi'
alt_fw$veg <- 'alt'
fw <- rbind(cppi_fw, alt_fw)

ggplot(fw) +
	geom_density(aes(x=wealth, fill = fw$veg), alpha = 0.55, position="identity", show.legend = FALSE) +
	xlab("Final Wealth") +
	ylab("Density") +
	theme_bw()


# Negative Results --------------------------------------------------------


fw2 <-fw[fw$wealth <=0,]
ggplot(fw2) +
	geom_density(aes(x=fw2$wealth, fill = fw2$veg), alpha = 0.55, position="identity", show.legend = FALSE) +
	xlab("Final Wealth") +
	ylab("Density") +
	theme_bw()




# Extreme Results ---------------------------------------------------------

fwe <-fw[fw$wealth <= quantile(fw$wealth, 0.05)[[1]],]

ggplot(fwe) +
	geom_density(aes(x=fwe$wealth, fill = fwe$veg), alpha = 0.55, position="identity", show.legend = TRUE) +
	xlab("Final Wealth") +
	ylab("Density") +
	theme_bw()


ggplot(fwe) +
	geom_histogram(aes(x=fwe$wealth, fill = fwe$veg),
								 alpha = 0.75, position="identity", show.legend = TRUE, binwidth = 1) +
	xlab("Final Wealth") +
	ylab("Density") +
	theme_bw()
